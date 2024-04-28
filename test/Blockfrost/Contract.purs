-- | Module to run `Test.Ctl.Plutip.Contract`s suite without Plutip, using
-- | an already running instance of Blockfrost (preview).
-- |
-- | Use `npm run blockfrost-test` to run.
module Test.Ctl.Blockfrost.Contract
  ( main
  , mkRunBlockfrostQuery
  ) where

import Prelude

import Contract.Address (AddressWithNetworkTag)
import Contract.Config
  ( QueryBackend(BlockfrostBackend)
  , testnetConfig
  )
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractE
  , liftContractM
  , throwContractError
  )
import Contract.Prelude (unwrap, (/\))
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Test
  ( ContractTest
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Blockfrost (executeContractTestsWithBlockfrost)
import Contract.Transaction
  ( awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , scriptCurrencySymbol
  , valueOf
  )
import Contract.Value as Value
import Contract.Wallet
  ( getWalletAddressesWithNetworkTag
  , getWalletUtxos
  )
import Control.Bind (bindFlipped)
import Control.Monad.Reader (ask)
import Ctl.Examples.AlwaysMints
  ( alwaysMintsPolicy
  )
import Ctl.Examples.Helpers (mkTokenName)
import Ctl.Examples.OneShotMinting
  ( oneShotMintingPolicy
  )
import Ctl.Internal.Helpers (logWithLevel)
import Ctl.Internal.Plutus.Conversion
  ( fromPlutusAddressWithNetworkTag
  , toPlutusTxOutputWithRefScript
  )
import Ctl.Internal.Service.Blockfrost
  ( BlockfrostServiceM
  , assetAddresses
  , runBlockfrostServiceM
  , utxosWithAssetAt
  )
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote (test)
import Test.Ctl.Integration as IntegrationTest
import Test.Ctl.Plutip.Contract as Plutip
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig) as TestSpec

main :: Effect Unit
main = launchAff_ do
  executeContractTestsWithBlockfrost
    TestSpec.defaultConfig { timeout = Just $ Milliseconds 1000000.0 }
    testnetConfig { suppressLogs = true }
    do
      test
        "utxosWithAssetAt returns all and only UTxOs with asset at address"
        testUtxosWithAssetAt
      test
        "assetAddresses returns the only correct addresses and amounts"
        testAssetAddresses
      Plutip.suite
      IntegrationTest.stakingSuite

testAssetAddresses :: ContractTest
testAssetAddresses = do
  withWallets
    (utxos /\ utxos)
    \(alice /\ bob) -> do
      withKeyWallet alice do
        runBlockfrostQuery <- mkRunBlockfrostQuery
        let
          checkUtxoIsMintedAndFound recipient tn amount = do
            cs <- scriptCurrencySymbol <$> alwaysMintsPolicy
            let
              getAssetAddresses = bindFlipped liftContractE
                $ runBlockfrostQuery
                $ assetAddresses cs tn

              address = fromPlutusAddressWithNetworkTag recipient
            -- There may be tokens left from previous test runs
            amountsBefore <- getAssetAddresses
            -- Test
            _ <- mintTestTokens tn amount
            foundAmounts <- getAssetAddresses
            -- Assertions
            let freshExpectedAmounts = Map.fromFoldable [ address /\ amount ]
            foundAmounts `shouldEqual`
              Map.unionWith (+) freshExpectedAmounts amountsBefore

        aliceAddress <- ownAddress
        bobAddress <- withKeyWallet bob ownAddress

        aliceTn <- mkTokenName "AliceToken"
        bobTn <- mkTokenName "BobToken"

        withKeyWallet alice
          $ checkUtxoIsMintedAndFound aliceAddress aliceTn (BigInt.fromInt 7)
        withKeyWallet bob
          $ checkUtxoIsMintedAndFound bobAddress bobTn (BigInt.fromInt 2)
  where
  utxos =
    [ BigInt.fromInt 6_000_000
    , BigInt.fromInt 5_000_000
    ]

testUtxosWithAssetAt :: ContractTest
testUtxosWithAssetAt = do
  withWallets
    [ BigInt.fromInt 10_000_000
    , BigInt.fromInt 5_000_000
    ]
    \alice -> do
      withKeyWallet alice do
        address <- ownAddress
        runBlockfrostQuery <- mkRunBlockfrostQuery
        let
          checkUtxoIsMintedAndFound amount = do
            { cs, tn } <- mintTestNFT
            let cslAddress = fromPlutusAddressWithNetworkTag address
            foundUtxos <-
              bindFlipped liftContractE
                $ runBlockfrostQuery
                $ utxosWithAssetAt cslAddress cs tn
            case Array.uncons $ Array.fromFoldable foundUtxos of
              Just { head: utxo, tail: [] } -> do
                utxo' <- liftContractM "Wrong Tx output format"
                  $ toPlutusTxOutputWithRefScript utxo
                valueOf (_.amount $ unwrap $ _.output $ unwrap utxo') cs tn
                  `shouldEqual` amount
              _ -> throwContractError "Minted UTxO not found"

        checkUtxoIsMintedAndFound $ BigInt.fromInt 1
        checkUtxoIsMintedAndFound $ BigInt.fromInt 1

mkRunBlockfrostQuery :: forall a. Contract (BlockfrostServiceM a -> Contract a)
mkRunBlockfrostQuery = do
  env <- ask
  blockfrost <- case env.backend of
    BlockfrostBackend backend _ -> pure backend
    _ -> throwContractError "Unexpected backend: expected Blockfrost"
  pure \query ->
    liftAff
      $ runBlockfrostServiceM (logWithLevel env.logLevel) blockfrost query

ownAddress :: Contract AddressWithNetworkTag
ownAddress = do
  liftContractM "No wallet addresses found"
    <<< Array.toUnfoldable
    =<< getWalletAddressesWithNetworkTag

mintTestTokens :: TokenName -> BigInt -> Contract CurrencySymbol
mintTestTokens = mintTestAssets (const alwaysMintsPolicy)

mintTestNFT :: Contract { tn :: TokenName, cs :: CurrencySymbol }
mintTestNFT = do
  tn <- mkTokenName "CTLNFT"
  cs <- mintTestAssets oneShotMintingPolicy tn one
  pure { cs, tn }

mintTestAssets
  :: (TransactionInput -> Contract MintingPolicy)
  -> TokenName
  -> BigInt
  -> Contract CurrencySymbol
mintTestAssets mkMp tn amount = do
  utxos <- liftContractM "No UTxOs at the own wallet" =<< getWalletUtxos
  { key: oref } <- liftContractM "Utxo set is empty"
    $ Map.findMin utxos
  oneShotMp <- mkMp oref
  let
    cs = scriptCurrencySymbol oneShotMp

    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue (Value.singleton cs tn amount)
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.mintingPolicy oneShotMp
        <> Lookups.unspentOutputs utxos
  txHash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txHash
  pure cs