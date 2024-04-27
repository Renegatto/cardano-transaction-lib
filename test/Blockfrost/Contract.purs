-- | Module to run `Test.Ctl.Plutip.Contract`s suite without Plutip, using
-- | an already running instance of Blockfrost (preview).
-- |
-- | Use `npm run blockfrost-test` to run.
module Test.Ctl.Blockfrost.Contract (main) where

import Prelude

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
import Contract.Prelude (log, unwrap)
import Contract.ScriptLookups as Lookups
import Contract.Test
  ( ContractTest
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Blockfrost (executeContractTestsWithBlockfrost)
import Contract.Transaction
  ( awaitTxConfirmed
  , submitTxFromConstraintsReturningFee
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
  ( getWalletAddresses
  , getWalletAddressesWithNetworkTag
  , getWalletUtxos
  )
import Control.Bind (bindFlipped)
import Control.Monad.Reader (ask)
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
  ( runBlockfrostServiceM
  , utxosWithAssetAt
  )
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
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
      Plutip.suite
      IntegrationTest.stakingSuite

testUtxosWithAssetAt :: ContractTest
testUtxosWithAssetAt = do
  withWallets
    [ BigInt.fromInt 500_000_000
    , BigInt.fromInt 5_000_000
    ]
    \alice -> do
      withKeyWallet alice do
        address <-
          liftContractM "Impossible happened: no wallet addresses found"
            <<< Array.toUnfoldable
            =<< getWalletAddressesWithNetworkTag
        runBlockfrostQuery <- mkRunBlockfrostQuery
        tn <- mkTokenName "CTLNFT"
        let
          checkUtxoIsMintedAndFound = do
            cs <- mintTestAssets tn
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
                  `shouldEqual` one
              _ -> throwContractError "Minted UTxO not found"

        checkUtxoIsMintedAndFound
        checkUtxoIsMintedAndFound
  where
  mkRunBlockfrostQuery = do
    env <- ask
    blockfrost <- case env.backend of
      BlockfrostBackend backend _ -> pure backend :: Contract _
      _ -> throwContractError "Unexpected backend: expected Blockfrost"
    pure \query ->
      liftAff
        $ runBlockfrostServiceM (logWithLevel env.logLevel) blockfrost query

mintTestAssets :: TokenName -> Contract CurrencySymbol
mintTestAssets tn = do
  utxos <- liftContractM "No UTxOs at the own wallet" =<< getWalletUtxos
  { key: oref } <- liftContractM "Utxo set is empty"
    $ Map.findMin utxos
  oneShotMp <- oneShotMintingPolicy oref
  let
    cs = scriptCurrencySymbol oneShotMp

    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue (Value.singleton cs tn one)
        <> Constraints.mustSpendPubKeyOutput oref

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.mintingPolicy oneShotMp
        <> Lookups.unspentOutputs utxos
  log <<< show =<< getWalletAddresses
  log "Minting"
  { txHash } <- submitTxFromConstraintsReturningFee lookups constraints
  awaitTxConfirmed txHash
  log "Minted"
  pure cs