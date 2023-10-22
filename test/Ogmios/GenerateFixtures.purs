module Test.Ctl.Ogmios.GenerateFixtures
  ( main
  ) where

import Prelude

import Aeson (class DecodeAeson, Aeson, stringifyAeson)
import Control.Parallel (parTraverse)
import Ctl.Internal.Hashing (md5HashHex)
import Ctl.Internal.Helpers (logString)
import Ctl.Internal.JsWebSocket
  ( _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsClose
  , _wsSend
  )
import Ctl.Internal.QueryM
  ( ListenerSet
  , WebSocket(WebSocket)
  , WebsocketDispatch
  , defaultMessageListener
  , defaultOgmiosWsConfig
  , mkListenerSet
  , mkRequestAff
  , mkWebsocketDispatch
  )
import Ctl.Internal.QueryM.JsonRpc2 (JsonRpc2Call)
import Ctl.Internal.QueryM.Ogmios (mkOgmiosCallTypeNoArgs)
import Ctl.Internal.ServerConfig (ServerConfig, mkWsUrl)
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Trace, Debug))
import Data.Map as Map
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Data.Traversable (for_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat)

-- A simple websocket for testing
mkWebSocket
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> (Either Error (WebSocket (ListenerSet a b)) -> Effect Unit)
  -> Effect (Error -> Effect Unit)
mkWebSocket lvl serverCfg cb = do
  dispatcher <- Ref.new Map.empty
  pendingRequests <- Ref.new Map.empty
  let
    (messageDispatch :: WebsocketDispatch) =
      mkWebsocketDispatch dispatcher
  ws <- _mkWebSocket (logger Debug) $ mkWsUrl serverCfg
  let
    sendRequest :: String -> Effect Unit
    sendRequest = _wsSend ws (logString lvl Debug)
    onError = do
      logString lvl Debug "WS error occured, resending requests"
      Ref.read pendingRequests >>= traverse_ sendRequest
  _onWsConnect ws do
    void $ _onWsError ws \_ -> onError
    _onWsMessage ws (logger Debug) $ defaultMessageListener (\_ _ -> pure unit)
      [ messageDispatch ]
    void $ _onWsError ws $ const onError
    cb $ Right $ WebSocket ws
      (mkListenerSet dispatcher pendingRequests)
  pure $ \err -> cb $ Left $ err
  where
  logger :: LogLevel -> String -> Effect Unit
  logger = logString lvl

mkWebSocketAff
  :: forall (a :: Type) (b :: Type)
   . DecodeAeson b
  => Show b
  => LogLevel
  -> ServerConfig
  -> Aff (WebSocket (ListenerSet a b))
mkWebSocketAff lvl = makeAff <<< map (map (Canceler <<< map liftEffect)) <<<
  mkWebSocket lvl

data Query = Query (JsonRpc2Call Unit Aeson) String

mkQuery' :: String -> Query
mkQuery' method = Query (mkOgmiosCallTypeNoArgs method) (sanitiseMethod method)

-- | To avoid creating directories, replace slashes with dashes
sanitiseMethod :: String -> String
sanitiseMethod = replace (Pattern "/") (Replacement "-")

main :: Effect Unit
main =
  launchAff_ do
    let logLevel = Trace
    WebSocket ws listeners <- mkWebSocketAff logLevel defaultOgmiosWsConfig

    let
      queries =
        [ mkQuery' "queryLedgerState/protocolParameters"
        , mkQuery' "queryLedgerState/eraSummaries"
        , mkQuery' "queryLedgerState/epoch"
        , mkQuery' "queryNetwork/systemStart"
        , mkQuery' "queryNetwork/tip"
        ]

    resps <- flip parTraverse queries \(Query qc method) -> do
      resp <- mkRequestAff listeners ws (\_ _ -> pure unit) qc identity unit
      pure { resp, method }

    for_ resps \{ resp, method } -> do
      let resp' = stringifyAeson resp
      respMd5 <- liftEffect $ md5HashHex resp'
      let
        fp = concat
          [ "fixtures"
          , "test"
          , "ogmios"
          , method <> "-" <> respMd5 <> ".json"
          ]
      writeTextFile UTF8 fp resp'
      log ("Written " <> fp)
    liftEffect $ _wsClose ws
