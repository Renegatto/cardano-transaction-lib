module TestPlutip (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (defaultPlutipConfig)
import Ctl.Internal.Plutip.Server (startPlutipCluster, startPlutipServer)
import Ctl.Internal.Plutip.Spawn (ManagedProcess(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Node.ChildProcess (stderr, stdout)
import Node.Encoding (Encoding(..))
import Node.Stream as NS

main :: Effect Unit
main = launchAff_ do  
  log "starting"
 -- ManagedProcess _ proc _ <- startPlutipServer defaultPlutipConfig -- []
  --liftEffect $ NS.onError (stderr proc) (\e -> log $ show e)
 --- liftEffect $ NS.onDataString (stdout proc) UTF8 (\e -> log e)
 -- delay $ Milliseconds 3000.0
  log "started" 
  _ /\ _ <- startPlutipCluster defaultPlutipConfig []
  log "321"