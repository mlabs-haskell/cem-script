{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Oura (
  WorkDir (MkWorkDir, unWorkDir),
  Oura (MkOura, send, receive, shutDown),
  withOura,
  runOura,
) where

import Prelude

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Trans (lift)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import System.Process qualified as Process
import Toml.Pretty qualified
import Utils qualified
import Utils (withNewFile)

import Data.Text.Encoding qualified as Text.Encoding
import Oura.Communication qualified as Communication
import Oura.Config qualified as Config
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import System.Directory (removeFile)

-- | A time required for oura to start up and create a socket,
-- in microseconds.
ouraStartupDurationNs :: Int
ouraStartupDurationNs = 1_000_000

data Oura m = MkOura
  { send :: T.Text -> m ()
  , receive :: m T.Text
  , shutDown :: m ()
  }
newtype WorkDir = MkWorkDir {unWorkDir :: T.Text}
  deriving newtype (IsString)

withOura ::
  WorkDir ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  (Oura IO -> IO r) ->
  IO r
withOura spotHandle workdir =
  runContT $ runOura spotHandle workdir $ Just $ Communication.MkIntervalMs 1_000

runOura ::
  WorkDir ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  Maybe Communication.Interval ->
  ContT r IO (Oura IO)
runOura (MkWorkDir (T.unpack -> workdir)) spotHandle outputCheckingInterval = do
  writerPath <-
    ContT $
      withNewFile "writer.socket" workdir
  sinkPath :: Config.SinkPath <-
    fmap fromString $
      ContT $
        withNewFile "sink.socket" workdir
  sourcePath :: Config.SourcePath <-
    fmap fromString $
      ContT $
        withNewFile "source.socket" workdir
  lift $ removeFile $ T.unpack $ Config.unSourcePath sourcePath
  let
    config = daemonConfig sourcePath sinkPath
  configPath <- ContT $ withNewFile "config.toml" workdir
  lift $ T.IO.writeFile configPath config
  (ouraHandle, waitingForClose) <- launchOura configPath spotHandle
  lift $ Async.link waitingForClose
  lift $ threadDelay ouraStartupDurationNs
  ouraConnection <-
    lift $
      Communication.connectToDaemon writerPath sourcePath
  ouraOutput <-
    lift $
      Communication.listenOuraSink sinkPath outputCheckingInterval
  let
    shutDown = do
      Communication.stopMonitoring ouraOutput
      Communication.close ouraConnection
      Async.cancel waitingForClose
      Process.terminateProcess ouraHandle
    receive = Communication.waitForOutput ouraOutput
    send = void
      . Communication.sendToOura ouraConnection
      . Text.Encoding.encodeUtf8
  pure MkOura {shutDown, receive, send}

daemonConfig :: Config.SourcePath -> Config.SinkPath -> T.Text
daemonConfig = fmap (T.pack . show . Toml.Pretty.prettyToml) . Config.daemonConfig

launchOura ::
  FilePath ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  ContT r IO (Process.ProcessHandle, Async ())
launchOura configPath spotHandle = do
  ouraHandle <- lift do
    ouraHandle <- Process.spawnProcess
      "oura"
      [ "daemon"
      , "--config"
      , configPath
      ]
    
    void $ spotHandle.run ouraHandle
    pure ouraHandle

  waitingForClose <- ContT $ Async.withAsync $ do
    _ <- Process.waitForProcess ouraHandle
    error "Oura process has stopped."
  pure (ouraHandle, waitingForClose)