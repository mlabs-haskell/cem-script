{-# LANGUAGE BlockArguments #-}

module Oura (
  WorkDir (MkWorkDir, unWorkDir),
  Oura (MkOura, send, receive, shutDown),
  withOura,
  runOura,
) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (void)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Oura.Communication qualified as Communication
import Oura.Config qualified as Config
import System.Process qualified as Process
import Toml.Pretty qualified
import Utils (withNewFile)
import Prelude

data Oura m = MkOura
  { send :: BS.ByteString -> m ()
  , receive :: m T.Text
  , shutDown :: m ()
  }
newtype WorkDir = MkWorkDir {unWorkDir :: T.Text}
  deriving newtype (IsString)

withOura :: WorkDir -> (Oura IO -> IO r) -> IO r
withOura workdir =
  runContT $ runOura workdir $ Just $ Communication.MkIntervalMs 1_000

runOura :: WorkDir -> Maybe Communication.Interval -> ContT r IO (Oura IO)
runOura (MkWorkDir (T.unpack -> workdir)) outputCheckingInterval = do
  sourcePath :: Config.SourcePath <-
    fmap fromString $
      ContT $
        withNewFile "source.socket" workdir
  writerPath <-
    ContT $
      withNewFile "writer.socket" workdir
  sinkPath :: Config.SinkPath <-
    fmap fromString $
      ContT $
        withNewFile "sink.socket" workdir
  let
    config = daemonConfig sourcePath sinkPath
  configPath <- ContT $ withNewFile "config.toml" workdir
  lift $ T.IO.writeFile configPath config
  (ouraHandle, ouraThread) <- lift $ launchOura configPath
  ouraConnection <-
    lift $
      Communication.connectToDaemon writerPath sourcePath
  ouraOutput <-
    lift $
      Communication.listenOuraSink sinkPath outputCheckingInterval
  let
    shutDown = do
      Communication.close ouraConnection
      killThread ouraThread
      Process.terminateProcess ouraHandle
    receive = Communication.waitForOutput ouraOutput
    send = void . Communication.sendToOura ouraConnection
  pure MkOura {shutDown, receive, send}

daemonConfig :: Config.SourcePath -> Config.SinkPath -> T.Text
daemonConfig = fmap (T.pack . show . Toml.Pretty.prettyToml) . Config.daemonConfig

launchOura :: FilePath -> IO (Process.ProcessHandle, ThreadId)
launchOura configPath = do
  ouraHandle <-
    Process.spawnProcess
      "oura"
      [ "daemon"
      , "--config"
      , configPath
      ]
  threadId <- forkIO do
    _ <- Process.waitForProcess ouraHandle
    error "Oura process has stopped."
  pure (ouraHandle, threadId)
