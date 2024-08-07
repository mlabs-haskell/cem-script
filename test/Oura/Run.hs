{-# LANGUAGE BlockArguments #-}

module Oura.Run where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (void)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Oura qualified
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
  runContT $ runOura workdir $ Just $ Oura.MkIntervalMs 1_000

runOura :: WorkDir -> Maybe Oura.Interval -> ContT r IO (Oura IO)
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
  ouraConnection <- lift $ Oura.connectToDaemon writerPath sourcePath
  ouraOutput <- lift $ Oura.listenOuraSink sinkPath outputCheckingInterval
  let
    shutDown = do
      Oura.close ouraConnection
      killThread ouraThread
      Process.terminateProcess ouraHandle
    receive = Oura.waitForOutput ouraOutput
    send = void . Oura.sendToOura ouraConnection
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
