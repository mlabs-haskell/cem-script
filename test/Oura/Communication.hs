{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Oura.Communication (
  WorkDir (MkWorkDir, unWorkDir),
  Oura (MkOura, send, receive, shutDown),
  withOura,
  runOura,
  connectToDaemon,
  sendToOura,
  close,
  listenOuraSink,
  stopMonitoring,
  waitForOutput,
  Interval (MkIntervalMs, unIntervalMs),
) where

import Prelude

import Control.Concurrent (
  Chan,
  ThreadId,
  forkIO,
  killThread,
  myThreadId,
  newChan,
  readChan,
  threadDelay,
  writeList2Chan,
 )
import Control.Monad (forever)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Traversable (for)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS

import Prelude

import Cardano.CEM.Indexing.Oura qualified as Indexing
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad (void)
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.Trans (lift)
import Data.ByteString qualified as BS
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import System.Directory (removeFile)
import System.Process qualified as Process
import Toml (Table)
import Utils (withNewFile)
import Utils qualified

import Cardano.CEM.Indexing.Oura (SinkPath, SourcePath (MkSourcePath), unSinkPath)
import Data.ByteString.Char8 qualified as BS.Char8

{- | A time required for oura to start up and create a socket,
in microseconds.
-}
ouraStartupDurationNs :: Int
ouraStartupDurationNs = 1_000_000

data Oura m = MkOura
  { send :: BS.ByteString -> m ()
  , receive :: m BS.ByteString
  , shutDown :: m ()
  }
newtype WorkDir = MkWorkDir {unWorkDir :: T.Text}
  deriving newtype (IsString)

withOura ::
  WorkDir ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  (Indexing.SourcePath -> Indexing.SinkPath -> Table) ->
  (Oura IO -> IO r) ->
  IO r
withOura spotHandle workdir makeConfig =
  runContT $ runOura spotHandle workdir makeConfig $ Just $ MkIntervalMs 1_000

runOura ::
  WorkDir ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  (Indexing.SourcePath -> Indexing.SinkPath -> Table) ->
  Maybe Interval ->
  ContT r IO (Oura IO)
runOura (MkWorkDir (T.unpack -> workdir)) spotHandle makeConfig outputCheckingInterval = do
  writerPath <-
    ContT $
      withNewFile "writer.socket" workdir
  sinkPath :: Indexing.SinkPath <-
    fmap fromString $
      ContT $
        withNewFile "sink.socket" workdir
  sourcePath :: Indexing.SourcePath <-
    fmap fromString $
      ContT $
        withNewFile "source.socket" workdir
  lift $ removeFile $ T.unpack $ Indexing.unSourcePath sourcePath
  let
    config = Indexing.configToText $ makeConfig sourcePath sinkPath
  configPath <- ContT $ withNewFile "Indexing.toml" workdir
  lift $ T.IO.writeFile configPath config
  (ouraHandle, waitingForClose) <- launchOura configPath spotHandle
  lift $ Async.link waitingForClose
  lift $ threadDelay ouraStartupDurationNs
  ouraConnection <-
    lift $
      connectToDaemon writerPath sourcePath
  ouraOutput <-
    lift $
      listenOuraSink sinkPath outputCheckingInterval
  let
    shutDown = do
      stopMonitoring ouraOutput
      close ouraConnection
      Async.cancel waitingForClose
      Process.terminateProcess ouraHandle
    receive = waitForOutput ouraOutput
    send = void . sendToOura ouraConnection
  pure MkOura {shutDown, receive, send}

launchOura ::
  FilePath ->
  Utils.SpotGarbage IO Process.ProcessHandle ->
  ContT r IO (Process.ProcessHandle, Async ())
launchOura configPath spotHandle = do
  ouraHandle <- lift do
    ouraHandle <-
      Process.spawnProcess
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

data OuraDaemonConnection = MkOuraDaemonConnection
  { ownSocket :: Socket.Socket
  , ouraAddress :: Socket.SockAddr
  }

connectToDaemon :: FilePath -> SourcePath -> IO OuraDaemonConnection
connectToDaemon ownSocketPath (MkSourcePath ouraSocketPath) = do
  let
    ouraAddress = Socket.SockAddrUnix $ T.unpack ouraSocketPath
    ownSocketHints =
      Socket.defaultHints
        { Socket.addrFamily = Socket.AF_UNIX
        , Socket.addrAddress = Socket.SockAddrUnix ownSocketPath
        , Socket.addrSocketType = Socket.Datagram
        }
  ownSocket <- Socket.openSocket ownSocketHints
  Socket.bind ownSocket (Socket.SockAddrUnix ownSocketPath)
  pure
    MkOuraDaemonConnection
      { ownSocket
      , ouraAddress = ouraAddress
      }

sendToOura :: OuraDaemonConnection -> BS.ByteString -> IO Int
sendToOura MkOuraDaemonConnection {ownSocket, ouraAddress} msg = do
  outcome <- Socket.BS.sendTo ownSocket msg ouraAddress
  putStrLn $ "Sent to oura, response code: " <> show outcome
  pure outcome

close :: OuraDaemonConnection -> IO ()
close MkOuraDaemonConnection {ownSocket} = do
  let
    timeoutMs = 2_000
  Socket.gracefulClose ownSocket timeoutMs

-- * Consuming Oura output

data OuraOutput = MkOuraOutput
  { output :: Chan BS.ByteString
  , sinkPath :: SinkPath
  , monitor :: Maybe ThreadId
  }

newtype Interval = MkIntervalMs {unIntervalMs :: Int}

listenOuraSink :: SinkPath -> Maybe Interval -> IO OuraOutput
listenOuraSink sinkPath monitoringInterval = do
  output <- newChan
  let
    mkOutput monitor =
      MkOuraOutput
        { output
        , sinkPath
        , monitor
        }
  mkOutput <$> for monitoringInterval \interval -> forkIO do
    monitor <- myThreadId
    monitorOutputs interval $ mkOutput $ Just monitor

stopMonitoring :: OuraOutput -> IO ()
stopMonitoring MkOuraOutput {monitor} = for_ monitor killThread

waitForOutput :: OuraOutput -> IO BS.ByteString
waitForOutput out@MkOuraOutput {output} = do
  collectOutput out
  readChan output

monitorOutputs :: Interval -> OuraOutput -> IO a
monitorOutputs MkIntervalMs {unIntervalMs} out = forever do
  collectOutput out
  let microSecondsInMillisecond = 1_000
  threadDelay $ microSecondsInMillisecond * unIntervalMs

collectOutput :: OuraOutput -> IO ()
collectOutput MkOuraOutput {output, sinkPath} = do
  let sink = T.unpack $ unSinkPath sinkPath
  contents <- BS.readFile sink
  BS.writeFile sink ""
  writeList2Chan output $ reverse $ BS.Char8.lines contents
