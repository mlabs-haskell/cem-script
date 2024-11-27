{-# LANGUAGE BlockArguments #-}

module Oura.Communication (
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

import Cardano.CEM.OuraConfig (SinkPath, SourcePath (MkSourcePath), unSinkPath)
import Data.ByteString.Char8 qualified as BS.Char8

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
