{-# LANGUAGE BlockArguments #-}
module Oura
  ( connectToDaemon
  , sendToOura
  , close
  , exampleTx

  , listenOuraSink
  , stopMonitoring
  , waitForOutput
  ) where

import Prelude
import qualified Network.Socket as Socket
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as Socket.BS
import Control.Concurrent (Chan, writeList2Chan, readChan, threadDelay, newChan, ThreadId, forkIO, killThread, myThreadId)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import Control.Monad (forever, void, replicateM)
import Data.Traversable (for)
import Data.Foldable (for_)

exampleTx :: IO BS.ByteString
exampleTx = BS.readFile "./tx.json"

data OuraDaemonConnection = MkOuraDaemonConnection
  { ownSocket :: Socket.Socket
  , ouraAddress :: Socket.SockAddr
  }

connectToDaemon :: FilePath -> FilePath -> IO OuraDaemonConnection
connectToDaemon ownSocketPath ouraSocketPath = do
  let
    ouraAddress = Socket.SockAddrUnix ouraSocketPath
    ownSocketHints = Socket.defaultHints
        { Socket.addrFamily = Socket.AF_UNIX
        , Socket.addrAddress = Socket.SockAddrUnix ownSocketPath
        , Socket.addrSocketType = Socket.Datagram
        }
  ownSocket <- Socket.openSocket ownSocketHints
  Socket.bind ownSocket (Socket.SockAddrUnix ownSocketPath)
  pure MkOuraDaemonConnection
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
  { output :: Chan T.Text
  , sinkPath :: FilePath
  , monitor :: Maybe ThreadId
  }

newtype Interval = MkIntervalMs { unIntervalMs :: Int }

listenOuraSink :: FilePath -> Maybe Interval -> IO OuraOutput
listenOuraSink sinkPath monitoringInterval = do
  output <- newChan
  let
    mkOutput monitor = MkOuraOutput
      { output
      , sinkPath
      , monitor
      }
  mkOutput <$> for monitoringInterval \interval -> forkIO do
    monitor <- myThreadId
    monitorOutputs interval $ mkOutput $ Just monitor

stopMonitoring :: OuraOutput -> IO ()
stopMonitoring MkOuraOutput {monitor} = for_ monitor killThread

waitForOutput :: OuraOutput -> IO T.Text
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
  contents <- BS.readFile sinkPath
  BS.writeFile sinkPath ""
  writeList2Chan output $ reverse $ T.lines $ Text.Encoding.decodeUtf8 contents
