module Oura
  ( connectToDaemon
  , sendToOura
  , close
  , exampleTx
  ) where

import Prelude
import qualified Network.Socket as Socket
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as Socket.BS

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