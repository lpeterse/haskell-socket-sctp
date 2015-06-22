{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bits
import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Protocol.SCTP as SCTP

main :: IO ()
main = do
  server <- socket                        `onException` p 0 :: IO (Socket Inet SequentialPacket SCTP)
  client <- socket                        `onException` p 1 :: IO (Socket Inet SequentialPacket SCTP)
  bind server addr                        `onException` p 3
  listen server 5                         `onException` p 4
  connect client addr                     `onException` p 5

  setSocketOption server (mempty { dataIOEvent = True })

  SCTP.sendMessage client "hallo" addr 1 mempty 2 3 4 `onException` p 6
  x@(msg, adr, sinfo, flags) <- SCTP.receiveMessage server 4096 mempty

  print x


addr :: SocketAddressInet
addr  = SocketAddressInet Inet.loopback 7777

p :: Int -> IO ()
p i = print i

e  :: Int -> IO ()
e i = error (show i)