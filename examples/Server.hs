{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Protocol.SCTP as SCTP

main :: IO ()
main = do
  server <- socket  :: IO (Socket Inet SequentialPacket SCTP)
  bind server (SocketAddressInet Inet.loopback 7777)
  listen server 5
  setSocketOption server (mempty { dataIOEvent = True })
  forever $ do
    x@(msg, adr, sinfo, flags) <- SCTP.receiveMessage server 4096 mempty
    print x