{-# LANGUAGE OverloadedStrings #-}
#include "netinet/sctp.h"
#ifdef SCTP_SENDALL
module Main where

import System.Socket
import System.Socket.Type.SequentialPacket
import System.Socket.Protocol.SCTP as SCTP
import System.Socket.Family.Inet as Inet
import Control.Monad

main :: IO ()
main = do
  server <- socket :: IO (Socket Inet SequentialPacket SCTP)
  client1 <- socket :: IO (Socket Inet SequentialPacket SCTP)
  client2 <- socket :: IO (Socket Inet SequentialPacket SCTP)
  bind server addr
  listen server 5
  connect client1 addr
  connect client2 addr
  SCTP.sendMessage
    server
    "hallo"
    Nothing
    0
    sendall
    0
    3
    0
  (msg1, _, _, _) <- SCTP.receiveMessage client1 4096 mempty
  when (msg1 /= "hallo") (error "0")
  (msg2, _, _, _) <- SCTP.receiveMessage client2 4096 mempty
  when (msg2 /= "hallo") (error "1")

addr :: SocketAddress Inet
addr  = SocketAddressInet Inet.inetLoopback 7777

#else

module Main where

main :: IO ()
main = return ()

#endif
