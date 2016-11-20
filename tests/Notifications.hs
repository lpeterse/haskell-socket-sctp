{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Bits

import System.Socket
import System.Socket.Type.SequentialPacket
import System.Socket.Family.Inet as Inet
import System.Socket.Protocol.SCTP as SCTP

main :: IO ()
main = do
  server <- socket                  `onException` p 0 :: IO (Socket Inet SequentialPacket SCTP)
  client <- socket                  `onException` p 1 :: IO (Socket Inet SequentialPacket SCTP)
  bind server addr                  `onException` p 3
  listen server 5                   `onException` p 4

  setSocketOption
    server
    (mempty { associationEvent = True }) `onException` p 5
  connect client addr               `onException` p 6

  (msg, adr, sinfo, flags) <- SCTP.receiveMessage server 4096 mempty
                                    `onException` p 7

  when (flags .&. msgEndOfRecord == mempty) (e 8)
  when (flags .&. msgNotification == mempty) (e 9)
  notification <- unsafeParseNotification msg
  case notification of
    AssocChangeNotification (AssocChange { acState = COMM_UP }) -> return ()
    _ -> e 10

addr :: SocketAddress Inet
addr  = SocketAddressInet Inet.inetLoopback 7777

p :: Int -> IO ()
p i = print i

e  :: Int -> IO ()
e i = error (show i)
