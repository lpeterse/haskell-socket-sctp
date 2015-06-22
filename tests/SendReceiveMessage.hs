{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Protocol.SCTP as SCTP

main :: IO ()
main = do
  server <- socket                  `onException` p 0 :: IO (Socket Inet SequentialPacket SCTP)
  client <- socket                  `onException` p 1 :: IO (Socket Inet SequentialPacket SCTP)
  bind server addr                  `onException` p 3
  listen server 5                   `onException` p 4
  connect client addr               `onException` p 5
  setSocketOption
    server 
    (mempty { dataIOEvent = True }) `onException` p 6
  SCTP.sendMessage
    client
    "hallo"
    addr
    ( 2342   :: PayloadProtocolIdentifier )
    ( mempty :: MessageFlags )
    ( 2      :: StreamNumber )
    ( 3      :: TimeToLive )
    ( 4      :: Context )           `onException` p 7
  (msg, adr, sinfo, flags) <- SCTP.receiveMessage server 4096 mempty
                                    `onException` p 8

  when (sinfoPayloadProtocolIdentifier sinfo /= 2342) (e 9)
  when (sinfoStreamNumber sinfo /= 2) (e 10)
  when (msg /= "hallo") (e 11)
  when (flags /= msgEndOfRecord) (e 12)

addr :: SocketAddressInet
addr  = SocketAddressInet Inet.loopback 7777

p :: Int -> IO ()
p i = print i

e  :: Int -> IO ()
e i = error (show i)