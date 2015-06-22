{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad

import System.Socket
import System.Socket.Family.Inet as Inet
import System.Socket.Protocol.SCTP as SCTP

main :: IO ()
main = do
  client <- socket :: IO (Socket Inet SequentialPacket SCTP)
  print =<< SCTP.sendMessage
    client
    "hallo"
    ( SocketAddressInet Inet.loopback 7777 )
    ( 2342   :: PayloadProtocolIdentifier )
    ( mempty :: MessageFlags )
    ( 2      :: StreamNumber )
    ( 0      :: TimeToLive )
    ( 0      :: Context )