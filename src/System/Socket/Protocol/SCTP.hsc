-----------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Protocol.SCTP
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
-----------------------------------------------------------------------------
module System.Socket.Protocol.SCTP (
  -- * Examples
  -- ** Server
  -- $server

  -- ** Client
  -- $client
  --

   module System.Socket.Protocol.SCTP.Internal
  ) where

import System.Socket.Protocol.SCTP.Internal

-- $server
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- > 
-- > import Data.Monoid
-- > import Control.Monad
-- > 
-- > import System.Socket
-- > import System.Socket.Family.Inet as Inet
-- > import System.Socket.Protocol.SCTP as SCTP
-- > 
-- > main :: IO ()
-- > main = do
-- >   server <- socket  :: IO (Socket Inet SequentialPacket SCTP)
-- >   bind server (SocketAddressInet Inet.loopback 7777)
-- >   listen server 5
-- >   setSocketOption server (mempty { dataIOEvent = True })
-- >   forever $ do
-- >     x@(msg, adr, sinfo, flags) <- SCTP.receiveMessage server 4096 mempty
-- >     print x

-- $client
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- > 
-- > import Data.Monoid
-- > import Control.Monad
-- > 
-- > import System.Socket
-- > import System.Socket.Family.Inet as Inet
-- > import System.Socket.Protocol.SCTP as SCTP
-- > 
-- > main :: IO ()
-- > main = do
-- >   client <- socket :: IO (Socket Inet SequentialPacket SCTP)
-- >   print =<< SCTP.sendMessage
-- >     client
-- >     "hallo"
-- >     ( SocketAddressInet Inet.loopback 7777 )
-- >     ( 2342   :: PayloadProtocolIdentifier )
-- >     ( mempty :: MessageFlags )
-- >     ( 2      :: StreamNumber )
-- >     ( 0      :: TimeToLive )
-- >     ( 0      :: Context )
