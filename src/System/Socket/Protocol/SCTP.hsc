{-# LANGUAGE TypeFamilies #-}
module System.Socket.Protocol.SCTP 
  ( SCTP
  ) where

import Data.Word
import qualified Data.ByteString as BS

import Foreign.C.Types

import System.Socket.Protocol

#include "sys/socket.h"
#include "netinet/in.h"
#include "netinet/sctp.h"

data    SCTP

newtype PPID
      = PPID Word32
      deriving (Eq, Ord, Show, Num)

newtype StreamNumber
      = StreamNumber Word16
      deriving (Eq, Ord, Show, Num)

newtype SctpTimeToLive
      = SctpTimeToLive Word32
      deriving (Eq, Ord, Show, Num)

newtype SctpContext
      = SctpContext Word32
      deriving (Eq, Ord, Show, Num)

newtype SendReceiveInfoFlags
      = SendReceiveInfoFlags Word32
      deriving (Eq, Ord, Show, Bits)

data SendReceiveInfo
   = SendReceiveInfo
     { sinfoStreamNumber                      :: StreamNumber
     , sinfoStreamSequenceNumber              :: SSN
     , sinfoFlags                             :: SendReceiveInfoFlags
     , sinfoPPID                              :: PPID
     , sinfoContext                           :: Context
     , sinfoTimeToLive                        :: TimeToLive
     , sinfoTransportSequenceNumber           :: TSN
     , sinfoCumulatedTransportSequenceNumber  :: TSN
     }
     deriving (Eq, Show)

instance Protocol  SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)


sctpUNORDERED :: SctpFlags
sctpUNORDERED  = SctpFlags (#const SCTP_UNORDERED)

sctpADDR_OVER :: SctpFlags
sctpADDR_OVER  = SctpFlags (#const SCTP_ADDR_OVER)

sctpABORT     :: SctpFlags
sctpABORT      = SctpFlags (#const SCTP_ABORT)

sctpEOF       :: SctpFlags
sctpEOF        = SctpFlags (#const SCTP_EOF)
