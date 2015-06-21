{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module System.Socket.Protocol.SCTP.Internal
  ( SCTP
  -- * SendReceiveInfo
  , SendReceiveInfo (..)
  , StreamNumber (..)
  , StreamSequenceNumber (..)
  , SendReceiveInfoFlags (..)
  , PayloadProtocolIdentifier (..)
  , Context (..)
  , TimeToLive (..)
  , TransportSequenceNumber (..)
  , CumulatedTransportSequenceNumber (..)
  , AssociationIdentifier (..)
  ) where

import Data.Bits
import Data.Word

import Foreign.Storable
import Foreign.Ptr

import System.Socket.Protocol

#include "netinet/sctp.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data    SCTP

data SendReceiveInfo
   = SendReceiveInfo
     { sinfoStreamNumber                      :: StreamNumber
     , sinfoStreamSequenceNumber              :: StreamSequenceNumber
     , sinfoFlags                             :: SendReceiveInfoFlags
     , sinfoPayloadProtocolIdentifier         :: PayloadProtocolIdentifier
     , sinfoContext                           :: Context
     , sinfoTimeToLive                        :: TimeToLive
     , sinfoTransportSequenceNumber           :: TransportSequenceNumber
     , sinfoCumulatedTransportSequenceNumber  :: CumulatedTransportSequenceNumber
     , sinfoAssociationIdentifier             :: AssociationIdentifier
     }
     deriving (Eq, Show)

newtype StreamNumber
      = StreamNumber Word16
      deriving (Eq, Ord, Show, Num, Storable)

newtype StreamSequenceNumber
      = StreamSequenceNumber Word16
      deriving (Eq, Ord, Show, Num, Storable)

newtype SendReceiveInfoFlags
      = SendReceiveInfoFlags Word16
      deriving (Eq, Ord, Show, Bits, Storable)

newtype PayloadProtocolIdentifier
      = PayloadProtocolIdentifier Word32
      deriving (Eq, Ord, Show, Num, Storable)

newtype TimeToLive
      = TimeToLive Word32
      deriving (Eq, Ord, Show, Num, Storable)

newtype Context
      = Context Word32
      deriving (Eq, Ord, Show, Num, Storable)

newtype TransportSequenceNumber
      = TransportSequenceNumber Word32
      deriving (Eq, Ord, Show, Num, Storable)

newtype CumulatedTransportSequenceNumber
      = CumulatedTransportSequenceNumber Word32
      deriving (Eq, Ord, Show, Num, Storable)

newtype AssociationIdentifier
      = AssociationIdentifier Word32
      deriving (Eq, Ord, Show, Num, Storable)

instance Protocol  SCTP where
  protocolNumber _ = (#const IPPROTO_SCTP)

unordered       :: SendReceiveInfoFlags
unordered        = SendReceiveInfoFlags (#const SCTP_UNORDERED)

addressOverride :: SendReceiveInfoFlags
addressOverride  = SendReceiveInfoFlags (#const SCTP_ADDR_OVER)

abort           :: SendReceiveInfoFlags
abort            = SendReceiveInfoFlags (#const SCTP_ABORT)

shutdown        :: SendReceiveInfoFlags
shutdown         = SendReceiveInfoFlags (#const SCTP_EOF)


instance Storable SendReceiveInfo where
  sizeOf    _ = (#size struct sctp_sndrcvinfo)
  alignment _ = (#alignment struct sctp_sndrcvinfo)
  peek ptr    = SendReceiveInfo
    <$> peek ((#ptr struct sctp_sndrcvinfo, sinfo_stream)     ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_ssn)        ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_flags)      ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_ppid)       ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_context)    ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_timetolive) ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_tsn)        ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_cumtsn)     ptr)
    <*> peek ((#ptr struct sctp_sndrcvinfo, sinfo_assoc_id)   ptr)
  poke ptr a = do
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_stream)     ptr) (sinfoStreamNumber a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_ssn)        ptr) (sinfoStreamSequenceNumber a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_flags)      ptr) (sinfoFlags a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_ppid)       ptr) (sinfoPayloadProtocolIdentifier a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_context)    ptr) (sinfoContext a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_timetolive) ptr) (sinfoTimeToLive a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_tsn)        ptr) (sinfoTransportSequenceNumber a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_cumtsn)     ptr) (sinfoCumulatedTransportSequenceNumber a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_assoc_id)   ptr) (sinfoAssociationIdentifier a)

