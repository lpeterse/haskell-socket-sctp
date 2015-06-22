{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module System.Socket.Protocol.SCTP.Internal
  ( SCTP
  -- * Operations
  -- ** receiveMessage
  , receiveMessage
  -- ** sendMessage
  , sendMessage
  -- * SendReceiveInfo
  , SendReceiveInfo (..)
  , StreamNumber (..)
  , StreamSequenceNumber (..)
  , PayloadProtocolIdentifier (..)
  , Context (..)
  , TimeToLive (..)
  , TransportSequenceNumber (..)
  , CumulatedTransportSequenceNumber (..)
  , AssociationIdentifier (..)
  -- * SendReceiveInfoFlags
  , SendReceiveInfoFlags (..)
  -- ** unordered
  , unordered
  -- ** addressOverride
  , addressOverride
  -- ** abort
  , abort
  -- ** shutdown
  , shutdown
  ) where

import Control.Applicative
import Control.Exception

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.Types

import System.Posix.Types ( Fd(..) )

import System.Socket
import System.Socket.Unsafe
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

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

receiveMessage :: Family f => Socket f t SCTP -> Int -> MessageFlags -> IO (BS.ByteString, SocketAddress f, SendReceiveInfo, MessageFlags)
receiveMessage = f
  where
    f :: forall f t p. (Family f) => Socket f t p -> Int -> MessageFlags -> IO (BS.ByteString, SocketAddress f, SendReceiveInfo, MessageFlags)
    f sock bufSize flags = do
      alloca $ \addrPtr-> do
        alloca $ \addrSizePtr-> do
          alloca $ \sinfoPtr-> do
            alloca $ \flagsPtr -> do
              c_memset sinfoPtr 0 (#size struct sctp_sndrcvinfo)
              poke addrSizePtr (fromIntegral $ sizeOf (undefined :: SocketAddress f))
              poke flagsPtr (flags `mappend` msgNoSignal)
              bracketOnError
                ( mallocBytes bufSize )
                (\bufPtr-> free bufPtr )
                (\bufPtr-> do
                    bytesReceived <- tryWaitAndRetry
                      sock
                      socketWaitWrite
                      (\fd-> c_sctp_recvmsg fd bufPtr (fromIntegral bufSize) addrPtr addrSizePtr sinfoPtr flagsPtr )
                    addr   <- peek addrPtr
                    flags' <- peek flagsPtr
                    sinfo  <- peek sinfoPtr
                    msg    <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
                    return (msg, addr, sinfo, flags')
                )

sendMessage :: Family f => Socket f t SCTP
                        -> BS.ByteString
                        -> SocketAddress f
                        -> PayloadProtocolIdentifier
                        -> MessageFlags
                        -> StreamNumber
                        -> TimeToLive
                        -> IO Int
sendMessage = do
  undefined

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall unsafe "memset"
  c_memset   :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall unsafe "sctp_recvmsg"
  c_sctp_recvmsg :: Fd -> Ptr a -> CSize -> Ptr b -> Ptr CInt -> Ptr SendReceiveInfo -> Ptr MessageFlags -> IO CInt

foreign import ccall unsafe "sctp_sendmsg"
  c_sctp_sendmsg :: Fd -> Ptr a -> CSize -> PayloadProtocolIdentifier -> MessageFlags -> StreamNumber -> TimeToLive -> Context -> IO CInt