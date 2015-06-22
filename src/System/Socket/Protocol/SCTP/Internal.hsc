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
  -- * Socket Options
  -- ** Events
  , Events (..)
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
      deriving (Eq, Ord, Show, Num, Storable, Bits)

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
    c_memset ptr 0 $ fromIntegral (sizeOf a)
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

receiveMessage :: Family f => Socket f SequentialPacket SCTP 
                           -> Int -- ^ buffer size in bytes
                           -> MessageFlags 
                           -> IO (BS.ByteString, SocketAddress f, SendReceiveInfo, MessageFlags)
receiveMessage = f
  where
    f :: forall f t p. (Family f) => Socket f SequentialPacket SCTP -> Int -> MessageFlags -> IO (BS.ByteString, SocketAddress f, SendReceiveInfo, MessageFlags)
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
                      socketWaitRead
                      (\fd-> c_sctp_recvmsg fd bufPtr (fromIntegral bufSize) addrPtr addrSizePtr sinfoPtr flagsPtr )
                    addr   <- peek addrPtr
                    flags' <- peek flagsPtr
                    sinfo  <- peek sinfoPtr
                    msg    <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
                    return (msg, addr, sinfo, flags')
                )

sendMessage :: Family f => Socket f SequentialPacket SCTP
                        -> BS.ByteString
                        -> SocketAddress f
                        -> PayloadProtocolIdentifier -- ^ a user value not interpreted by SCTP
                        -> MessageFlags
                        -> StreamNumber
                        -> TimeToLive
                        -> Context
                        -> IO Int
sendMessage sock msg addr ppid flags sn ttl context = do
  alloca $ \addrPtr-> do
    BS.unsafeUseAsCStringLen msg $ \(msgPtr,msgSize)-> do
      poke addrPtr addr
      i <- tryWaitAndRetry
        sock
        socketWaitWrite
        $ \fd-> c_sctp_sendmsg
                  fd
                  msgPtr
                  (fromIntegral msgSize)
                  addrPtr
                  (fromIntegral $ sizeOf addr)
                  ppid
                  flags
                  sn
                  ttl
                  context
      return (fromIntegral i)

-- | @SCTP_EVENTS@
data Events
   = Events
     { dataIOEvent           :: Bool
     , associationEvent      :: Bool
     , addressEvent          :: Bool
     , sendFailureEvent      :: Bool
     , peerErrorEvent        :: Bool
     , shutdownEvent         :: Bool
     , partialDeliveryEvent  :: Bool
     , adaptationLayerEvent  :: Bool
     , authenticationEvent   :: Bool
     , senderDryEvent        :: Bool
     }
   deriving (Eq, Ord, Show)

instance Monoid Events where
  mempty = let x = False in Events x x x x x x x x x x
  mappend a b = Events
    (max (dataIOEvent          a) (dataIOEvent          b))
    (max (associationEvent     a) (associationEvent     b))
    (max (addressEvent         a) (addressEvent         b))
    (max (sendFailureEvent     a) (sendFailureEvent     b))
    (max (peerErrorEvent       a) (peerErrorEvent       b))
    (max (shutdownEvent        a) (shutdownEvent        b))
    (max (partialDeliveryEvent a) (partialDeliveryEvent b))
    (max (adaptationLayerEvent a) (adaptationLayerEvent b))
    (max (authenticationEvent  a) (authenticationEvent  b))
    (max (senderDryEvent       a) (senderDryEvent       b))

instance Storable Events where
  sizeOf    _ = (#size struct sctp_event_subscribe)
  alignment _ = (#alignment struct sctp_event_subscribe)
  peek ptr    = Events
    <$> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_data_io_event)            ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_association_event)        ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_address_event)            ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_send_failure_event)       ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_peer_error_event)         ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_shutdown_event)           ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_partial_delivery_event)   ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_adaptation_layer_event)   ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_authentication_event)     ptr :: Ptr CUChar)))
    <*> ((/=0) <$> (peek ((#ptr struct sctp_event_subscribe, sctp_sender_dry_event)         ptr :: Ptr CUChar)))
  poke ptr a = do
    c_memset ptr 0 $ fromIntegral (sizeOf a)
    poke ((#ptr struct sctp_event_subscribe, sctp_data_io_event)            ptr :: Ptr CUChar) (f $ dataIOEvent          a)
    poke ((#ptr struct sctp_event_subscribe, sctp_association_event)        ptr :: Ptr CUChar) (f $ associationEvent     a)
    poke ((#ptr struct sctp_event_subscribe, sctp_address_event)            ptr :: Ptr CUChar) (f $ addressEvent         a)
    poke ((#ptr struct sctp_event_subscribe, sctp_send_failure_event)       ptr :: Ptr CUChar) (f $ sendFailureEvent     a)
    poke ((#ptr struct sctp_event_subscribe, sctp_peer_error_event)         ptr :: Ptr CUChar) (f $ peerErrorEvent       a)
    poke ((#ptr struct sctp_event_subscribe, sctp_shutdown_event)           ptr :: Ptr CUChar) (f $ shutdownEvent        a)
    poke ((#ptr struct sctp_event_subscribe, sctp_partial_delivery_event)   ptr :: Ptr CUChar) (f $ partialDeliveryEvent a)
    poke ((#ptr struct sctp_event_subscribe, sctp_adaptation_layer_event)   ptr :: Ptr CUChar) (f $ adaptationLayerEvent a)
    poke ((#ptr struct sctp_event_subscribe, sctp_authentication_event)     ptr :: Ptr CUChar) (f $ authenticationEvent  a)
    poke ((#ptr struct sctp_event_subscribe, sctp_sender_dry_event)         ptr :: Ptr CUChar) (f $ senderDryEvent       a)
    where
      f True  = 1
      f False = 0

instance SetSocketOption Events where
  setSocketOption sock value =
    unsafeSetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_EVENTS) value

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall unsafe "memset"
  c_memset   :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall unsafe "sctp_recvmsg"
  c_sctp_recvmsg :: Fd -> Ptr a -> CSize -> Ptr b -> Ptr CInt -> Ptr SendReceiveInfo -> Ptr MessageFlags -> IO CInt

foreign import ccall unsafe "sctp_sendmsg"
  c_sctp_sendmsg :: Fd -> Ptr a -> CSize -> Ptr b -> CInt -> PayloadProtocolIdentifier -> MessageFlags -> StreamNumber -> TimeToLive -> Context -> IO CInt