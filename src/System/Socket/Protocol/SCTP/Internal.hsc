{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
#include "netinet/sctp.h"
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
  , CumulativeTransportSequenceNumber (..)
  , AssociationIdentifier (..)
  , SendmsgFlags (..)
#ifdef SCTP_SENDALL
  , sendall
#endif
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
  -- ** InitMessage
  , InitMessage (..)
  -- ** Events
  , Events (..)
  ) where

import Control.Applicative
import Control.Exception

import Data.Bits
import Data.Monoid
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
import System.Socket.Type.SequentialPacket
import System.Socket.Type.Stream

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

data    SCTP

-- | Class containing all protocol types that can be used with SCTP.
class SCTPType t where

instance SCTPType SequentialPacket
instance SCTPType Stream

data SendReceiveInfo
   = SendReceiveInfo
     { sinfoStreamNumber                      :: StreamNumber
     , sinfoStreamSequenceNumber              :: StreamSequenceNumber
     , sinfoFlags                             :: SendReceiveInfoFlags
     , sinfoPayloadProtocolIdentifier         :: PayloadProtocolIdentifier
     , sinfoContext                           :: Context
     , sinfoTimeToLive                        :: TimeToLive
     , sinfoTransportSequenceNumber           :: TransportSequenceNumber
     , sinfoCumulativeTransportSequenceNumber  :: CumulativeTransportSequenceNumber
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

newtype SendmsgFlags
      = SendmsgFlags Word32
      deriving (Eq, Ord, Show, Num, Storable, Bits)

instance Monoid SendmsgFlags where
  mempty  = SendmsgFlags 0
  mappend = (.|.)

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

newtype CumulativeTransportSequenceNumber
      = CumulativeTransportSequenceNumber Word32
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

#ifdef SCTP_SENDALL
sendall         :: SendmsgFlags
sendall          = SendmsgFlags (#const SCTP_SENDALL)
#endif

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
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_cumtsn)     ptr) (sinfoCumulativeTransportSequenceNumber a)
    poke ((#ptr struct sctp_sndrcvinfo, sinfo_assoc_id)   ptr) (sinfoAssociationIdentifier a)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Receive a message on a SCTP socket.
--
-- - Everything that applies to `System.Socket.receive` is also true for this operation.
-- - The fields of the `SendReceiveInfo` structure are only filled if `dataIOEvent`
--   has been enabled trough the `Events` socket option.
-- - If the supplied buffer size is not sufficient, several consecutive reads are
--   necessary to receive the complete message. The `msgEndOfRecord` flag is set
--   when the message has been read completely.
receiveMessage :: (Family f, Storable (SocketAddress f), SCTPType t) => Socket f t SCTP
                                         -> Int -- ^ buffer size in bytes
                                         -> MessageFlags
                                         -> IO (BS.ByteString, SocketAddress f, SendReceiveInfo, MessageFlags)
receiveMessage sock bufSize flags = do
  alloca $ \addrPtr-> do
    alloca $ \addrSizePtr-> do
      alloca $ \sinfoPtr-> do
        alloca $ \flagsPtr -> do
          uaddr <- return undefined
          c_memset sinfoPtr 0 (#size struct sctp_sndrcvinfo)
          poke addrSizePtr (fromIntegral $ sizeOf uaddr)
          poke flagsPtr (flags `mappend` msgNoSignal)
          bracketOnError
            ( mallocBytes bufSize )
            (\bufPtr-> free bufPtr )
            (\bufPtr-> do
                bytesReceived <- tryWaitRetryLoop
                  sock
                  unsafeSocketWaitRead
                  (\fd-> c_sctp_recvmsg fd bufPtr (fromIntegral bufSize) addrPtr addrSizePtr sinfoPtr flagsPtr )
                addr   <- peek addrPtr
                flags' <- peek flagsPtr
                sinfo  <- peek sinfoPtr
                msg    <- BS.unsafePackMallocCStringLen (bufPtr, fromIntegral bytesReceived)
                return (msg, addr `asTypeOf` uaddr, sinfo, flags')
            )

-- | Send a message on a SCTP socket.
--
-- - Everything that applies to `System.Socket.send` is also true for this operation.
-- - Sending a message is atomic unless the `ExplicitEndOfRecord` option has been enabled (not yet supported),
sendMessage :: (Storable (SocketAddress f)) => Socket f t SCTP
                                      -> BS.ByteString
                                      -> Maybe (SocketAddress f)
                                      -> PayloadProtocolIdentifier -- ^ a user value not interpreted by SCTP
                                      -> SendmsgFlags
                                      -> StreamNumber
                                      -> TimeToLive
                                      -> Context
                                      -> IO Int
sendMessage sock msg addr ppid flags sn ttl context = do
  BS.unsafeUseAsCStringLen msg $ \(msgPtr,msgSize)-> do
    let finish addrPtr sz = do
          i <- tryWaitRetryLoop
            sock
            unsafeSocketWaitWrite
              $ \fd-> c_sctp_sendmsg
                      fd
                      msgPtr
                      (fromIntegral msgSize)
                      addrPtr
                      sz
                      ppid
                      flags
                      sn
                      ttl
                      context
          return (fromIntegral i)
    case addr of
      Just addr' -> do
        alloca $ \addrPtr-> do
          poke addrPtr addr'
          finish addrPtr (fromIntegral $ sizeOf addr')
      Nothing -> finish nullPtr 0

{-- NOT YET SUPPORTED :-(

-- | @SCTP_EXPLICIT_EOR@
data ExplicitEndOfRecord
   = ExplicitEndOfRecord Bool
     deriving (Eq, Ord, Show)

instance Storable ExplicitEndOfRecord where
  sizeOf _    = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr    = do
    i <- peek ptr :: IO CInt
    return (ExplicitEndOfRecord $ i /= 0)
  poke ptr (ExplicitEndOfRecord False)  = do
    poke ptr (0 :: CInt)
  poke ptr (ExplicitEndOfRecord True) = do
    poke ptr (1 :: CInt)

instance GetSocketOption ExplicitEndOfRecord where
  getSocketOption sock =
    unsafeGetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_EXPLICIT_EOR)

instance SetSocketOption ExplicitEndOfRecord where
  setSocketOption sock value =
    unsafeSetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_EXPLICIT_EOR) value
--}

-- | @SCTP_INITMSG@
data InitMessage
   = InitMessage
     { outboundStreams     :: Word16 -- ^ number of outbound streams
     , maxInboundStreams   :: Word16 -- ^ max number of inbound streams
     , maxAttempts         :: Word16 -- ^ max number re-transmissions while establishing an association
     , maxInitTimeout      :: Word16 -- ^ time-out in milliseconds for establishing an association
     } deriving (Eq, Ord, Show)

instance Storable InitMessage where
  sizeOf    _ = (#size struct sctp_initmsg)
  alignment _ = (#alignment struct sctp_initmsg)
  peek ptr    = InitMessage
    <$> peek ( (#ptr struct sctp_initmsg, sinit_num_ostreams)   ptr)
    <*> peek ( (#ptr struct sctp_initmsg, sinit_max_instreams)  ptr)
    <*> peek ( (#ptr struct sctp_initmsg, sinit_max_attempts)   ptr)
    <*> peek ( (#ptr struct sctp_initmsg, sinit_max_init_timeo) ptr)
  poke ptr a  = do
    c_memset ptr 0 $ fromIntegral (sizeOf a)
    poke ((#ptr struct sctp_initmsg, sinit_num_ostreams)   ptr) (outboundStreams    a)
    poke ((#ptr struct sctp_initmsg, sinit_max_instreams)  ptr) (maxInboundStreams  a)
    poke ((#ptr struct sctp_initmsg, sinit_max_attempts)   ptr) (maxAttempts        a)
    poke ((#ptr struct sctp_initmsg, sinit_max_init_timeo) ptr) (maxInitTimeout     a)

instance SocketOption InitMessage where
  getSocketOption sock =
    unsafeGetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_INITMSG)

  setSocketOption sock value =
    unsafeSetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_INITMSG) value

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
     }
   deriving (Eq, Ord, Show)

instance Monoid Events where
  mempty = let x = False in Events x x x x x x x x x -- x
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
    where
      f True  = 1
      f False = 0

instance SocketOption Events where
  getSocketOption sock =
    unsafeGetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_EVENTS)

  setSocketOption sock value =
    unsafeSetSocketOption sock (#const IPPROTO_SCTP) (#const SCTP_EVENTS) value

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall unsafe "memset"
  c_memset   :: Ptr a -> CInt -> CSize -> IO ()

foreign import ccall unsafe "hs_sctp_recvmsg"
  c_sctp_recvmsg :: Fd -> Ptr a -> CSize -> Ptr b -> Ptr CInt -> Ptr SendReceiveInfo -> Ptr MessageFlags -> Ptr CInt -> IO CInt

foreign import ccall unsafe "hs_sctp_sendmsg"
  c_sctp_sendmsg :: Fd -> Ptr a -> CSize -> Ptr b -> CInt -> PayloadProtocolIdentifier -> SendmsgFlags -> StreamNumber -> TimeToLive -> Context -> Ptr CInt -> IO CInt
