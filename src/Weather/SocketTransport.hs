{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Weather.SocketTransport where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr (withForeignPtr)
import System.Posix.Types (CSsize(..))
import Control.Exception (bracket)

-- Constants for C socket functions
af_inet :: CInt
af_inet = 2

sock_stream :: CInt
sock_stream = 1

foreign import ccall unsafe "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "connect" c_connect :: CInt -> Ptr () -> CUInt -> IO CInt
foreign import ccall unsafe "send" c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize
foreign import ccall unsafe "recv" c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize
foreign import ccall unsafe "close" c_close :: CInt -> IO CInt
foreign import ccall unsafe "inet_addr" c_inet_addr :: CString -> IO CUInt
foreign import ccall unsafe "htons" c_htons :: CShort -> IO CShort
foreign import ccall unsafe "gethostbyname" c_gethostbyname :: CString -> IO (Ptr ())

data Socket = Socket CInt

openSocket :: IO Socket
openSocket = do
    fd <- c_socket af_inet sock_stream 0
    if fd < 0
        then error "Could not create socket"
        else return (Socket fd)

closeSocket :: Socket -> IO ()
closeSocket (Socket fd) = do
    _ <- c_close fd
    return ()

resolveHost :: String -> IO String
resolveHost host = do
    c_host <- newCString host
    he_ptr <- c_gethostbyname c_host
    free c_host
    if he_ptr == nullPtr
        then error $ "Could not resolve host: " ++ host
        else do
            h_addr_list_ptr <- peekByteOff he_ptr 16 :: IO (Ptr (Ptr CUInt))
            h_addr_ptr <- peek h_addr_list_ptr
            addr <- peek h_addr_ptr
            return (show addr)

connectTo :: Socket -> String -> Int -> IO ()
connectTo (Socket fd) host port = do
    c_host <- newCString host
    he_ptr <- c_gethostbyname c_host
    free c_host
    
    addr <- if he_ptr == nullPtr
        then error $ "Could not resolve host: " ++ host
        else do
            h_addr_list_ptr <- peekByteOff he_ptr 24 :: IO (Ptr (Ptr CUInt))
            h_addr_ptr <- peek h_addr_list_ptr
            peek h_addr_ptr
    
    p_port <- c_htons (fromIntegral port)
    
    allocaBytes 16 $ \ptr -> do
        fillBytes ptr 0 16
        pokeByteOff ptr 0 (16 :: Word8)
        pokeByteOff ptr 1 (fromIntegral af_inet :: Word8)
        pokeByteOff ptr 2 p_port
        pokeByteOff ptr 4 addr
        
        res <- c_connect fd (castPtr ptr) 16
        if res < 0
            then error $ "Could not connect to " ++ host ++ ":" ++ show port
            else return ()

sendAll :: Socket -> ByteString -> IO ()
sendAll (Socket fd) bs = 
    let (fptr, off, len) = BI.toForeignPtr bs
    in withForeignPtr fptr $ \ptr -> do
        res <- c_send fd (ptr `plusPtr` off) (fromIntegral len) 0
        if res < 0
            then error "Send failed"
            else if fromIntegral res < len
                then sendAll (Socket fd) (B.drop (fromIntegral res) bs)
                else return ()

recvBuffer :: Int
recvBuffer = 4096

receive :: Socket -> IO ByteString
receive (Socket fd) = allocaBytes recvBuffer $ \ptr -> do
    res <- c_recv fd ptr (fromIntegral recvBuffer) 0
    if res < 0
        then error "Recv failed"
        else if res == 0
            then return B.empty
            else B.packCStringLen (castPtr ptr, fromIntegral res)

receiveAll :: Socket -> IO ByteString
receiveAll sock = do
    chunk <- receive sock
    if B.null chunk
        then return B.empty
        else do
            rest <- receiveAll sock
            return (B.append chunk rest)
