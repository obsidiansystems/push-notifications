{-# LANGUAGE OverloadedStrings #-}

module IOS (pushMessLive, pushMessTest, checkFailLive, checkFailTest) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import GHC.Word (Word32, Word16)
import Data.Convertible (convert)

import qualified Data.ByteString.Base16 as B16

import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.Socket
import Network.BSD (getHostByName, hostAddress, getProtocolNumber)
import OpenSSL
import OpenSSL.Session as SSL
  
{-
show link to source
-}
  
pushMessLive :: FilePath -> FilePath -> BL.ByteString -> [B.ByteString] -> IO ()
pushMessLive =
  pushMess "gateway.push.apple.com"
  
pushMessTest :: FilePath -> FilePath -> BL.ByteString -> [B.ByteString] -> IO ()
pushMessTest =
  pushMess "gateway.sandbox.push.apple.com"

checkFailLive :: FilePath -> FilePath -> IO [B.ByteString]
checkFailLive =
  checkFail "feedback.push.apple.com"
  
checkFailTest :: FilePath -> FilePath -> IO [B.ByteString]
checkFailTest =
  checkFail "feedback.sandbox.push.apple.com"


splitBS :: B.ByteString -> [B.ByteString]
splitBS xs =
  let xs1 = B.drop 6 xs
      token = B.take 32 xs1
      nexst = B.drop 32 xs1
  in
   if B.null token then [] else token:(splitBS nexst)
      
  

checkFail :: String -> FilePath -> FilePath -> IO [B.ByteString]
checkFail server keyfile certfile = withOpenSSL $ do
  ssl <- context
  contextSetPrivateKeyFile ssl keyfile
  contextSetCertificateFile ssl certfile
  contextSetDefaultCiphers ssl
  contextSetVerificationMode ssl SSL.VerifyNone
  
  proto <- (getProtocolNumber "tcp")
  he <- getHostByName server
  sock <- socket AF_INET Stream proto
  Network.Socket.connect sock (SockAddrInet 2196 (hostAddress he))
  
  sslsocket <- connection ssl sock
  SSL.connect sslsocket  -- Handshake
  bs <- SSL.read sslsocket 7600000
  print $ B.length bs
  SSL.shutdown sslsocket Unidirectional
  
  return $ splitBS bs
  
pushMess :: String -> FilePath -> FilePath -> BL.ByteString -> [B.ByteString] -> IO ()  
pushMess server keyfile certfile payload tokens = withOpenSSL $ do
  -- Prepare SSL context
  ssl <- context
  contextSetPrivateKeyFile ssl keyfile
  contextSetCertificateFile ssl certfile
  contextSetDefaultCiphers ssl
  contextSetVerificationMode ssl SSL.VerifyNone

  -- Open socket
  proto <- (getProtocolNumber "tcp")
  he <- getHostByName server
  sock <- socket AF_INET Stream proto
  Network.Socket.connect sock (SockAddrInet 2195 (hostAddress he))

  -- Promote socket to SSL stream
  sslsocket <- connection ssl sock
  SSL.connect sslsocket  -- Handshake

  expiration <- getExpiryTime
  let sendPDU token =
        let btoken = fst $ B16.decode token
            lpdu = runPut $ buildPDU btoken payload expiration
            pdu = B.concat $ BL.toChunks lpdu
        in 
          SSL.write sslsocket pdu
  sequence_ $ map sendPDU tokens
  SSL.shutdown sslsocket Unidirectional -- Close gracefully


buildPDU :: B.ByteString -> BL.ByteString -> Word32 -> Put
buildPDU token payload expiry
  | (B.length token) /= 32 = fail "Invalid token"
  | (BL.length payload > 255) = fail "Too long payload"
  | otherwise = do
    putWord8 1
    putWord32be 1
    putWord32be expiry
    putWord16be ((convert $ B.length token) :: Word16)
    putByteString token
    putWord16be ((convert $ BL.length payload) :: Word16)
    putLazyByteString payload


getExpiryTime :: IO (Word32)
getExpiryTime = do
  pt <- getPOSIXTime
  -- One hour expiry time
  return ( (round pt + 60*60):: Word32)