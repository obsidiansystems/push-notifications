{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: PhonePush.IOS
--
-- Apple Push Notification Service provider
--
-- Sending push notifications requires an "Apple Push Services" certificate and
-- an Apple-provided device token.
--
-- == Getting an APS Certificate
--
-- The APS certificate is produced in the iOS Provisioning Portal. Once you've
-- generated the certificate, you can download it from the Provisioning Portal.
-- It is usually named @aps_production.cer@ or @aps_development.cer@.
--
-- The private key for the certificate can be extracted from Apple's Keychain
-- utility as a @.p12@ file.
--
-- Once you have both the certificate and private key, the following commands
-- can be used to convert the certificate and private key files into the format
-- required by this library.
--
-- > openssl x509 -in aps_development.cer -inform DER -outform PEM -out cert.pem
-- > openssl pkcs12 -in key.p12 -out key.pem -nodes
--
-- == Getting a Device Token
--
-- Device tokens are retrieved from Apple on the device itself by calling
-- the @registerForRemoteNotifications@ method of the @UIApplication@ object.
-- For more information, please see Apple's documentation <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/HandlingRemoteNotifications.html#//apple_ref/doc/uid/TP40008194-CH6-SW1 here>.
--
-- == Credits
-- Originally based on a blog post by Teemu Ikonen, available <https://bravenewmethod.com/2012/11/08/apple-push-notifications-with-haskell/ here>.

module PhonePush.IOS where

import Control.Exception (bracket, catch, IOException)
import Data.Binary.Put
import Data.Convertible (convert)
import GHC.Word (Word32, Word16)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.BSD (getHostByName, hostAddress, getProtocolNumber)
import Network.Socket
import OpenSSL
import OpenSSL.Session as SSL

data APNSConfig = APNSConfig
  { _APNSConfig_server :: String
  , _APNSConfig_key :: FilePath
  , _APNSConfig_certificate :: FilePath
  }
  deriving (Show, Read, Eq, Ord)

gatewayLive :: String
gatewayLive = "gateway.push.apple.com"

gatewayTest :: String
gatewayTest = "gateway.sandbox.push.apple.com"

feedbackLive :: String
feedbackLive = "feedback.push.apple.com"

feedbackTest :: String
feedbackTest = "feedback.sandbox.push.apple.com"

data ApplePushMessage = ApplePushMessage
  { _applePushMessage_deviceToken :: B.ByteString
  , _applePushMessage_payload :: BL.ByteString
  -- ^ JSON encoded payload, conforming to <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/PayloadKeyReference.html#//apple_ref/doc/uid/TP40008194-CH17-SW1 this specification>. See "PhonePush.IOS.Payload"
  , _applePushMessage_expiry :: Word32
  }

checkFailLive :: FilePath -> FilePath -> IO [B.ByteString]
checkFailLive = checkFail feedbackLive

checkFailTest :: FilePath -> FilePath -> IO [B.ByteString]
checkFailTest = checkFail feedbackTest

withSocketSafe :: ProtocolNumber -> (Socket -> IO a) -> IO a
withSocketSafe proto =
  bracket (socket AF_INET Stream proto) $ \sock ->
    catch (close sock) $ \(e :: IOException) ->
      putStrLn . unwords $ [ "Caught exception trying to close"
                           , "Apple push notifications socket:"
                           , show e
                           ]

checkFail :: String -> FilePath -> FilePath -> IO [B.ByteString]
checkFail server keyfile certfile = withOpenSSL $ do
  ssl <- context
  contextSetPrivateKeyFile ssl keyfile
  contextSetCertificateFile ssl certfile
  contextSetDefaultCiphers ssl
  contextSetVerificationMode ssl SSL.VerifyNone

  proto <- (getProtocolNumber "tcp")
  he <- getHostByName server
  withSocketSafe proto $ \sock -> do
    Network.Socket.connect sock (SockAddrInet 2196 (hostAddress he))

    sslsocket <- connection ssl sock
    SSL.connect sslsocket  -- Handshake
    bs <- SSL.read sslsocket 7600000
    print $ B.length bs
    SSL.shutdown sslsocket Unidirectional

    return $ splitBS bs

-- | Opens an APNS connection, runs the supplied action with the SSL socket, and closes the connection.
--
-- Example usage:
--
-- > withAPNSSocket cfg $ \sslsocket -> sendApplePushMessage msg sslsocket
--
-- Note that in practice you should keep the SSL socket open and re-use it. From the <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html#//apple_ref/doc/uid/TP40008194-CH11-SW1 Apple documentation>:
-- \"APNs treats rapid connection and disconnection as a denial-of-service attack.\"
withAPNSSocket :: APNSConfig -> (SSL -> IO ()) -> IO ()
withAPNSSocket (APNSConfig server keyfile certfile) f = withOpenSSL $ do
  -- Prepare SSL context
  ssl <- context
  contextSetPrivateKeyFile ssl keyfile
  contextSetCertificateFile ssl certfile
  contextSetDefaultCiphers ssl
  contextSetVerificationMode ssl SSL.VerifyNone
  -- Open socket
  proto <- (getProtocolNumber "tcp")
  he <- getHostByName server
  withSocketSafe proto $ \sock -> do
    Network.Socket.connect sock (SockAddrInet 2195 (hostAddress he))
    -- Promote socket to SSL stream
    sslsocket <- connection ssl sock
    SSL.connect sslsocket  -- Handshake
    -- Use socket
    f sslsocket
    -- Close gracefully
    SSL.shutdown sslsocket Unidirectional

-- | Send a message through the SSL socket
sendApplePushMessage :: SSL -> ApplePushMessage -> IO ()
sendApplePushMessage sslsocket m =
  let lpdu = runPut $ buildPDU m
      pdu = B.concat $ BL.toChunks lpdu
  in SSL.write sslsocket pdu

tokenLength :: Num a => a
tokenLength = 32

maxPayloadLength :: Num a => a
maxPayloadLength = 2048

buildPDU :: ApplePushMessage -> Put
buildPDU (ApplePushMessage token payload expiry)
  | (B.length token) /= tokenLength = fail "Invalid token"
  | (BL.length payload >= maxPayloadLength) = fail "Payload too large"
  | otherwise = do
    putWord8 1
    putWord32be 1
    putWord32be expiry
    putWord16be ((convert $ B.length token) :: Word16)
    putByteString token
    putWord16be ((convert $ BL.length payload) :: Word16)
    putLazyByteString payload

splitBS :: B.ByteString -> [B.ByteString]
splitBS xs =
  let xs1 = B.drop 6 xs
      token = B.take 32 xs1
      nexst = B.drop 32 xs1
  in if B.null token then [] else token:(splitBS nexst)

getExpiryTime :: IO (Word32)
getExpiryTime = do
  pt <- getPOSIXTime
  -- One hour expiry time
  return ( (round pt + 60*60):: Word32)

{-# DEPRECATED pushMess "Use withAPNSSocket and sendApplePushMessage instead." #-}
pushMess :: String -> FilePath -> FilePath -> BL.ByteString -> [B.ByteString] -> IO ()
pushMess server keyfile certfile payload tokens = withAPNSSocket (APNSConfig server keyfile certfile) $ \sslsocket -> do
  expiration <- getExpiryTime
  let sendPDU token = sendApplePushMessage sslsocket $ ApplePushMessage token payload expiration
  sequence_ $ map sendPDU tokens
