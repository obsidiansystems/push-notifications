{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PhonePush.Android (pushMess, sendAndroidPushMessage) where 

import Data.Aeson (encode)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (ResponseHeaders)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import PhonePush.Android.Payload

sendAndroidPushMessage :: Manager
                       -> BS.ByteString
                       -> FcmPayload
                       -> IO (Response LBS.ByteString)
sendAndroidPushMessage mgr key p = do
  target <- parseUrlThrow "https://fcm.googleapis.com/fcm/send"
  let req = target
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", BS.append "key=" $ C8.takeWhile (/='\n') key)
            , ("Content-Type", "application/json")
            ]
        , requestBody = RequestBodyLBS $ encode p
        }
  httpLbs req mgr

{-# DEPRECATED pushMess "Use sendAndroidPushMessage instead." #-}
pushMess :: BS.ByteString -> LBS.ByteString -> IO ResponseHeaders
pushMess apikey payload = do
  mgr <- newManager tlsManagerSettings
  req <- parseUrlThrow "https://android.googleapis.com/gcm/send"
  res <- httpLbs req {method = "POST",
                     requestHeaders = [("Authorization", BS.append "key=" apikey),
                                       ("Content-Type", "application/json")],
                     requestBody = RequestBodyLBS payload} mgr
  return $ responseHeaders res
