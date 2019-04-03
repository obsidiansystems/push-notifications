{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.PushNotification.Android where 

import Data.Aeson (encode)
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Network.PushNotification.Android.Payload

-- | Sends a POST request to the firebase service containing the push notification data
sendAndroidPushMessage :: Manager
                       -> BS.ByteString -- ^ The server key (see README.md)
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
