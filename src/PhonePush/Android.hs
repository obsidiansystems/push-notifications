{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module PhonePush.Android (pushMess) where 

import Network.HTTP.Conduit (newManager, parseUrlThrow, httpLbs, method, requestHeaders, requestBody, responseHeaders, RequestBody(RequestBodyLBS), tlsManagerSettings)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Types.Header (ResponseHeaders)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

{-
Resend with exponential fallback
-}

pushMess :: BS.ByteString -> LBS.ByteString -> IO ResponseHeaders
pushMess apikey payload =
   runResourceT $ do
     liftIO $ print payload
     manager <- liftIO $ newManager tlsManagerSettings
     req <- liftIO $ parseUrlThrow "https://android.googleapis.com/gcm/send"
     res <- httpLbs req {method = "POST",
                         requestHeaders = [("Authorization", BS.append "key=" apikey),
                                           ("Content-Type", "application/json")],
                         requestBody = RequestBodyLBS payload} manager
     return $ responseHeaders res


