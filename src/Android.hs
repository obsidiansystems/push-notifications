{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Android (pushMess) where 

import Network.HTTP.Conduit (newManager, parseUrl, def, httpLbs, method, requestHeaders, requestBody, responseHeaders, RequestBody(RequestBodyLBS))
import Data.Conduit (runResourceT)
import Control.Monad.IO.Class (liftIO)
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
     manager <- liftIO $ newManager def
     req <- liftIO $ parseUrl "https://android.googleapis.com/gcm/send"
     res <- httpLbs req {method = "POST",
                         requestHeaders = [("Authorization", BS.append "key=" apikey),
                                           ("Content-Type", "application/json")],
                         requestBody = RequestBodyLBS payload} manager
     return $ responseHeaders res


