{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Android (pushMess) where 

import Network.HTTP.Conduit
import Data.Conduit
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Lazy.Char8 (unpack)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

{-
Resend with exponential fallback
-}

pushMess :: BS.ByteString -> LBS.ByteString -> IO ()
pushMess apikey payload =
   runResourceT $ do
     liftIO $ print payload
     manager <- liftIO $ newManager def
     req <- liftIO $ parseUrl "https://android.googleapis.com/gcm/send"
     res <- httpLbs req {method = "POST",
                         requestHeaders = [("Authorization", BS.append "key=" apikey),
                                           ("Content-Type", "application/json")],
                         requestBody = RequestBodyLBS payload} manager
     liftIO $ do
       --print $ unpack payload
       --print $ responseStatus res
       --print $ responseBody res
       mapM_ print $ responseHeaders res

