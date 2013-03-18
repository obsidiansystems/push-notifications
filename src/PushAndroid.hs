{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Network.HTTP.Conduit
import Data.Conduit
import Data.Aeson.TH (deriveJSON)
import Data.Aeson (encode)
import Control.Monad.IO.Class (liftIO)
import Control.Exception

import Data.ByteString.Lazy.Char8 (unpack)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

pushRequest :: BS.ByteString -> TopAndroid -> [BS.ByteString] -> IO ()
pushRequest apikey regids =
  let payload = encode defaultPayload {_data = jsonmess,
                                       _registration_ids = regids}
  in
   runResourceT $ do
     liftIO $ print payload
     manager <- liftIO $ newManager def
     req <- liftIO $ parseUrl "https://android.googleapis.com/gcm/send"
     res <- httpLbs req {method = "POST",
                         requestHeaders = [("Authorization", BS.append "key=" apikey),
                                           ("Content-Type", "application/json")],
                         requestBody = RequestBodyLBS payload} manager
     liftIO $ do
       print $ unpack payload
       print $ responseStatus res
       print $ responseBody res
       mapM_ print $ responseHeaders res

