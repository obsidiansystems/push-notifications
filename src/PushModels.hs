{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module PushModels where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)

data AndroidData = AndroidData {_title :: BS.ByteString,
                                _msg :: BS.ByteString,
                                _type :: BS.ByteString
                               }

data IOSData = IOSData {_alert :: BS.ByteString,
                        _badge :: Integer}

data TopMessage = TopMessage {__data :: AndroidData, __aps :: IOSData }

data GCMPayload = GCMPayload {_registration_ids :: [BS.ByteString],
                              _data :: TopMessage,
                              _delay_while_idle :: Bool,
                              _time_to_live :: Integer,
                              _collapse_key :: String}

defaultPayload :: GCMPayload
defaultPayload = GCMPayload [] (TopMessage (AndroidData "" "" "") (IOSData "" 0)) False 1000 "Aitellu"

$(deriveJSON (drop 1) ''GCMPayload)

api_key :: BS.ByteString
api_key = "AIzaSyCZKcE9n0YYnGHPgxLrhatYwxEnCmSJG1Y"

testreg :: BS.ByteString
testreg = "APA91bEaUAouGPuKwG2nBOEBLv9ijjrqJ6Qbe4ETTKUtlEJd6SIVLFhxDrGyAjtZVJLTx4PxfSusDQIuYop5_TmaRNT89vWRi8tauMTxBHzGjaN_hqRukDzcesw7n1M6zANEP9vwnyYTierKgEC6uVzPp7Q6eCJmiw"

testToken :: BS.ByteString
testToken = "2ddc944d3d2e22c1084f854efa9fda37df23e74ee0eb1bae1b0b57bcafcd59ba"

$(deriveJSON (drop 2) ''TopMessage)
$(deriveJSON (drop 1) ''AndroidData)
$(deriveJSON (drop 1) ''IOSData)

testData :: TopMessage
testData = TopMessage (AndroidData "Lambda Cat" "...sez hello" "notice")
           (IOSData "The alert" 0)

testMess :: LBS.ByteString
testMess = encode testData