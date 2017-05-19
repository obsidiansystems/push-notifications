{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: PhonePush.Android.Payload
--
-- Data type and JSON instances for Firebase Cloud Messaging payloads
-- For more information, please see <https://firebase.google.com/docs/cloud-messaging/http-server-ref Google's documentation>.

module PhonePush.Android.Payload where

import Data.Aeson.TH
import Data.Default
import Data.Text (Text)
import Text.Casing

data FcmPayload = FcmPayload
  { _fcmPayload_to :: Maybe Text
  , _fcmPayload_registrationIds :: Maybe [Text]
  , _fcmPayload_collapseKey :: Maybe Text
  , _fcmPayload_priority :: Maybe Text
  , _fcmPayload_contentAvailable :: Maybe Bool
  , _fcmPayload_timeToLive :: Maybe Int
  , _fcmPayload_restrictedPackageName :: Maybe Text
  , _fcmPayload_dryRun :: Maybe Bool
  , _fcmPayload_data :: Maybe FcmData
  }
  deriving (Show, Read, Eq, Ord)

instance Default FcmPayload where
  def = FcmPayload
    { _fcmPayload_to = Nothing
    , _fcmPayload_registrationIds = Nothing
    , _fcmPayload_collapseKey = Nothing
    , _fcmPayload_priority = Nothing
    , _fcmPayload_contentAvailable = Nothing
    , _fcmPayload_timeToLive = Nothing 
    , _fcmPayload_restrictedPackageName = Nothing
    , _fcmPayload_dryRun = Nothing
    , _fcmPayload_data = Nothing
    }

data FcmData = FcmData
  { _fcmData_title :: Maybe Text
  , _fcmData_body :: Maybe Text
  , _fcmData_androidChannelId :: Maybe Text
  , _fcmData_icon :: Maybe Text
  , _fcmData_sound :: Maybe Text
  , _fcmData_tag :: Maybe Text
  , _fcmData_color :: Maybe Text
  , _fcmData_clickAction :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

instance Default FcmData where
  def = FcmData
    { _fcmData_title = Nothing
    , _fcmData_body = Nothing
    , _fcmData_androidChannelId = Nothing
    , _fcmData_icon = Nothing
    , _fcmData_sound = Nothing
    , _fcmData_tag = Nothing
    , _fcmData_color = Nothing
    , _fcmData_clickAction = Nothing
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . drop 9, omitNothingFields = True}) ''FcmData)
$(deriveJSON (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . drop 12, omitNothingFields = True}) ''FcmPayload)
