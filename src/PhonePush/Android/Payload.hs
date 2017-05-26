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
  , _fcmPayload_notification :: Maybe FcmNotification
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
    , _fcmPayload_notification = Nothing
    }

data FcmNotification = FcmNotification
  { _fcmNotification_title :: Maybe Text
  , _fcmNotification_body :: Maybe Text
  , _fcmNotification_androidChannelId :: Maybe Text
  , _fcmNotification_icon :: Maybe Text
  , _fcmNotification_sound :: Maybe Text
  , _fcmNotification_tag :: Maybe Text
  , _fcmNotification_color :: Maybe Text
  , _fcmNotification_clickAction :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

instance Default FcmNotification where
  def = FcmNotification
    { _fcmNotification_title = Nothing
    , _fcmNotification_body = Nothing
    , _fcmNotification_androidChannelId = Nothing
    , _fcmNotification_icon = Nothing
    , _fcmNotification_sound = Nothing
    , _fcmNotification_tag = Nothing
    , _fcmNotification_color = Nothing
    , _fcmNotification_clickAction = Nothing
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . drop 17, omitNothingFields = True}) ''FcmNotification)
$(deriveJSON (defaultOptions { fieldLabelModifier = toQuietSnake . fromHumps . drop 12, omitNothingFields = True}) ''FcmPayload)
