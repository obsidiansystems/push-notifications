{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PhonePush.IOS.Payload where

import Data.Aeson.TH
import Data.Default
import Data.Text (Text)
import Text.Casing

data ApsPayload = ApsPayload
  { _apsPayload_aps :: Aps
  , _apsPayload_custom :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

instance Default ApsPayload where
  def = ApsPayload def Nothing

data Aps = Aps
  { _aps_alert :: Maybe ApsAlert
  -- ^ The alert content
  , _aps_badge :: Maybe Int
  -- ^ The unread count to display on the application icon
  , _aps_sound :: Maybe Text
  -- ^ The filename of the sound to play on notification 
  , _aps_contentAvailable :: Maybe Int
  -- ^ Used for <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CreatingtheNotificationPayload.html#//apple_ref/doc/uid/TP40008194-CH10-SW8 silent notifications>
  , _aps_category :: Maybe Text
  -- ^ The app-specific <https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/SupportingNotificationsinYourApp.html#//apple_ref/doc/uid/TP40008194-CH4-SW26 notification category>
  , _aps_threadId :: Maybe Text
  -- ^ The app-specific messaging thread identifier, used to group notifications
  }
  deriving (Show, Read, Eq, Ord)

instance Default Aps where
  def = Aps
    { _aps_alert = Nothing
    , _aps_badge = Nothing
    , _aps_sound = Nothing
    , _aps_contentAvailable = Nothing
    , _aps_category = Nothing
    , _aps_threadId = Nothing
    }

data ApsAlert = ApsAlert
  { _apsAlert_title :: Text
  , _apsAlert_body :: Text
  , _apsAlert_launchImage :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

instance Default ApsAlert where
  def = ApsAlert
    { _apsAlert_title = ""
    , _apsAlert_body = ""
    , _apsAlert_launchImage = Nothing
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = toKebab . fromHumps . drop 10, omitNothingFields = True }) ''ApsAlert)
$(deriveJSON (defaultOptions { fieldLabelModifier = toKebab . fromHumps . drop 5, omitNothingFields = True }) ''Aps)
$(deriveJSON (defaultOptions { fieldLabelModifier = toKebab . fromHumps . drop 12, omitNothingFields = True }) ''ApsPayload)
