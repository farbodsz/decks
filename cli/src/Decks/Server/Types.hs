--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}

-- | Data types for sending to and receiving from the frontend.
--
module Decks.Server.Types where

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Decks.Document                 ( SrcRange )
import           Decks.Utils                    ( Html )
import           GHC.Generics                   ( Generic )
import           Network.WebSockets

--------------------------------------------------------------------------------

newtype Presentation = Presentation { unPresentation :: Maybe Html }
    deriving (ToJSON)

examplePresentation :: Presentation
examplePresentation =
    Presentation
        $ Just
              "<html>\
                \<head></head>\
                \<body>Example Presentation</body>\
                \</html>"

instance WebSocketsData Presentation where
    fromDataMessage (Text bs _) = fromLazyByteString bs
    fromDataMessage (Binary bs) = fromLazyByteString bs
    fromLazyByteString = Presentation . decode
    toLazyByteString   = encode

--------------------------------------------------------------------------------

-- | Wrapper around 'Notification'.
--
-- When a notification is received from the frontend, there may be an error in
-- decoding it. This type encapsulates that possibility.
newtype ReceivedNotification = ReceivedNotification (Either String Notification)
    deriving (ToJSON)

instance WebSocketsData ReceivedNotification where
    fromDataMessage (Text bs _) = fromLazyByteString bs
    fromDataMessage (Binary bs) = fromLazyByteString bs
    fromLazyByteString = ReceivedNotification . eitherDecode
    toLazyByteString   = encode

data Notification = Notification
    { notifType   :: NotificationType
    , notifSrc    :: SrcRange
    , notifNewVal :: Text
    }
    deriving (Generic, Show)

instance FromJSON Notification
instance ToJSON Notification

data NotificationType = NotifTextChanged
    deriving (Generic, Show)

instance FromJSON NotificationType where
    parseJSON = withText "NotificationType" $ \case
        "NotifTextChanged" -> pure NotifTextChanged
        txt -> fail $ "Invalid NotificationType: " ++ T.unpack txt

instance ToJSON NotificationType

--------------------------------------------------------------------------------
