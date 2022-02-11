--------------------------------------------------------------------------------

-- | Data types for sending to and receiving from the frontend.
--
module Decks.Server.Types where

import           Data.Aeson
import           Data.Text                      ( Text )
import           Decks.Document                 ( SrcRange )
import           Decks.Utils                    ( Html )
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

data Notification = Notification
    { notifType   :: NotificationType
    , notifSrc    :: SrcRange
    , notifNewVal :: Text
    }

data NotificationType = NotifTextChanged
    deriving Show

--------------------------------------------------------------------------------
