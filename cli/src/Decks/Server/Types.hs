--------------------------------------------------------------------------------

-- | Data types for sending to and receiving from the frontend.
--
module Decks.Server.Types where

import           Data.Aeson                     ( ToJSON )
import           Data.Text                      ( Text )
import           Decks.Document                 ( SrcRange )
import           Decks.Utils                    ( Html )

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

data Notification = Notification
    { notifType   :: NotificationType
    , notifSrc    :: SrcRange
    , notifNewVal :: Text
    }

data NotificationType = NotifTextChanged
    deriving Show

--------------------------------------------------------------------------------
