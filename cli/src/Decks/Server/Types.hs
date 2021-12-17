--------------------------------------------------------------------------------

-- | Data types for sending to and receiving from the frontend.
--
module Decks.Server.Types where

import           Decks.Utils                    ( Html )

import           Data.Aeson                     ( ToJSON )

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

--------------------------------------------------------------------------------
