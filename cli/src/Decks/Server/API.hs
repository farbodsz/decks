--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines the API for the backend server.
--
module Decks.Server.API where

import           Decks.Server.Types             ( Presentation )
import           Servant

--------------------------------------------------------------------------------

type DecksAPI
    = "decks"
    :> Get '[JSON] Presentation

--------------------------------------------------------------------------------
