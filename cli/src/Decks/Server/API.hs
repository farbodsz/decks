--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Defines the API for the backend server.
--
module Decks.Server.API where

import           Servant
import           Servant.API.WebSocket          ( WebSocket )

--------------------------------------------------------------------------------

type DecksAPI
  = "decks"
  :> WebSocket

decksAPI :: Proxy DecksAPI
decksAPI = Proxy

--------------------------------------------------------------------------------
