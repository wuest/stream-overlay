{-# LANGUAGE DeriveGeneric #-}

-- | Provides Json types for communication with the Elm frontend
module Server.Json where

import Prelude
import Data.Text    ( Text )
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

data Message = Message
    { userHandle :: Text
    , eventType  :: Text
    , amount     :: Float
    , message    :: Text
    } deriving ( Show, Generic )
instance FromJSON Message
instance ToJSON Message
