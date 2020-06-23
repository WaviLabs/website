{-# LANGUAGE DeriveGeneric #-}

module Page.Services.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data Event = Event
    { title   :: Text
    , content :: Text
    } deriving (Generic, Show)

instance FromDhall Event

data Config = Config
    { servicesText :: Text
    , events :: [Event]
    } deriving (Generic, Show)

instance FromDhall Config
