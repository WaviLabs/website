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

data ColSection = ColSection
    { colTitle  :: Text
    , colPara   :: Text
    , imgSrc    :: Text
    , imgAlt    :: Text
    } deriving (Generic, Show)

instance FromDhall ColSection

data Config = Config
    { servicesText :: Text
    , events :: [Event]
    , col1 :: ColSection
    , col2 :: ColSection
    , col3 :: ColSection
    } deriving (Generic, Show)

instance FromDhall Config
