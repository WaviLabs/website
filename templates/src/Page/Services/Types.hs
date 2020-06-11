{-# LANGUAGE DeriveGeneric #-}

module Page.Services.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data Info = Info
    { title :: Text
    , content :: Text
    } deriving (Generic, Show)

instance FromDhall Info

data Config = Config
    { panels :: [Info]
    } deriving (Generic, Show)

instance FromDhall Config
