{-# LANGUAGE DeriveGeneric #-}

module Page.Blog.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data Config = Config
    { posts :: [Text]
    } deriving (Generic, Show)

instance FromDhall Config
