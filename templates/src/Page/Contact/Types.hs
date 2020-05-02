{-# LANGUAGE DeriveGeneric #-}

module Page.Contact.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data Config = Config
    { surveyName      :: Text
    , surveyOptNames  :: [Text]
    , surveyOptValues :: [Text]
    }
    deriving (Generic, Show)

instance FromDhall Config
