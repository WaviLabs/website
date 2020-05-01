{-# LANGUAGE DeriveGeneric #-}

module Base.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data FooterLink = FooterLink
    { name :: Text
    , link :: Text
    } deriving (Generic, Show)

instance FromDhall FooterLink

data FooterColumn = FooterColumn
    { title :: Text
    , links :: [FooterLink]
    } deriving (Generic, Show)

instance FromDhall FooterColumn

data Footer = Footer
    { col1 :: FooterColumn
    , col2 :: FooterColumn
    , col3 :: FooterColumn
    , col4 :: FooterColumn
    } deriving (Generic, Show)

instance FromDhall Footer

data Config = Config
    { footerConfig :: Footer
    } deriving (Generic, Show)

instance FromDhall Config
