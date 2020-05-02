{-# LANGUAGE DeriveGeneric #-}

module Page.Home.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)


data HalfSection = HalfSection
    { halfTitle :: Text
    , halfPara  :: Text
    , button    :: Text
    } deriving (Generic, Show)

instance FromDhall HalfSection

data ColSection = ColSection
    { colTitle  :: Text
    , colPara   :: Text
    , imgSrc    :: Text
    , imgAlt    :: Text
    } deriving (Generic, Show)

instance FromDhall ColSection

data Project = Project
    { name :: Text
    , link :: Text
    } deriving (Generic, Show)

instance FromDhall Project

data SubjectOpt = SubjectOpt
    { optName :: Text
    , value   :: Text
    } deriving (Generic, Show)

instance FromDhall SubjectOpt

data Config = Config
    { heroContent     :: Text
    -- | Content title and content paragraph
    , halfSections    :: [HalfSection]
    -- | Image source links
    , halfImages      :: [Maybe Text]
    -- | Name of project and link to project
    , projects        :: [Project]
    , triColumnTitle  :: Text
    , col1            :: ColSection
    , col2            :: ColSection
    , col3            :: ColSection
    , surveyName      :: Text
    , surveyOptNames  :: [Text]
    , surveyOptValues :: [Text]
    } deriving (Generic, Show)

instance FromDhall Config
