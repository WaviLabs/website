{-# LANGUAGE DeriveGeneric #-}

module Page.Services.Types where

import Dhall

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Bulma.Component.Image
import qualified Bulma.Component.SectionA
import qualified Bulma.Component.SectionB
import qualified Vanilla.Layout.Timeline


data Config = Config
    { image1Options :: Bulma.Component.Image.Options
    , image2Options :: Bulma.Component.Image.Options
    , image3Options :: Bulma.Component.Image.Options
    , sectionA1Options :: Bulma.Component.SectionA.Options
    , sectionA2Options :: Bulma.Component.SectionA.Options
    , sectionA3Options :: Bulma.Component.SectionA.Options
    , button1Text :: Text
    , button2Text :: Text
    , button3Text :: Text
    , button4Text :: Text
    , timelineSections :: [Bulma.Component.SectionA.Options]
    , sectionB1Options :: Bulma.Component.SectionB.Options
    , sectionB2Options :: Bulma.Component.SectionB.Options
    , sectionB3Options :: Bulma.Component.SectionB.Options
    } deriving (Generic, Show)

instance FromDhall Config
instance ToDhall Config
