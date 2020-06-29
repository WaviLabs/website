{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Contact.Template where

import Lucid
import Page.Contact.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Basic (showT, flipFlop)
import Html (dataNetifly_, i'_)

import qualified Bulma.Layout.DoubleColumns
import qualified Bulma.Basic

import Layout

import qualified Data.Text as Text


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = Bulma.Basic.container $ do
    br_ []
    br_ []
    br_ []
    br_ []
    br_ []
    br_ []
    Bulma.Layout.DoubleColumns.render doubleColumnConfig
    br_ []
    br_ []
  where
    doubleColumnConfig =
        Bulma.Layout.DoubleColumns.Options
            False
            False
            False
            (Bulma.Basic.container $ surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues)
            contactInfo_

    contactInfo_ =
        Bulma.Basic.container $
            section_ [class_ "section has-text-centered"] $ do
                h1_ [class_ "title"] "Contact Information"
                ul_ $ do
                    li_ $ do
                        strong_ "Name:"
                        " Wavi Labs LLC"
                    li_ $ do
                        strong_ "Location:"
                        " Los Angeles, CA"
                    li_ $ do
                        strong_ "Github:"
                        " Wavi Labs LLC"
                    li_ $ do
                        strong_ "Phone:"
                        " (310) 910-3199"
                    li_ $ do
                        strong_ "Email:"
                        " wavi.labs@gmail.com"
