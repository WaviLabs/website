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

import qualified Bulma.Layout.DoubleColumn as DoubleColumn
import Bulma.Wrapper as Wrapper

import Layout

import qualified Data.Text as T


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = container_ $ DoubleColumn.render doubleColumnConfig
  where
    doubleColumnConfig =
        DoubleColumn.Options
            False
            False
            (Wrapper.container_ $ surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues)
            contactInfo_

    contactInfo_ =
        container_ $
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
