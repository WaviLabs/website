{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Contact.Template where

import Lucid
import Page.Contact.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Basic (showT, flipFlop)
import Html (container_, dataNetifly_, i'_)
import Layout

import qualified Data.Text as T


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    br_ []
    br_ []
    br_ []
    br_ []
    br_ []
    br_ []
    halfHalf_ $
        HalfHalf
            False
            False
            (container_ $ surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues)
            contactInfo_
    br_ []
    br_ []
  where
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
