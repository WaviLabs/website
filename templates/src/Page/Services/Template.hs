{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Services.Template where

import Basic
import Lucid
import Layout
import Page.Services.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Html (dataNetifly_, i'_)

import qualified Data.Text as T

import qualified Bulma.Wrapper as Wrapper

import qualified Layout.Timeline as Timeline


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    br_ []
    br_ []
    br_ []

    Wrapper.container_ $ do
       section_ [class_ "section has-text-centered"] $ do
              h1_ [class_ "title"] "Services"
              p_ "At Wavi Labs we provide a variety of resources ranging from SEO consulting to staff augmentation for your software needs."

    Timeline.render timelineOpts
  where
    timelineOpts :: Monad m => Timeline.Options m ()
    timelineOpts = Timeline.Options $ map renderEvent events

    renderEvent :: Monad m => Event -> HtmlT m ()
    renderEvent Event{..} = do
       h3_ $ toHtml title
       p_ $ toHtml content
