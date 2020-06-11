{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Services.Template where

import Basic
import Lucid
import Layout
import Page.Home.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Html (dataNetifly_, i'_)

import qualified Data.Text as T

import qualified Layout.Timeline as Timeline

-- import qualified Bulma.Component.Level as Level
-- import qualified Bulma.Layout.DoubleColumn as DoubleColumn
-- import qualified Bulma.Layout.TripleColumn as TripleColumn
import qualified Bulma.Wrapper as Wrapper


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    Wrapper.container_ $ do
       section_ [class_ "section has-text-centered"] $
              h1_ [class_ "title"] "Services"
              p_ "At Wavi Labs we provide a variety of resources ranging from SEO consulting to staff augmentation for your software needs."

    Timeline.render timelineOpts
  where
    timelineOpts = Timeline.Options inners