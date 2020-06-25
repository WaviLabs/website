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

import qualified Bulma.Component.Level as Level
import qualified Bulma.Wrapper as Wrapper
import qualified Bulma.Layout.TripleColumn as TripleColumn

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
              p_ $ toHtml servicesText

    Wrapper.container_ $ do
       section_ [class_ "has-text-centered"] $ do
              h1_ [class_ "title"] "Our Process"
       Timeline.render timelineOpts

    -- Triple column header
    Wrapper.container_ $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Technologies"

    -- Triple column
    Wrapper.container_ $ do
        TripleColumn.render tripleColumnOpts
        -- Level after triple column
        Level.render levelOpts

    br_ []
    br_ []
  where
    timelineOpts :: Monad m => Timeline.Options m ()
    timelineOpts = Timeline.Options $ map renderEvent events

    renderEvent :: Monad m => Event -> HtmlT m ()
    renderEvent Event{..} = do
       h1_ [class_ "title is-4"] $ toHtml title
       p_ $ toHtml content

    colSection_ :: Applicative m
                => Monad m
                => ColSection
                -> HtmlT m ()
    colSection_ ColSection{..} = do
        figure_ [class_ "image container is-128x128"] $
            img_ [src_ imgSrc, alt_ imgAlt]
        section_ [class_ "section has-text-centered"] $ do
            h1_ [class_ "title is-4"] $ processTitle colTitle
            p_ $ toHtml colPara
      where
        processTitle :: Monad m
                     => Applicative m
                     => Text
                     -> HtmlT m ()
        processTitle = mconcat
                     . intersperse (br_ [])
                     . map toHtml
                     . T.words

    tripleColumnOpts :: Applicative m => Monad m => TripleColumn.Options m ()
    tripleColumnOpts =
       TripleColumn.Options
              (True)
              (False)
              (colSection_ col1)
              (colSection_ col2)
              (colSection_ col3)

    levelOpts :: Applicative m => Monad m => Level.Options m ()
    levelOpts =
       Level.Options
              "" $ do
              a_ [href_ "#", class_ "button is-info is-outlined hvr-icon-forward is-medium"] $ do
                     "Read Our Blog"
                     space
                     i_ [class_ "fas fa-chevron-right hvr-icon"] ""

space :: Applicative m => Monad m => HtmlT m ()
space = "               "
