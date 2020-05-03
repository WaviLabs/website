{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Home.Template where

import Basic
import Lucid
import Layout
import Page.Home.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Html (dataNetifly_, i'_)

import qualified Data.Text as T

import qualified Layout.Carousel as Carousel

import qualified Bulma.Component.Level as Level
import qualified Bulma.Layout.DoubleColumn as DoubleColumn
import qualified Bulma.Layout.TripleColumn as TripleColumn
import qualified Bulma.Wrapper as Wrapper


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    -- | Hero
    hero_ heroContent

    -- | List of double columns
    Wrapper.container_ $
        mapM_
            DoubleColumn.render
            (map
                (uncurrry doubleColumnOptsZip)
                (tripZip (flipFlop True) halfSections halfImages)
            )

    -- | Triple column header
    Wrapper.container_ $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Why Wavi Labs?"

    -- | Triple column
    Wrapper.container_ $ do
        TripleColumn.render tripleColumnOpts
        -- | Level after triple column
        Level.render levelOpts

    -- | Header for form and form
    Wrapper.container_ $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Catch The Wave"
        div_ [class_ "columns is-centered"] $
            div_ [class_ "column is-three-fifths"] $
                div_ [class_ "section"] $
                    surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues
  where
    -- | The first element on the home page
    hero_ :: Applicative m
          => Monad m
          => Text
          -> HtmlT m ()
    hero_ content =
        section_ [class_ "hero is-fullheight"] $
            div_ [class_ "hero-body"] $
                Wrapper.container_ $
                    div_ [class_ "columns"] $ do
                        div_ [class_ "column"] ""
                        div_ [class_ "column is-10"] $
                            div_ [class_ "box"] $
                                section_ [class_ "section has-text-centered"] $ do
                                    h2_ [class_"title"] $ toHtml content
                                    a_ [class_ "button is-primary is-rounded is-medium hvr-grow", href_ "#"] "Catch The Wave"
                        div_ [class_ "column"] ""

    halfSection_ :: Applicative m
                 => Monad m
                 => HalfSection
                 -> HtmlT m ()
    halfSection_ HalfSection{..} = do
        section_ [class_ "section"] $ do
            h1_ [class_ "title"] $ toHtml halfTitle
            p_ $ toHtml halfPara
        Level.render levelOpts
      where
        levelOpts =
            Level.Options
                ("") $
                a_ [href_ "#", class_ "button hvr-icon-forward is-medium"] $ do
                    toHtml button
                    i'_ [class_ "fas fa-chevron-right hvr-icon"]

    halfImage_ :: Applicative m
                => Monad m
                => Text
                -> HtmlT m ()
    halfImage_ src =
        div_ [class_ "column"] $
            img_ [src_ src]

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
        processTitle = mconcat . intersperse (br_ []) . map toHtml . T.words

    doubleColumnOptsZip :: Applicative m
                        => Monad m
                        => Bool
                        -> HalfSection
                        -> Maybe Text
                        -> DoubleColumn.Options m ()
    doubleColumnOptsZip isRev sec mbImg =
        DoubleColumn.Options
            (True)
            (isRev)
            (halfSection_ sec) $
            case mbImg of
                Nothing  -> Wrapper.container_ $ Carousel.render $ Carousel.Options ["", "", ""]
                Just img -> halfImage_ img

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
            "" $
            a_ [href_ "#", class_ "button hvr-icon-forward is-medium"] $ do
                "Read Our Blog"
                i'_ [class_ "fas fa-chevron-right hvr-icon"]
