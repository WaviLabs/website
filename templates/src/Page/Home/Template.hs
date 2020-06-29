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

import qualified Bulma.Basic
import qualified Bulma.Layout.DoubleColumns
import qualified Bulma.Layout.Level
import qualified Bulma.Layout.TripleColumns
import qualified Data.Text as Text
import qualified Vanilla.Layout.Carousel


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    -- Hero
    hero_ heroContent

    -- List of double columns
    Bulma.Basic.container $
        mapM
            (\x -> do
                Bulma.Layout.DoubleColumns.render x
                -- TODO: Make repeating break function
                mapM_ (\_ -> br_ []) [1..2]
            )
            (map
                (uncurrry doubleColumnOptsZip)
                (tripZip (flipFlop True) halfSections halfImages)
            )

    -- Triple column header
    Bulma.Basic.container $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Why Wavi Labs?"

    -- Triple column
    Bulma.Basic.container $ do
        Bulma.Layout.TripleColumns.render tripleColumnOpts
        -- Level after triple column
        Bulma.Layout.Level.render levelOpts

    -- Header for form and form
    Bulma.Basic.container $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Catch The Wave"
        div_ [class_ "columns is-centered"] $
            div_ [class_ "column is-three-fifths"] $
                Bulma.Basic.container $
                    surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues

    br_ []
    br_ []
  where
    -- The first element on the home page
    hero_ :: Applicative m
          => Monad m
          => Text
          -> HtmlT m ()
    hero_ content =
        section_ [class_ "hero is-fullheight"] $
            div_ [class_ "hero-body"] $
                Bulma.Basic.container $
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
        Bulma.Layout.Level.render levelOpts
      where
        levelOpts =
            Bulma.Layout.Level.Options
                ("") $
                a_ [href_ "#", class_ "button is-info is-outlined hvr-icon-forward is-medium"] $ do
                    toHtml button
                    space
                    i_ [class_ "fas fa-chevron-right hvr-icon"] ""

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
        processTitle = mconcat
                     . intersperse (br_ [])
                     . map toHtml
                     . Text.words

    doubleColumnOptsZip :: Applicative m
                        => Monad m
                        => Bool
                        -> HalfSection
                        -> Maybe Text
                        -> Bulma.Layout.DoubleColumns.Options m ()
    doubleColumnOptsZip isRev sec mbImg =
        Bulma.Layout.DoubleColumns.Options
            (False)
            (True)
            (isRev)
            (halfSection_ sec) $
            case mbImg of
                Nothing  -> Bulma.Basic.container $ Vanilla.Layout.Carousel.render $ Vanilla.Layout.Carousel.Options ["", "", "", "", ""]
                Just img -> halfImage_ img

    tripleColumnOpts :: Applicative m => Monad m => Bulma.Layout.TripleColumns.Options m ()
    tripleColumnOpts =
        Bulma.Layout.TripleColumns.Options
            (True)
            (False)
            (colSection_ col1)
            (colSection_ col2)
            (colSection_ col3)

    levelOpts :: Applicative m => Monad m => Bulma.Layout.Level.Options m ()
    levelOpts =
        Bulma.Layout.Level.Options
            "" $ do
            a_ [href_ "#", class_ "button is-info is-outlined hvr-icon-forward is-medium"] $ do
                "Read Our Blog"
                space
                i_ [class_ "fas fa-chevron-right hvr-icon"] ""

space :: Applicative m => Monad m => HtmlT m ()
space = "               "