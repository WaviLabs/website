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
import Html (container_, dataNetifly_, i'_)

import qualified Data.Text as T


-- | The first element on the home page
hero_ :: Applicative m
      => Monad m
      => Text
      -> HtmlT m ()
hero_ content =
    section_ [class_ "hero is-fullheight"] $
        div_ [class_ "hero-body"] $
            container_ $
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
halfSection_ HalfSection{..} =
    section_ [class_ "section"] $ do
        h1_ [class_ "title"] $ toHtml halfTitle
        p_ $ toHtml halfPara
        div_ [class_ "level"] $ do
            div_ [class_ "level-left"] ""
            div_ [class_ "level-right"] $
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

-- | TODO: Write function for Project to HTML

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

render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    hero_ heroContent
    mapM_
        halfHalf_
        (map
            (uncurrry halfHalfZipper)
            (tripZip (flipFlop True) halfSections halfImages)
        )
    container_ $ do
        section_ [class_ "section has-text-centered"] $
            h1_ [class_ "title"] "Why Wavi Labs?"
        triColumn_
            (colSection_ col1)
            (colSection_ col2)
            (colSection_ col3)
        div_ [class_ "level"] $ do
            div_ [class_ "level-left"] ""
            div_ [class_ "level-right"] $
                a_ [href_ "#", class_ "button hvr-icon-forward is-medium"] $ do
                    "Read Our Blog"
                    i'_ [class_ "fas fa-chevron-right hvr-icon"]
        container_ $ do
            section_ [class_ "section has-text-centered"] $
                h1_ [class_ "title"] "Catch The Wave"
            div_ [class_ "columns is-centered"] $
                div_ [class_ "column is-three-fifths"] $
                    div_ [class_ "section"] $
                        surveyForm_ $ SurveyForm surveyName surveyOptNames surveyOptValues
  where
    halfHalfZipper :: Applicative m
                   => Monad m
                   => Bool
                   -> HalfSection
                   -> Maybe Text
                   -> HalfHalf m ()
    halfHalfZipper isRev sec mbImg =
        HalfHalf
            { isReverse   = isRev
            , isVCentered = True
            , htmlA       = halfSection_ sec
            , htmlB       =
                case mbImg of
                    Nothing  -> carousel_ ["", "", ""]
                    Just img -> halfImage_ img
            }
