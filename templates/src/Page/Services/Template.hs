{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Services.Template where

import Basic
import Lucid
import Layout
import Page.Services.Types

import Data.List (intersperse)
-- import Data.Monoid (mconcat)
-- import Data.Text (Text)
-- import Html (dataNetifly_, i'_)

import qualified Bulma.Basic
import qualified Bulma.Component.Image (render)
import qualified Bulma.Component.SectionA (render)
import qualified Bulma.Component.SectionB (render)
import qualified Bulma.Layout.DoubleColumns
import qualified Bulma.Layout.Level
import qualified Bulma.Layout.TripleColumns
import qualified Data.Text as Text
import qualified Vanilla.Layout.Timeline


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    -- TODO: Add repeater function.
    br_ []
    br_ []
    br_ []

    let image1View = Bulma.Component.Image.render image1Options
        image2View = Bulma.Component.Image.render image2Options
        image3View = Bulma.Component.Image.render image3Options

        -- renderButton :: Applicative m
        --             => Monad m
        --             => Text.Text
        --             -> HtmlT m ()
        renderButton = \text ->
            a_ [href_ "#", class_ "button is-info is-outlined hvr-icon-forward is-medium"] $ do
                toHtml text
                i_ [class_ "fas fa-chevron-right hvr-icon"] ""

        button1View = renderButton button1Text
        button2View = renderButton button2Text
        button3View = renderButton button3Text

        level1Options = Bulma.Layout.Level.Options "" button1View
        level2Options = Bulma.Layout.Level.Options "" button2View
        level3Options = Bulma.Layout.Level.Options "" button3View

        sectionA1View = do
            Bulma.Component.SectionA.render sectionA1Options
            Bulma.Layout.Level.render level1Options
        sectionA2View = do
            Bulma.Component.SectionA.render sectionA2Options
            Bulma.Layout.Level.render level2Options
        sectionA3View = do
            Bulma.Component.SectionA.render sectionA3Options
            Bulma.Layout.Level.render level3Options

        doubleColumn1Options =
            Bulma.Layout.DoubleColumns.Options
                False
                True
                False
                image1View
                sectionA1View

        doubleColumn2Options =
            Bulma.Layout.DoubleColumns.Options
                False
                True
                True
                image2View
                sectionA2View

        doubleColumn3Options =
            Bulma.Layout.DoubleColumns.Options
                False
                True
                False
                image3View
                sectionA3View

    Bulma.Basic.container $ do
        Bulma.Layout.DoubleColumns.render doubleColumn1Options
        Bulma.Layout.DoubleColumns.render doubleColumn2Options
        Bulma.Layout.DoubleColumns.render doubleColumn3Options

    Bulma.Basic.container $ do
        Bulma.Basic.sectionHasTextCentered $
            Bulma.Basic.title Bulma.Basic.TitleSizeOne $
                "Our Process"

        let timelineOptions = Vanilla.Layout.Timeline.Options $
                map Bulma.Component.SectionA.render timelineSections

        Vanilla.Layout.Timeline.render timelineOptions

    Bulma.Basic.container $ do
        Bulma.Basic.sectionHasTextCentered $
            Bulma.Basic.title Bulma.Basic.TitleSizeOne $
                "Technologies"

        let sectionB1View = Bulma.Component.SectionB.render sectionB1Options
            sectionB2View = Bulma.Component.SectionB.render sectionB2Options
            sectionB3View = Bulma.Component.SectionB.render sectionB3Options

            tripleColumnsOptions =
                Bulma.Layout.TripleColumns.Options
                    True
                    False
                    sectionB1View
                    sectionB2View
                    sectionB3View

            button4View = renderButton button4Text

            level4Options = Bulma.Layout.Level.Options "" button4View

        Bulma.Layout.TripleColumns.render tripleColumnsOptions
        Bulma.Layout.Level.render level4Options

    br_ []
    br_ []
{-
render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render Config{..} = do
    br_ []
    br_ []
    br_ []

    -- List of double columns
    Wrapper.container_ $
        mapM
            (\x -> do
                DoubleColumn.render x
                -- TODO: Make repeating break function
                mapM_ (\_ -> br_ []) [1..2]
            )
            (map
                (uncurrry doubleColumnOptsZip)
                (tripZip (flipFlop True) halfSections halfImages)
            )

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
        -- Try taking container out...? Seems weird to have for image
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

    halfImage_ :: Applicative m
               => Monad m
               => Text
               -> HtmlT m ()
    halfImage_ src =
        div_ [class_ "column"] $
            img_ [src_ src]

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
                a_ [href_ "#", class_ "button is-info is-outlined hvr-icon-forward is-medium"] $ do
                    toHtml button
                    space
                    i_ [class_ "fas fa-chevron-right hvr-icon"] ""

    doubleColumnOptsZip :: Applicative m
                        => Monad m
                        => Bool
                        -> HalfSection
                        -> Text
                        -> DoubleColumn.Options m ()
    doubleColumnOptsZip isRev sec img =
        DoubleColumn.Options
            (False)
            (True)
            (isRev)
            (halfSection_ sec)
            (halfImage_ img)

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
-}
