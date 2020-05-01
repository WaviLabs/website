{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Home.Template where

import Lucid
import Page.Home.Types

import Data.Text (Text)
import Basic (showT, flipFlop)
import Html (container_)

import qualified Data.Text as T


-- Helpers
i'_ :: Applicative m
    => Monad m
    => [Attribute]
    -> HtmlT m ()
i'_ attrs = term "i" attrs ""

-- | TODO: Move to Layout.hs
-- | The middle of the home page. Layout component.
data HalfHalf m a = HalfHalf
    { isReverse :: Bool
    , htmlA     :: HtmlT m a
    , htmlB     :: HtmlT m a
    }

halfHalf_ :: Applicative m
          => Monad m
          => HalfHalf m a
          -> HtmlT m a
halfHalf_ HalfHalf{..} =
    container_ $
        div_ [class_ $ "columns is-vcentered " <> reverse] $
            if isReverse
            then do
                div_ [class_ "column is-half"] htmlA
                div_ [class_ "column"] htmlB
            else do
                div_ [class_ "column is-half"] htmlB
                div_ [class_ "column"] htmlA
  where
    reverse =
        if isReverse
        then "reverse-columns"
        else ""

-- | The carousel on the page. This also a layout component.
carousel_ :: Applicative m
          => Monad m
          => [HtmlT m a]
          -> HtmlT m ()
carousel_ items =
    container_ $
        div_ [class_ "carousel"] $ do
            div_ [class_ "carousel-prev"] ""
            div_ [class_ "carousel-next"] ""
            ul_ [class_ "carousel-pagination"] $
                mapM_ (\_ -> li_ [class_ "carousel-bullet"] "") items
            ul_ [class_ "carousel-container"] $
                mapM_ (uncurry carouselItem_) (zip [1..] items)
  where
    -- | The HTML for each individual carousel item
    carouselItem_ :: Applicative m
                   => Monad m
                   => Int
                   -> HtmlT m a
                   -> HtmlT m a
    carouselItem_ n html =
        li_ [class_ "carousel-item"] $
            div_ [class_ $ "item-" <> showT n] html

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

triColumn_ :: Applicative m
           => Monad m
           => HtmlT m a
           -> HtmlT m a
           -> HtmlT m a
           -> HtmlT m a
triColumn_ htmlA htmlB htmlC = do
    div_ [class_ "columns is-centered"] $ do
        div_ [class_ "column is-one-thirds"] htmlA
        div_ [class_ "column"] htmlB
        div_ [class_ "column"] htmlC

colSection_ :: Applicative m
            => Monad m
            => ColSection
            -> HtmlT m ()
colSection_ ColSection{..} = do
    figure_ [class_ "image container is-128x128"] $
        img_ [src_ imgSrc, alt_ imgAlt]
    section_ [class_ "section has-text-centered"] $ do
        h1_ [class_ "title"] $ toHtml colTitle
        p_ $ toHtml colPara

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
  where
    halfHalfZipper :: Applicative m
                   => Monad m
                   => Bool
                   -> HalfSection
                   -> Maybe Text
                   -> HalfHalf m ()
    halfHalfZipper isRev sec mbImg =
        HalfHalf
            { isReverse = isRev
            , htmlA     = halfSection_ sec
            , htmlB     = case mbImg of
                Nothing  -> carousel_ ["", "", ""]
                Just img -> halfImage_ img
            }

    tripZip :: [a] -> [b] -> [c] -> [(a, b, c)]
    tripZip (x:xs) (y:ys) (z:zs) = (x,y,z) : tripZip xs ys zs
    tripZip [] [] [] = []
    tripZip _ _ _    = []

    uncurrry :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurrry f = \(x, y, z) -> f x y z
