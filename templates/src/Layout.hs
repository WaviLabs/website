{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Layout where

import Basic
import Data.Text hiding (zip)
import Lucid
import Html


-- | TODO: Move to Layout.hs
-- | The middle of the home page. Layout component.
data HalfHalf m a = HalfHalf
    { isReverse   :: Bool
    , isVCentered :: Bool
    , htmlA       :: HtmlT m a
    , htmlB       :: HtmlT m a
    }

halfHalf_ :: Applicative m
          => Monad m
          => HalfHalf m a
          -> HtmlT m a
halfHalf_ HalfHalf{..} =
    container_ $
        div_ [class_ $ "columns " <> vCenter <> " " <> reverse] $
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
    vCenter =
        if isVCentered
        then "is-vcentered"
        else ""

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

-- TODO: Doesn't really belong to layout. More component.
data SurveyForm = SurveyForm
    { name      :: Text
    , optNames  :: [Text]
    , optValues :: [Text]
    }

surveyForm_ :: Applicative m
            => Monad m
            => SurveyForm
            -> HtmlT m ()
surveyForm_ SurveyForm{..} =
        form_ [name_ name, method_ "POST", dataNetifly_ "true"] $ do
            div_ [class_ "field"] $ do
                label_ [class_ "label"] "Name"
                div_ [class_ "control"] $
                    input_ [class_ "input is-rounded", type_ "text", placeholder_ "Your Name", name_ "name"]
            div_ [class_ "field"] $ do
                label_ [class_ "label"] "Website"
                div_ [class_ "control has-icons-left has-icons-right"] $ do
                    input_ [class_ "input is-rounded", type_ "text", placeholder_ "www.yourwebsite.com", name_ "website"]
                    span_ [class_ "icon is-small is-left"] $
                        i'_ [class_ "fas fa-user"]
            div_ [class_ "field"] $ do
                label_ [class_ "label"] "Email"
                div_ [class_ "control has-icons-left has-icons-right"] $ do
                    input_ [class_ "input is-rounded", type_ "email", placeholder_ "youremail@email.com", name_ "email"]
                    span_ [class_ "icon is-small is-left"] $
                        i'_ [class_ "fas fa-envelope"]
            div_ [class_ "field"] $ do
                label_ [class_ "label"] "Subject"
                div_ [class_ "control"] $
                    div_ [class_ "select is-rounded"] $
                        select_ [name_ "subject[]"] $
                            mapM_ (uncurry subjectOpt_) (zip optNames optValues)
            div_ [class_ "field"] $ do
                label_ [class_ "label"] "Message"
                div_ [class_ "control"] $
                    textarea_ [class_ "textarea", name_ "message", placeholder_ "Your message."] ""
            div_ [class_ "field is-grouped is-grouped-centered"] $
                div_ [class_ "control"] $
                    button_ [type_ "submit", class_ "button is-primary is-medium is-rounded hvr-grow"] "Submit"
  where
    subjectOpt_ :: Applicative m
                => Monad m
                => Text
                -> Text
                -> HtmlT m ()
    subjectOpt_ name value = option_ [value_ value] $ toHtml name
