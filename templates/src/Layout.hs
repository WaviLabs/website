{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Layout where

import Basic
import Data.Text hiding (zip)
import Lucid
import Html


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
