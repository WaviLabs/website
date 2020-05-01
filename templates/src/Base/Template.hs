{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: Make the navbar and footer seperate components
module Base.Template where

import Base.Types

import Data.Text hiding (zip)
import Html
import Lucid
import Lucid.Base


-- | Navbar

navbarLogo_ :: Applicative m
            => Monad m
            => HtmlT m ()
navbarLogo_ =
    a_ [class_ "navbar-item", href_ "index.html"] $
        img_ [src_ "new-logo.svg.png", alt_ "Wavi Labs logo"]

hamburgerNav_ :: Applicative m
              => Monad m
              => HtmlT m ()
hamburgerNav_ =
    a_ [ role_ "button"
       , class_ "navbar-burger burger"
       , aria_label_ "menu"
       , aria_expanded_ "false"
       , data_target_ "navbar"
       ] $ do
        span_ [aria_hidden_ "true"] ""
        span_ [aria_hidden_ "true"] ""
        span_ [aria_hidden_ "true"] ""

-- | First arg is [href], second arg is [inner]
navbarMenu_ :: Monad m
            => Applicative m
            => [Text]
            -> [HtmlT m a]
            -> HtmlT m ()
navbarMenu_ hrefs inners =
    div_ [id_ "navbar", class_ "navbar-menu"] $ do
        div_ [class_ "navbar-end"] $ do
            mapM_
                (uncurry (\href -> \inner -> a_ [href_ href, class_ "navbar-item animate-hover"] inner))
                (zip hrefs inners)

-- | Footer
-- | TODO: This should be the render func for footer comp.
myFooter_ :: Monad m
          => Applicative m
          => Footer
          -> HtmlT m ()
myFooter_ Footer{..} =
    footer_ [class_ "footer"] $ do
        div_ [class_ "columns"] $ do
            div_ [class_ "column is-one-fifths"] $
                div_ [class_ "container has-text-centered"] $
                    a_ [href_ "#"] $
                        img_ [src_ "g915.png", alt_ "Wavi Labs logo"]

            -- | Columns
            footerCol_ col1
            footerCol_ col2
            footerCol_ col3
            footerCol_ col4

footerCol_ :: Applicative m
           => Monad m
           => FooterColumn
           -> HtmlT m ()
footerCol_ FooterColumn{..} =
    div_ [class_ "column"] $
        div_ [class_ "container has-text-centered"] $ do
            h2_ [class_ "subtitle"] $
                strong_ $
                    u_ [] $ toHtml title
            mapM_
                (\FooterLink{..} -> p_ $ a_ [href_ link] $ toHtml name)
                links

render :: Applicative m
     => Monad m
     => Config
     -> HtmlT m a
     -> HtmlT m ()
render Config{..} innerHtml =
    doctypehtml_ $ do
        head_ $ do
            meta_ [ charset_ "utf-8"
                  , name_ "viewport"
                  , content_ "width=device-width, initial-scale=1"
                  ];
            title_ "Wavi Labs";
            link_ [ rel_ "stylesheet"
                  , href_ "css/wavi-labs.css"
                  ];
            script'_ [ defer_ "", src_ "https://use.fontawesome.com/releases/v5.3.1/js/all.js"];
            script'_ [ src_ "https://kit.fontawesome.com/a5fd11fb22.js", crossorigin_ "anonymous"];
        body_ $ do
            nav_ [ class_ "navbar is-fixed-top", role_ "navigation", aria_label_ "main navigation"] $
                container_ $ do
                    div_ [class_ "navbar-brand"] $ do
                        navbarLogo_
                        hamburgerNav_
                    navbarMenu_
                        ["index.html", "services.html", "blog.html", "team.html", "contact.html"]
                        ["About", "Services", "Blog", "Team", "Contact"]
            -- | Inner Html goes here
            innerHtml
            myFooter_ footerConfig;
