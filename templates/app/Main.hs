{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Dhall
import GHC.Generics (Generic)
import Lucid (renderTextT)

import qualified Base as Base
import qualified Page.Blog as Blog
import qualified Page.Contact as Contact
import qualified Page.Home as Home
import qualified Page.Services as Services
import qualified Page.Team as Team

import qualified Data.Text.Lazy.IO as IO


data Config = Config
    { baseConfig     :: Base.Config
    , homeConfig     :: Home.Config
    -- , servicesConfig :: Services.Config
    -- , blogConfig     :: Blog.Config
    -- , teamConfig     :: Team.Config
    , contactConfig  :: Contact.Config
    } deriving (Generic, Show)

instance FromDhall Config

main :: IO ()
main = do
    config <- input auto "../config.dhall"

    let wrap     = Base.render $ baseConfig config
        home     = Home.render $ homeConfig config
        -- services = Services.render $ servicesConfig config
        -- blog     = Blog.render $ blogConfig config
        -- team     = Team.render $ teamConfig config
        contact  = Contact.render $ contactConfig config

    homeHtml <- renderTextT $ wrap home
    IO.writeFile "../docs/index.html" homeHtml

    -- servicesHtml <- renderTextT $ wrap services
    -- IO.writeFile "../docs/servicesText.html" servicesHtml

    -- blogHtml <- renderTextT $ wrap blog
    -- IO.writeFile "../docs/blogText.html" blogHtml

    -- teamHtml <- renderTextT $ wrap team
    -- IO.writeFile "../docs/teamText.html" teamHtml

    contactHtml <- renderTextT $ wrap contact
    IO.writeFile "../docs/contact.html" contactHtml
