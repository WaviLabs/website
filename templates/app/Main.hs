{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Dhall
import GHC.Generics (Generic)
import Lucid (renderTextT)

import qualified Base as Base
import qualified Data.Text.Lazy.IO as IO
import qualified Page.Home as Home

data Config = Config
    { baseConfig :: Base.Config
    , homeConfig :: Home.Config
    } deriving (Generic, Show)

instance FromDhall Config

main :: IO ()
main = do
    config <- input auto "../config.dhall"

    let wrap = Base.render $ baseConfig config
        home = Home.render $ homeConfig config

    text <- renderTextT $ wrap home
    IO.writeFile "../docs/indexText.html" text
