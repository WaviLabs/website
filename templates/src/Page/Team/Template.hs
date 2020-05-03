module Page.Team.Template where

import Lucid
import Page.Team.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Basic (showT, flipFlop)
import Html (dataNetifly_, i'_)

import qualified Data.Text as T


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render = undefined
