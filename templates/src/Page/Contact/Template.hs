module Page.Contact.Template where

import Lucid
import Page.Contact.Types

import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Text (Text)
import Basic (showT, flipFlop)
import Html (container_, dataNetifly_, i'_)

import qualified Data.Text as T


render :: Applicative m
       => Monad m
       => Config
       -> HtmlT m ()
render = undefined