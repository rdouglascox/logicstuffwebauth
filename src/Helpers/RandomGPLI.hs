{-# LANGUAGE OverloadedStrings #-}

module Helpers.RandomGPLI where

import Data.Text
import Data.Maybe
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5

import Random.GPLIprop
import Settings.GPLISettings
import Printing.UnicodeGPLIProps
import Printing.GPLIprop
import System.Random

-- suppose from the yesod form that what we get is a
-- Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text
-- we want to return some html with the list of rprops in it

getSettings :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Settings
getSettings x y z u1 u2 = dSettings {minConst = fromMaybe 1 x 
                                          ,maxConst = fromMaybe 3 y
                                          ,numProps = fromMaybe 1 z
                                          ,predicats = unpack (fromMaybe "FGH" u1 )
                                          ,constants = unpack (fromMaybe "abc" u2 )
                                          }

getrGPLI :: RandomGen g => g -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> H.Html
getrGPLI g x y z u1 u2 b = case b of 
    Nothing -> H.toHtml $ printprops $ rGPLI g (getSettings x y z u1 u2)
    Just False -> H.toHtml $ printprops $ rGPLI g (getSettings x y z u1 u2)
    Just True -> H.toHtml $ printGPLIprops $ rGPLI g (getSettings x y z u1 u2)

