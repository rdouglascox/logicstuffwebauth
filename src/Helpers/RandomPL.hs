{-# LANGUAGE OverloadedStrings #-}

module Helpers.RandomPL where

import Data.Text
import Data.Maybe
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5

import Random.PLprops
import Settings.PLSettings
import Printing.UnicodePLProps
import Printing.PLprop
import System.Random

-- suppose from the yesod form that what we get is a
-- Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text
-- we want to return some html with the list of rprops in it

getSettings :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Settings
getSettings x y z u = dSettings {minConstr = fromMaybe 1 x 
                                          ,maxConstr = fromMaybe 3 y
                                          ,numProps = fromMaybe 1 z
                                          ,basics = unpack (fromMaybe "PQR" u )
                                          }

getrPL :: RandomGen g => g -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> H.Html
getrPL g x y z u b = case b of 
    Nothing -> H.toHtml $ printprops $ rPL g (getSettings x y z u)
    Just False -> H.toHtml $ printprops $ rPL g (getSettings x y z u)
    Just True -> H.toHtml $ printPLprops $ rPL g (getSettings x y z u)


