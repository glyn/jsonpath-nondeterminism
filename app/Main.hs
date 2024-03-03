{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Lib


main :: IO ()

main = do
         mapM_ (mapM_ print) [
            do
              nl <- root $ fromMaybe undefined (decode "{\"foo\": 123, \"bar\": 456}")
              fmap childWildcard nl,
            do
              nl <- root $ fromMaybe undefined (decode "[1,2]")
              fmap childWildcard nl
            ]
