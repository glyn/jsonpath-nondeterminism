{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Lib


main :: IO ()

main = do
         mapM_ print [
            do -- [*] applied to an object
              nl <- root $ fromMaybe undefined (decode "{\"foo\": 123, \"bar\": 456}")
              concatMap childWildcard nl,
            do -- [*][*] applied to an object of objects
               -- expect 4 possibilities, but got 8 with some duplicates
               -- TODO: understand this
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"x\": {\"a\": 1, \"b\":2 }, \"y\": {\"c\": 3, \"d\": 4}}")
              nl' :: Nodelist <- concatMap childWildcard nl
              concatMap childWildcard nl'
            ]
