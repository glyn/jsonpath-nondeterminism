{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Lib


main :: IO ()

main = do
         let result = do
                        nl <- root $ fromMaybe undefined (decode "{\"foo\": 123}")
                        fmap childWildcard nl in        
            mapM_ print result

