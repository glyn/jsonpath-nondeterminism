{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Lib


main :: IO ()

main = do
         mapM_ (BSL.putStrLn . encode) [
            do -- [*] applied to an object
              nl <- root $ fromMaybe undefined (decode "{\"foo\": 123, \"bar\": 456}")
              concatMap childWildcard nl,
            do -- [*][*] applied to an object of objects
              -- DEBUG: we are in the list monad so the following binds nl to the Nodelist in the list returned from root
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"x\": {\"a\": 1, \"b\":2 }, \"y\": {\"c\": 3, \"d\": 4}}")

              -- The following maps childWildcard over nl and concatenates the resultant nodelists.
               -- expect 4 possibilities, but got 8 with some duplicates
               -- TODO: understand this
              nl' :: Nodelist <- concatMap childWildcard nl
              concatMap childWildcard nl'
            ]
