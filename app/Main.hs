{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import Lib


main :: IO ()

main = do
         mapM_ (BSL.putStrLn . encode . uniq) ([
            do -- [*] applied to an object
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"foo\": 123, \"bar\": 456}")
              childWildcard nl,
            do -- [*][*] applied to an object of objects
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"x\": {\"a\": 1, \"b\":2 }, \"y\": {\"c\": 3, \"d\": 4}}")
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl',
            do -- [*,*] applied to an object (derived from non-deterministic example in Table 5 of RFC 9535)
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"j\": 1, \"k\": 2}")
              childDoubleWildcard nl
            ] :: [[Nodelist]])

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList