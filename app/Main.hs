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
            do -- [*] applied to an object (dervied from $.o[*] example of Table 6 in RFC 9535)
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"j\": 1, \"k\": 2}")
              childWildcard nl,
            do -- [*,*] applied to an object (derived from $.o[*, *] example in Table 6 of RFC 9535)
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"j\": 1, \"k\": 2}")
              w1 <- childWildcard nl
              w2 <- childWildcard nl
              return (w1 ++ w2),
            do -- [*][*] applied to an object of objects
              nl :: Nodelist <- root $ fromMaybe undefined (decode "{\"x\": {\"a\": 1, \"b\":2 }, \"y\": {\"c\": 3, \"d\": 4}}")
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl'
            ] :: [[Nodelist]])

-- Remove duplicates from the input list
-- The implementation ensures that the result list is sorted
uniq :: Ord a => [a] -> [a]
uniq = toList . fromList