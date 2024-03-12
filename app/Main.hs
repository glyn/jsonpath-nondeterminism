{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy.Char8 as BSL
import Lib


main :: IO ()

main = do
         mapM_ (BSL.putStrLn . addNewline . encode) ([
            do -- [*] applied to an object (dervied from $.o[*] example of Table 6 in RFC 9535)
              nl :: Nodelist <- root [aesonQQ| {"j": 1, "k": 2} |]
              childWildcard nl,
            do -- [*,*] applied to an object (derived from $.o[*, *] example in Table 6 of RFC 9535)
              nl :: Nodelist <- root [aesonQQ| {"j": 1, "k": 2} |]
              w1 <- childWildcard nl
              w2 <- childWildcard nl
              return (w1 ++ w2),
            do -- [*][*] applied to an object of objects
              nl :: Nodelist <- root [aesonQQ| { "x": {"a": 1, "b": 2}
                                               , "y": {"c": 3, "d": 4}
                                               } |]
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl',
            do -- [*][*] applied to an object of arrays/objects
              nl :: Nodelist <- root [aesonQQ| { "x": {"a": 1, "b": 2}
                                               , "y": [3, 4]
                                               } |]
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl',
            do -- [*][*] applied to an array of objects
              nl :: Nodelist <- root [aesonQQ| [ {"a": 1, "b": 2}
                                               , {"c": 3, "d": 4}
                                               ] |]
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl',
            do -- ..[*] applied to an array containing a non-empty array and something else
              nl :: Nodelist <- root [aesonQQ| [ [1]
                                               , 2
                                               ] |]
              descendantWildcard nl,
            do -- ..[*] applied to an object 
              nl :: Nodelist <- root [aesonQQ| { "x": 1
                                               , "y": 2
                                               } |]
              descendantWildcard nl,
            do -- ..[*] applied to an object containing arrays
              nl :: Nodelist <- root [aesonQQ| { "x": [1]
                                               , "y": [2]
                                               } |]
              descendantWildcard nl,
            do -- ..[*] applied to an object containing an object and an array
              nl :: Nodelist <- root [aesonQQ| { "x": {"a": 1}
                                               , "y": [3]
                                               } |]
              descendantWildcard nl,
            do -- ..[*] applied to an object containing objects
              nl :: Nodelist <- root [aesonQQ| { "x": {"a": 1}
                                               , "y": {"c": 3}
                                               } |]
              descendantWildcard nl
            ] :: [[Nodelist]])

addNewline :: BSL.ByteString -> BSL.ByteString
addNewline bs = BSL.snoc bs '\n'