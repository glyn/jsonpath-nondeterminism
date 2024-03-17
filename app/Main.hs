{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Lib

runQuery :: ToJSON a => String -> Value -> ([Nodelist] -> a) -> BLU.ByteString
runQuery desc arg lam = (addNewline $ BLU.fromString desc) `BSL.append` addNewline (encode arg) `BSL.append` encode (lam $ root arg)

main :: IO ()

main = do
         mapM_ (BSL.putStrLn . addNewline) [
          runQuery "[*] applied to an object (derived from $.o[*] example of Table 6 in RFC 9535)"
          [aesonQQ| {"j": 1, "k": 2} |]
          (\r -> (do
              nl :: Nodelist <- r
              childWildcard nl)),

          runQuery "[*,*] applied to an object (derived from $.o[*, *] example in Table 6 of RFC 9535)"
          [aesonQQ| {"j": 1, "k": 2} |]
          (\r -> do
              nl :: Nodelist <- r
              w1 <- childWildcard nl
              w2 <- childWildcard nl
              return (w1 ++ w2)),

          runQuery "[*][*] applied to an object of objects"
          [aesonQQ| { "x": {"a": 1, "b": 2}
                    , "y": {"c": 3, "d": 4}
                    } |]
          (\r -> do
              nl :: Nodelist <- r
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl'),
          
          runQuery "[*][*] applied to an object of arrays/objects"
          [aesonQQ| { "x": {"a": 1, "b": 2}
                    , "y": [3, 4]
                    } |]
          (\r -> do 
              nl :: Nodelist <- r
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl'),
          
          runQuery "[*][*] applied to an array of objects"
          [aesonQQ| [ {"a": 1, "b": 2}
                    , {"c": 3, "d": 4}
                    ] |]
          (\r -> do
              nl :: Nodelist <- r
              nl' :: Nodelist <- childWildcard nl
              childWildcard nl'),
          
          runQuery "..[*] applied to an array containing a non-empty array and something else"
          [aesonQQ| [ [1]
                    , 2
                    ] |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl),

          runQuery "..[*] applied to a carefully chosen nested collection of arrays"
          [aesonQQ| [ [[1]]
                    , [2]
                    ] |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl),
          
          runQuery "..[*] applied to an object"
          [aesonQQ| { "x": 1
                    , "y": 2
                    } |]
          (\r -> do 
              nl :: Nodelist <- r
              descendantWildcard nl),
          
          runQuery "..[*] applied to an object containing arrays"
          [aesonQQ| { "x": [1]
                    , "y": [2]
                    } |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl),
          
          runQuery "..[*] applied to an object containing an object and an array"
          [aesonQQ| { "x": {"a": 1}
                    , "y": [3]
                    } |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl),
          
          runQuery "..[*] applied to an object containing objects"
          [aesonQQ| { "x": {"a": 1}
                    , "y": {"c": 3}
                    } |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl),

          runQuery "..[*] example from Table 16 in RFC 9535"
          [aesonQQ| {"o": {"j": 1, "k": 2}
                    ,"a": [5, 3, [{"j": 4}, {"k": 6}]]
                    } |]
          (\r -> do
              nl :: Nodelist <- r
              descendantWildcard nl)
            ]

addNewline :: BSL.ByteString -> BSL.ByteString
addNewline bs = BSL.snoc bs '\n'