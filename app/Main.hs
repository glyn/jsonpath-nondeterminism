{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.List (ListT(..))
import Data.Aeson
import Lib


main :: IO ()

main = do
    let ioAction = runListT $ do
         nl <- root decode "{\"foo\": 123}" -- :: Maybe Value
         fmap childWildcard nl
             
    -- Execute the IO action and print each nodelist
    mapM_ print ioAction

