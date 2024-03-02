{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.List (runListT)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Lib


main :: IO ()

main = do
    let ioAction = runListT $ do
         nl <- root $ fromMaybe undefined decode "{\"foo\": 123}"
         fmap childWildcard nl
             
    -- Execute the IO action and print each nodelist
    mapM_ print ioAction

