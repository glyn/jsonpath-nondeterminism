{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( Nodelist
    , Query
    , root
    , childWildcard
    , childDoubleWildcard
    ) where

import Data.Aeson
import Data.Aeson.KeyMap (elems)
import Data.List (permutations)
import Data.Vector (toList)

type Nodelist = [Value] -- omit locations

 -- Generalise a RFC 9535 query to return a list of possible results (nodelists).
 -- If there is one item in the list, the query is deterministic.
 -- If there is more than one item in the list, the query is non-deterministic.
type Query = Nodelist -> [Nodelist]

-- root is a query corresponding to $
-- It is deterministic, so it returns one nodelist.
root :: Value -> [Nodelist]
root v = [[v]]

-- childWildcard is a query corresponding to [*]
-- It is non-deterministic when applied to an object with more than one member.
childWildcard :: Query
childWildcard [] = [[]]
childWildcard (n:ns) = do
    l :: Nodelist <- children n
    r :: Nodelist <- childWildcard ns
    return (l ++ r)
    where children :: Value -> [Nodelist] -- input value must be a child of the argument
          children (Object o) = permutations $ elems o
          children (Array a) = [toList a]
          children _ = [[]]

-- childWildcard is a query corresponding to [*,*]
-- It is non-deterministic when applied to an object with more than one member.
childDoubleWildcard :: Query
childDoubleWildcard [] = [[]]
childDoubleWildcard nl = do
    l :: Nodelist <- childWildcard nl
    r :: Nodelist <- childWildcard nl
    return (l ++ r)
