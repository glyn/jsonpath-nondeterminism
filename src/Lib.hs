module Lib
    ( Nodelist
    , Query
    , root
    , childWildcard
    ) where

import Data.Aeson
import Data.Aeson.KeyMap (elems)
import Data.List (permutations)
import Data.Vector (toList)

type Nodelist = [Value] -- omit locations

 -- Generalise a RFC 9535 query to return a list of possible results (nodelists).
 -- If there is one item in the list, the query is deterministic.Applicative
 -- If there is more than one item in the list, the query is non-deterministic.
type Query = Nodelist -> [Nodelist]

-- root is a query corresponding to $
-- It is deterministic, so it returns one nodelist.
root :: Value -> [Nodelist]
root v = [[v]]

-- childWildcard is a query corresponding to [*]
-- It is non-deterministic when applied to an object with more than one member.
-- WIP: need to upgrade the implementation to take a Nodelist as input
-- The difficulty is how to combine the non-deterministic effects produced by each node in the nodelist.
-- Is it some kind of Cartesian product of the non-deterministic lists?
childWildcard :: Query
childWildcard [] = [[]]
childWildcard (n:ns) = do
    l <- cw n
    r <- childWildcard ns
    if null r then if null l then [] else [l] else if null l then [r] else [l, r]
    where cw :: Value -> [Nodelist] -- input value must be a child of the argument
          cw (Object o) = permutations $ elems o
          cw (Array a) = [toList a]
          cw _ = [[]]
