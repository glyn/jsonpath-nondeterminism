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
type Query = Value -> [Nodelist]

-- root is a query corresponding to $
-- It is deterministic, so it returns one nodelist.
root :: Query
root v = [[v]]

-- childWildcard is a query corresponding to [*]
-- It is non-deterministic when applied to an object with more than one member.
childWildcard :: Query
childWildcard (Object o) = permutations $ elems o
childWildcard (Array a) = [toList a]
childWildcard (String _) = [[]]
childWildcard (Number _) = [[]]
childWildcard (Bool _) = [[]]
childWildcard Null = [[]]