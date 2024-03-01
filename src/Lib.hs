module Lib
    ( Nodelist
    , Query
    , root
    , childWildcard
    ) where

import Data.Aeson
import Data.List

type Nodelist = [Value] -- omit locations
type Query = Value -> [Nodelist]
 -- what does a non-deterministic query look like?

-- root is a query corresponding to $
-- It is deterministic, so it returns one nodelist.
root :: Query
root v = [[v]]

childWildcard :: Query
-- Object !Object	 
-- Array !Array	 
-- String !Text	 
-- Number !Scientific	 
--Bool !Bool	 
--Null
childWildcard (Object o) = permutations $ elems o
childWildcard (Array a) = [toList a]
childWildcard (String _) = [[]]
childWildcard (Number _) = [[]]
childWildcard (Bool _) = [[]]
childWildcard Null = [[]]