{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( Nodelist
    , Query
    , root
    , childWildcard
    , descendantWildcard
    , inputAndDescendants
    , unitTests
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.List (permutations)
import qualified Data.Set as Set
import Data.Vector (toList, (!), Vector)
import Test.HUnit (Test(..), assertEqual, test)

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
childWildcard (n:ns) = uniq (do
    l :: Nodelist <- children n
    r :: Nodelist <- childWildcard ns
    return (l ++ r))
    where children :: Value -> [Nodelist] -- input value must be a child of the argument
          children (Object o) = permutations $ KM.elems o
          children (Array a) = [toList a]
          children _ = [[]]

-- descendantWildcard is a query corresponding to ..[*]
-- It is non-deterministic in certain cases.
descendantWildcard :: Query
descendantWildcard [] = [[]]
descendantWildcard nl = uniq (do
    -- as per the spec, descendantWildcard is childWildcard applied to the node and its descendants
    inputNodeAndDescendants <- inputAndDescendants nl
    childWildcard inputNodeAndDescendants)

-- inputAndDescendants is a query corresponding to ..
-- The syntax .. (on its own) is not supported in RFC 9535, although its semantics is used to define ..[*].
-- It is non-deterministic when it traverses over an object with more than one member and in some cases where
-- descendants of one node are interleaved with descendants of another node.
inputAndDescendants :: Query
inputAndDescendants [] = [[]]
inputAndDescendants (n:ns) = uniq (do
    l :: Nodelist <- map (map snd) $ filter validDescendantOrdering $ permutations $ ([], n):descendants [] n
    r :: Nodelist <- descendantWildcard ns
    return (l ++ r))
    where descendants :: Path -> Value -> [(Path,Value)] -- input value must be a child of the argument at location denoted by the input path
          descendants p (Object o) = objectChildren p o ++ [ x | (k,v) <- KM.toList o, x <- descendants (p++[Member $ K.toString k]) v]
          descendants p (Array a) = arrayChildren p a ++ [x | i <- [0..(length a - 1)], x <- descendants (p++[Element i]) $ a ! i]
          descendants _ _ = []

type Path = [PathItem]
data PathItem = Member String | Element Int
                deriving Eq

objectChildren :: Path -> KM.KeyMap Value -> [(Path, Value)]
objectChildren p km = [(p++[Member $ K.toString k],v) | (k,v) <- KM.toList km]

arrayChildren :: Path -> Vector Value -> [(Path, Value)]
arrayChildren p v = [(p++[Element i], v ! i) | i <- [0.. length (toList v) - 1]]

validDescendantOrdering :: [(Path,Value)] -> Bool
validDescendantOrdering [] = True
validDescendantOrdering [_] = True
validDescendantOrdering (x@(px,_):y@(py,_):xs) = validBefore px py && validDescendantOrdering (x:xs) && validDescendantOrdering (y:xs)

validBefore :: Path -> Path -> Bool
validBefore x y = not (childBeforeParent x y || arrayElementsOutOfOrder x y)

childBeforeParent :: Path -> Path -> Bool
childBeforeParent [] _ = False
childBeforeParent p q = init p == q
    
arrayElementsOutOfOrder :: Path -> Path -> Bool
arrayElementsOutOfOrder [] _ = False  
arrayElementsOutOfOrder _ [] = False  
arrayElementsOutOfOrder [Element m] [Element n] = m >= n  
arrayElementsOutOfOrder (p:ps) (q:qs) = p == q && arrayElementsOutOfOrder ps qs

-- Remove duplicates from the input list
-- The implementation ensures that the result list is sorted
uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList

unitTests :: Test
unitTests = test [testUniqRemdup
                 ,testUniqSort
                 ]

testUniqRemdup :: Test
testUniqRemdup = TestCase ( assertEqual "removes duplicates" expected (uniq input) )
    where input :: [Int] = [0,1,2,1,0]
          expected = [0,1,2]

testUniqSort :: Test
testUniqSort = TestCase ( assertEqual "sorts result" expected (uniq input) )
    where input :: [Int] = [2,0,0,2,1]
          expected = [0,1,2]
