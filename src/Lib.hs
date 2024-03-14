{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( Nodelist
    , Query
    , root
    , childWildcard
    , descendantWildcard
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
    -- See https://www.rfc-editor.org/rfc/rfc9535#section-2.5.2.2
    inputNodeAndDescendants <- inputAndStructuredDescendants nl
    childWildcard inputNodeAndDescendants)

-- inputAndStructuredDescendants is a query corresponding to .. except that primitive descendants are discarded
inputAndStructuredDescendants :: Query
inputAndStructuredDescendants [] = [[]]
inputAndStructuredDescendants (n:ns) = uniq (do
    -- To generate all valid non-deterministic orderings of the input node and its structured descendants, a list of all
    -- permutations of the list of structured descendants (annotated with their paths) is filtered according to the rules
    -- in RFC 9535 (Section 2.5.2.2). Then the annotations are removed.

    -- OPT: factor of 3 improvement by factoring out the root node from the permutations and adding it back in at the end 
    -- l :: Nodelist <- map (map snd) $ filter validDescendantOrdering $ permutations $ ([], n):descendants [] n
    -- OPT: factor of two by addings root node in after filter
    -- l :: Nodelist <- map (map snd) $ filter validDescendantOrdering $ map (([], n):) $ permutations $ descendants [] n
    -- OPT: combining maps slows things down
    l :: Nodelist <- map (map snd) $ map (([], n):) $ filter validDescendantOrdering $ permutations $ descendants [] n

    r :: Nodelist <- descendantWildcard ns
    return (l ++ r))
    where descendants :: Path -> Value -> [(Path,Value)] -- input value must be a child of the argument at location denoted by the input path
          descendants p (Object o) = objectStructuredChildren p o ++ [ x | (k,v) <- KM.toList o, x <- descendants (p++[Member $ K.toString k]) v]
          descendants p (Array a) = arrayStructuredChildren p a ++ [x | i <- [0..(length a - 1)], x <- descendants (p++[Element i]) $ a ! i]
          descendants _ _ = []

type Path = [PathItem]
data PathItem = Member String | Element Int
                deriving Eq

-- objectStructuredChildren returns the annotated childen of an object which are structured (i.e. not primitives)
-- (primitives would be discarded later)
objectStructuredChildren :: Path -> KM.KeyMap Value -> [(Path, Value)]
objectStructuredChildren p km = [(p++[Member $ K.toString k],v) | (k,v) <- KM.toList km, not $ primitive v]

-- arrayStructuredChildren returns the annotated childen of an array which are structured (i.e. not primitives)
-- (primitives would be discarded later)
arrayStructuredChildren :: Path -> Vector Value -> [(Path, Value)]
arrayStructuredChildren p v = filter (not . primitive . snd) [(p++[Element i], v ! i) | i <- [0.. length (toList v) - 1]]

primitive :: Value -> Bool
primitive (Object _) = False
primitive (Array _) = False
primitive _ = True

validDescendantOrdering :: [(Path,Value)] -> Bool
validDescendantOrdering [] = True
validDescendantOrdering [_] = True
validDescendantOrdering (x@(px,_):y@(py,_):xs) = validBefore px py && validDescendantOrdering (x:xs) && validDescendantOrdering (y:xs)

validBefore :: Path -> Path -> Bool
-- See https://www.rfc-editor.org/rfc/rfc9535#section-2.5.2.2
-- OPT: validBefore x y = not (childBeforeParent x y || arrayElementsOutOfOrder x y) was slower than the following
validBefore x y = not (arrayElementsOutOfOrder x y || childBeforeParent x y)

childBeforeParent :: Path -> Path -> Bool
childBeforeParent [] _ = False
childBeforeParent p q = init p == q
-- OPT: following version only gave 5% improvement
-- childBeforeParent [] _ = False
-- childBeforeParent p [] = length p == 1
-- childBeforeParent (p:ps) (q:qs) = (p == q) && not (null ps) && (init ps == qs)
    
arrayElementsOutOfOrder :: Path -> Path -> Bool
arrayElementsOutOfOrder [] _ = False  
arrayElementsOutOfOrder _ [] = False  
arrayElementsOutOfOrder [Element m] [Element n] = m >= n
-- OPT: && is strict in its first argument
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
