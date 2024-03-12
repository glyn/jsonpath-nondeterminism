{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson.QQ
import Lib
import Test.HUnit
 
main :: IO ()
main = do
        runTestTTAndExit (test [testChildWildcard1
                               ,testChildWildcard2
                               ,testChildWildcard3
                               --,testDescendantWildcard -- FIXME: this test takes several minutes to complete
                               ,testDescendantWildcard1
                               ,testDescendantWildcard2
                               ,unitTests                   
                               ])

testChildWildcard1 :: Test
testChildWildcard1 = TestCase ( assertEqual "[*] applied to an object (derived from $[*] example of Table 6 in RFC 9535)" expected (childWildcard input) )
    where arg = [aesonQQ| {"o": {"j": 1, "k": 2}
                          ,"a": [5, 3]
                          } |]
          input = [arg]
          val1 = [aesonQQ| {"j": 1, "k": 2} |]
          val2 = [aesonQQ| [5, 3] |]
          expected = [[val1, val2], [val2, val1]]

testChildWildcard2 :: Test
testChildWildcard2 = TestCase ( assertEqual "[*] applied to an object (derived from $.o[*] example of Table 6 in RFC 9535)" expected (childWildcard input) )
    where arg = [aesonQQ| {"j": 1, "k": 2} |]
          input = [arg]
          val1 = [aesonQQ| 1 |]
          val2 = [aesonQQ| 2 |]
          expected = [[val1, val2], [val2, val1]]

testChildWildcard3 :: Test
testChildWildcard3 = TestCase ( assertEqual "[*] applied to an array (derived from $.a[*] example of Table 6 in RFC 9535)" expected (childWildcard input) )
    where arg = [aesonQQ| [5, 3] |]
          input = [arg]
          val5 = [aesonQQ| 5 |]
          val3 = [aesonQQ| 3 |]
          expected = [[val5, val3]]

-- FIXME: The following test takes several minutes to complete and produces the following actual value:
{-
[[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 1.0,Number 2.0,Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 2.0,Number 1.0,Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 1.0,Number 2.0,Number 4.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 2.0,Number 1.0,Number 4.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 1.0,Number 2.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 2.0,Number 1.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0,Number 1.0,Number 2.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0,Number 2.0,Number 1.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Number 1.0,Number 2.0,Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Number 2.0,Number 1.0,Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 1.0,Number 2.0,Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 2.0,Number 1.0,Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 1.0,Number 2.0,Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 2.0,Number 1.0,Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 1.0,Number 2.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 2.0,Number 1.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0,Number 1.0,Number 2.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0,Number 2.0,Number 1.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Number 1.0,Number 2.0,Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0],[Array [Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])]],Object (fromList [("j",Number 1.0),("k",Number 2.0)]),Number 5.0,Number 3.0,Array [Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)])],Number 2.0,Number 1.0,Object (fromList [("j",Number 4.0)]),Object (fromList [("k",Number 6.0)]),Number 4.0,Number 6.0]]
-}
testDescendantWildcard :: Test
testDescendantWildcard = TestCase ( assertEqual "..[*] test in Table 16 in RFC 9535)" expected (descendantWildcard input) )
    where arg = [aesonQQ| {"o": {"j": 1, "k": 2}
                          ,"a": [5, 3, [{"j": 4}, {"k": 6}]]
                          } |]
          input = [arg]
          expected = [] -- FIXME: add the correct value

testDescendantWildcard1 :: Test
testDescendantWildcard1 = TestCase ( assertEqual "..[*] test derived from example in Table 16 in RFC 9535)" expected (descendantWildcard input) )
    where arg = [aesonQQ| {"a": [5, 3, [{"j": 4}, {"k": 6}]]} |]
          input = [arg]
          val3 = [aesonQQ| 3 |]
          val4 = [aesonQQ| 4 |]
          val5 = [aesonQQ| 5 |]
          val6 = [aesonQQ| 6 |]
          valArray1 = [aesonQQ| [5, 3, [{"j": 4}, {"k": 6}]] |]
          valArray2 = [aesonQQ| [{"j": 4}, {"k": 6}] |]
          valObj1 = [aesonQQ| {"j": 4} |]
          valObj2 = [aesonQQ| {"k": 6} |]
          expected = [[valArray1, val5, val3, valArray2, valObj1, valObj2, val4, val6]]

testDescendantWildcard2 :: Test
testDescendantWildcard2 = TestCase ( assertEqual "another ..[*] test derived from example in Table 16 in RFC 9535)" expected (descendantWildcard input) )
    where arg = [aesonQQ| {"o": {"j": 1, "k": 2}} |]
          input = [arg]
          val1 = [aesonQQ| 1 |]
          val2 = [aesonQQ| 2 |]
          valObj = [aesonQQ| {"j": 1, "k": 2} |]
          expected = [[valObj, val1, val2], [valObj, val2, val1]]
