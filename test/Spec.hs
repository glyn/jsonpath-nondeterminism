{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson.QQ
import Lib
import Test.HUnit
 
main :: IO ()
main = do
        runTestTTAndExit (test [testChildWildcard1
                               ,testChildWildcard2
                               ,testChildWildcard3
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
