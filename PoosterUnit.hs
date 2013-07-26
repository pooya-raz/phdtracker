module PoosterUnit where 

import Test.HUnit
import Counter

{-
test1 = TestCase (assertEqual "for compareDates s1 s2," (False) (compareDates s1 s2)) 
tests = TestList [TestLabel "test1" test1]
-}

tests = test ["test1" ~: "for compareDates s1 s2" ~: (False) ~=? (compareDates s1 s2)]
