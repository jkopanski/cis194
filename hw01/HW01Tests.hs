-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, d) = toRevDigits n == d

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, d) = sumDigits n == d

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, a, b, c, d) = hanoi n a b c== d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(123, [3,2,1]), (1234, [4,3,2,1]), (0, []), (-5, []), (-123, [])]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([0,0,0,0], [0,0,0,0]), ([4,3,2,1], [4,6,2,2]), ([4,9,5,5], [4,18,5,10])]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([10,5,18,4], 19), ([32, 4, 40, 50], 18)]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(2, "a", "b", "c",  [("a","c"), ("a","b"), ("c","b")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
