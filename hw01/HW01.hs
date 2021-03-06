{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = quot x 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []    = []
doubleEveryOther [x]   = [x]
doubleEveryOther (x:(y:zs)) = x : y*2 : doubleEveryOther zs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:[]) = lastDigit x + dropLastDigit x
sumDigits (x:ys) = sumDigits [x] + sumDigits ys


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = (sumDigits (doubleEveryOther (toRevDigits x))) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ (a, b) : hanoi (n-1) c b a
