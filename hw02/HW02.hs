{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length $ filter (==True) $ zipWith (==) a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = go [] colors
  where go :: [Int] -> [Peg] -> [Int]
        go acc [] = acc
        go _ (x:xs) = (length $ filter (==True) $ map (==x) code) : go [] xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = foldr (+) 0 $ zipWith (min) (countColors a) (countColors b)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
  where exact    = exactMatches secret guess
        nonexact = matches secret guess - exact

-- Exercise 4 -----------------------------------------

exactl :: Move -> Int
exactl (Move _ e _) = e

nexact :: Move -> Int
nexact (Move _ _ n) = n

getCode :: Move -> Code
getCode (Move code _ _) = code

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e n) code = exactl gen == e && nexact gen == n
  where gen = getMove code guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move list = filter (isConsistent move) list

-- Exercise 6 -----------------------------------------

addPegs :: [Peg] -> [Code] -> [Code]
addPegs [] _        = []
addPegs (p:ps) []   = [p] : (addPegs ps [])
addPegs (p:ps) code = map (p:) code ++ (addPegs ps code)
-- addPegs (p:ps) code = foldr (++) (addPegs ps code) (map (p:) code)

allCodes :: Int -> [Code]
allCodes n
  | n <= 0    = []
  | otherwise = addPegs colors $ allCodes (n-1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = go [] $ allCodes 4
  where go :: [Move] -> [Code] -> [Move]
        go res (g:gs)
          | g == secret = guess : res
          | otherwise   = go (guess : res) (filterCodes guess gs)
          where guess = getMove secret g

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
