{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) fibs2 $ tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- Exercise 4 -----------------------------------------
-- Monoid doesn't make sense becouse there is no empty element for Streams?

instance Functor Stream where
    fmap f (Cons a b) = Cons (f a) (fmap f b)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f $ f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a b) c = Cons a $ sInterleave c b

sTake :: Int -> Stream a -> [a]
sTake n a = go 0 n a
  where go :: Int -> Int -> Stream a -> [a]
        go acc k (Cons x y)
          | acc == k = []
          | otherwise = x:(go (acc + 1) k y)

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1 +) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) $ sInterleave (sRepeat 1) $ sInterleave (sRepeat 2) $ sInterleave (sRepeat 3) (sRepeat 4)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r = sIterate gen $ gen r
  where gen :: Int -> Int
        gen n = ((1103515245 * n) + 12345) `mod`2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 190 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
