{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P p) == (P q) = dropWhileEnd (== 0) p == dropWhileEnd (== 0) q

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show a = intercalate " + " (go 0 a)
      where go :: (Num a, Eq a, Show a) => Int -> Poly a -> [String]
            go _   (P [])     = []
            go 0   (P (z:[])) = [show z]
            go 1   (P (z:[])) = [show z ++ "x"]
            go acc (P (z:[]))
              | z == 0    = []
              | otherwise = [(show z) ++ "x^" ++ (show acc)]
            go acc (P (z:zs))
              | z == 0    = go (acc+1) (P zs)
              | otherwise = go (acc+1) (P zs) ++ go acc (P [z])

-- Exercise 4 -----------------------------------------

getList :: Poly a -> [a]
getList (P z) = z

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P (p:ps)) (P (q:qs)) = (P ((p + q) : getList (plus (P ps) (P qs))))
plus (P [])     (P qs)     = (P (zipWith (+) (repeat 0) qs))
plus (P ps)     (P [])     = (P (zipWith (+) (repeat 0) ps))

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P p) (P q) = sum (go 0 (P p) (P q))
  where go :: Num a => Int -> Poly a -> Poly a -> [Poly a]
        go _ (P [])     _      = []
        go _ _          (P []) = []
        go n (P (r:rs)) (P s)  = (P (map (* r) ((take n $ repeat 0) ++ s))) : (go (n+1) (P rs) (P s))

-- Exercise 6 -----------------------------------------

neg :: Num a => Poly a -> Poly a
neg (P []) = (P [])
neg (P (p:ps)) = (P (-p : getList( neg (P ps))))

fromInt :: Num a => Integer -> Poly a
fromInt z = (P [(fromInteger z)])

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = neg
    fromInteger = fromInt
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = go 0
  where go :: Num a => Int -> Poly a -> a -> a
        go _ (P [])     _ = 0
        go n (P (q:qs)) a = (a ^ n * q) + (go (n+1) (P qs) a)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f
      | n > 0 = deriv (nderiv (n-1) f)
      | otherwise = f

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = go 0
      where go :: Num a => Int -> Poly a -> Poly a
            go _ (P []) = (P [])
            go n (P (p:ps))
              | n == 0    = go (n+1) (P ps)
              | otherwise = (P (((fromIntegral n) * p) : (getList $ go (n+1) (P ps))))

