{-# LANGUAGE FlexibleContexts, MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \a -> return (f a)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV j k v = liftM2 sub (v !? j) (v !? k)
  where sub x y = v // [(j,y),(k,x)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . fmap f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts i v = mapM (v !?) i

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) <$> getRandomR (0, pred $ V.length v)

-- Exercise 4 -----------------------------------------

randVec :: Monad m => Int -> m a -> m (Vector a)
randVec n = liftM V.fromList . sequence . replicate n

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = randVec n $ getRandom


randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n p = randVec n $ getRandomR p

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = foldM randomSwap v $ reverse [1..pred $ V.length v]
  where randomSwap w i = getRandomR(0,i) >>= swap w i
        swap w i j = return $ w // [(i, w ! j), (j, w ! i)]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> a -> (Vector a, a, Vector a)
partitionAt v n = (less, V.head greater, V.drop 1 greater)
  where
    (less, more) = V.unstablePartition (< n) v
    minId = V.minIndex more
    min   = V.minimum more
    first = more V.! 0
    greater = more V.// [(0,min), (minId,first)]

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | v == V.empty = V.empty
  | otherwise = qsort [ y | y <- rest, y < first]
                <> (V.cons first $ qsort [ y | y <- rest, y >= first])
    where
      (first, rest) = (V.head v, V.drop 1 v)

-- Exercise 8 -----------------------------------------

qsortR :: (Random a, Ord a) => Vector a -> Rnd (Vector a)
-- qsortR = undefined
qsortR v
  | v == V.empty = return V.empty
  | otherwise =
    getRandomR (min, max)   >>=
    return . partitionAt v  >>= \(first, pivot, last) ->
    [ (V.++) a $ V.cons pivot b | a <- qsortR first
                                , b <- qsortR last ]
  where
    min = V.minimum v
    max = V.maximum v

-- Exercise 9 -----------------------------------------

-- Selection
select :: (Random a, Ord a) => Int -> Vector a -> Rnd (Maybe a)
-- select = undefined
select i v
  | i < 0              = return Nothing
  | i > V.length v - 1 = return Nothing
  | otherwise =
    getRandomR (min, max) >>=
    return . partitionAt v >>= \(left, pivot,  right) ->
    let l = V.length left in
      case () of _
                  | i < l     -> select i left
                  | i == l    -> return $ Just pivot
                  | otherwise -> select (i - l - 1) right
    where
      min = V.minimum v
      max = V.maximum v

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | s <- suits
                      , l <- labels ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard v
  | v == V.empty = Nothing
  | otherwise = Just (V.head v, V.tail v)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck
  | n == 0 = Just ([], deck)
  | deck == V.empty = Nothing
  | otherwise = do
    (card, ndeck) <- nextCard deck
    (cs, d) <- getCards (n - 1) ndeck
    return (card:cs, d)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
