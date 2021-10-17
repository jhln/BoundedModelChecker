{-# LANGUAGE DeriveFoldable #-}

module Sets where

import Prelude hiding (elem)
import Data.List (sort)
import Data.Foldable (Foldable)
{-- Sets implemented as ordered lists without duplicates --}

newtype Set a = Set [a] deriving (Foldable)

instance (Show a) => Show (Set a)
  where
    showsPrec _ (Set s) str = showSet s str

instance (Eq a, Ord a) => Eq (Set a) where
  --Set as1 == Set as2 = and $ zipWith (==) as1 as2
  Set as1 == Set as2 = as1 == as2

instance (Eq a, Ord a) => Ord (Set a) where
  Set as1 <= Set as2 = as1 <= as2

setSize :: Set a -> Int
setSize (Set as) = length as


showSet [] str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
  where
    showl [] str = showChar '}' str
    showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _ = False

inSet :: (Ord a) => a -> Set a -> Bool
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = inSet x set && subSet (Set xs) set

insertSet :: (Ord a) => a -> Set a -> Set a
insertSet x (Set s) = Set (insertList x s)

insertList x [] = [x]
insertList x ys@(y:ys') =
  case compare x y of
    GT -> y : insertList x ys'
    EQ -> ys
    _ -> x : ys

deleteSet :: Ord a => a -> Set a -> Set a
deleteSet x (Set s) = Set (deleteList x s)
deleteList x [] = []
deleteList x ys@(y:ys') =
  case compare x y of
    GT -> y : deleteList x ys'
    EQ -> ys'
    _ -> ys

list2set :: Ord a => [a] -> Set a
list2set = foldr insertSet emptySet
-- list2set xs = Set (foldr insertList [] xs)

set2List :: Set a -> [a]
set2List (Set a) = a

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) =
  Set (sort (map list2set (powerList xs)))

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) =
  powerList xs ++ map (x:) (powerList xs)

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n


union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys = x : union xs (delete x ys)

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] s = []
intersect (x:xs) s
  | elem x s = x : intersect xs s
  | otherwise = intersect xs s

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys)
  | x == y = ys
  | otherwise = y : delete x ys

elem, notElem :: Eq a => a -> [a] -> Bool
elem = any . (==)
notElem = all . (/=)