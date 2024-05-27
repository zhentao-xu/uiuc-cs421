--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n _  | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x: mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n xs | n <= 0 = xs
mydrop _ [] = []
mydrop n (x:xs) = mydrop (n-1) xs


--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = tail_rev xs []
    where
        tail_rev [] acc = acc
        tail_rev (x:xs) acc = tail_rev xs (x:acc)


--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x: (app xs ys)

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : (inclist xs)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist ::Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)] 
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y): (myzip xs ys)

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = go (myzip xs ys)
    where
        go [] = []
        go ((x, y):xs) = (x+y) : (go xs)

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1: ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats  = go 0
    where 
        go x = x: go (x+1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = go 0 1
    where
        go x y = x: (go y (x + y)) 

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a] 
add x [] = [x]
add v (x:xs) = if v < x then v : (x : xs)
               else if v == x then (x:xs)
               else x : (add v xs)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] xs = xs
union xs [] = xs
union (x:xs) (y:ys) =   if x == y then x: (union xs ys)
                        else if x < y then x: (union xs (y:ys))
                        else y : (union (x:xs) ys)


--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] ys = []
intersect xs [] = []
intersect (x:xs) (y:ys)  =  if x == y then x : (intersect xs ys)
                            else if x < y then intersect xs (y:ys)
                            else intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]] 
powerset [] = [[]]
powerset (x:xs) = union (powerset xs) (P.map (\list -> x : list) (powerset xs))

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a] 
inclist' xs = P.map (\v -> v + 1) xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' xs = P.foldl (+) 0 xs
