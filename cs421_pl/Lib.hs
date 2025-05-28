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
-- mytake :: Int -> [a] -> [a]
-- mytake 0 xs = xs
-- mytake n (x:xs) = mytake (n-1) xs
mytake :: Int -> [a] -> [a]
mytake n _  | n <= 0 = []
mytake _ [] = []
mytake n (x:xs) = x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n xs | n <= 0 = xs
mydrop _ [] = []
mydrop n (x:xs) = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = aux xs []
  where
    aux [] acc = acc
    aux (x:xs) acc = aux xs (x:acc)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] b = b
app (a:as) b = a : app as b

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x+1 : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist list = sumlistacc list 0
sumlistacc [] _ = 0
sumlistacc (x:xs) acc = sumlistacc xs acc+x

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip [] b = []
myzip a [] = []
myzip (a:as) (b:bs) = (a,b) : myzip as bs

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairsHelp :: Num a => [(a, a)] -> [a]
addpairsHelp [] = []
addpairsHelp ((a, b):pairs) = a+b : addpairsHelp pairs

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a b =
  let paired = myzip a b
  in addpairsHelp paired

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0,1..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
-- fib :: [Integer]
-- fib = fibHelp 0 1
-- fibHelp a b = a : fibHelp b (a+b)

-- cooler fib with tail
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add a (y:ys)
  | a < y = a : y : ys
  | a == y = y : ys
  | a > y = y : add a ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union x [] = x
union [] y = y
union (x:xs) (y:ys)
  | x < y = x : union xs (y:ys)
  | x == y = x : union xs ys
  | x > y = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect x [] = []
intersect [] y = []
intersect (x:xs) (y:ys)
  | x == y = x : intersect xs ys
  | x < y = intersect xs (y:ys)
  | y < x = intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
-- powerset :: Ord a => [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = 

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' [] = []
inclist' x = P.map (+1) x

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' = undefined
