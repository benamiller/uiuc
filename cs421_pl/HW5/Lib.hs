module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: (Num a, Ord a) => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc xx init ka ks = aux init xx
     where aux a [] = ks a
           aux a ((Add i):xs) = aux (a + i) xs
           aux a ((Sub i):xs)
               | a >= i = aux (a - i) xs
               | otherwise = ka a
