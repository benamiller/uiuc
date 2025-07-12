module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: (Num a) => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc ins init ka ks = aux ins ka ks
  where
    aux [] k_add k_sub = k_sub (k_add init)
    aux ((Add i):xs) k_add k_sub =
      let new_k_add = \acc -> k_add (acc + i)
      in aux xs new_k_add k_sub
    aux ((Sub i):xs) k_add k_sub =
      let new_k_sub = \acc -> k_sub (acc - i)
      in aux xs k_add new_k_sub
