incList :: [Integer] -> [Integer]
incList [] = []
incList (x:xs) = x+1 : incList xs

sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x:xs) = x + sumList xs

power :: Integer -> Integer -> Integer
power x 0 = 1
power x y = x * power x (y-1)
