incList :: [Integer] -> [Integer]
incList [] = []
incList (x:xs) = x+1 : incList xs

sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x:xs) = x + sumList xs

power :: Integer -> Integer -> Integer
power x 0 = 1
power x y = x * power x (y-1)

tailPower :: Integer -> Integer -> Integer

tailPower x y = aux x y 1
  where
    aux x 0 a = a
    aux x y a = aux x (y-1) (a*x)

