incList :: [Integer] -> [Integer]
incList [] = []
incList (x:xs) = x+1 : incList xs

sumList :: Num t => [t] -> t
sumList [] = 0
sumList (x:xs) = x + sumList xs

