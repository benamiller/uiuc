incList :: [Integer] -> [Integer]
incList [] = []
incList (x:xs) = x+1 : incList xs

sumList :: Num t => [t] -> t
sumList = undefined
