dotProduct :: [Integer] -> [Integer] -> Integer

dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = (x * y) + dotProduct xs ys
