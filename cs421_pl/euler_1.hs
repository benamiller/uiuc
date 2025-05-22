inc :: Integer -> Integer
inc x = x + 1

euler1 = sumMods [1..999]
    where
        mod3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0
        sumMods xx = sum (filter mod3or5 xx)