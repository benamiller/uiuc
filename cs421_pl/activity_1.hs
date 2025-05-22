inc :: Integer -> Integer
inc a = a + 1


fibValue = fib 6
  where
    fib 0 = 0
    fib 1 = 1
    fib 2 = 1
    fib 3 = 2
    fib n = fib(n - 1) + fib(n - 2)


summedList = sumList [1,2,3]
  where
    sumList x = sum (x)


incedList = incList [1,2,3]
  where
    incList l = map inc (l)
