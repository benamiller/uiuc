sqr a = a * a
hypotsq a b = sqr a + sqr b

sqrValue = sqr 10
hypotsqValue = hypotsq 3 4

main :: IO ()
main = do
  print sqrValue
  print hypotsqValue
