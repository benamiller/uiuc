sqr a = a * a
hypotsq a b = sqr a + sqr b

sqrValue = sqr 10
hypotsqValue = hypotsq 3 4

inc x = x + 1
double x = x * 2
compose f g x = f (g x)

doubleValue = double 10
composeValue = compose inc double 10

main :: IO ()
main = do
  print sqrValue
  print hypotsqValue
  print doubleValue
  print composeValue
