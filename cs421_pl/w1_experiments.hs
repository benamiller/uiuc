sqr a = a * a
hypotsq a b = sqr a + sqr b

sqrValue = sqr 10
hypotsqValue = hypotsq 3 4

inc x = x + 1
double x = x * 2
compose f g x = f (g x)

doubleValue = double 10
composeValue = compose inc double 10

twice f x = f (f x)

twiceValue = twice inc 5
twiceValue2 = twice twice inc 4

main :: IO ()
main = do
  print sqrValue
  print hypotsqValue
  print doubleValue
  print composeValue
  print twiceValue
  print twiceValue2
