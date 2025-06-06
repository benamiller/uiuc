sqr a = a * a
hypotsq a b = sqr a + sqr b

sqrValue = sqr 10
hypotsqValue = hypotsq 3 4

-- inc x = x + 1
inc = \x -> x + 1
double x = x * 2
compose f g x = f (g x)

incValue = inc 3
doubleValue = double 10
composeValue = compose inc double 10

twice f x = f (f x)

twiceValue = twice inc 5
twiceValue2 = twice twice inc 4

incL [] = []
incL (x:xs) = x+1 : incL xs

incLValue = incL [1,2,3]

doubleL [] = []
doubleL (x:xs) = x*2 : doubleL xs

doubleLValue = doubleL [1,2,3]

mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

incLMap = mymap inc

doubleLMap = mymap double

incLMapValue = incLMap [1,2,3]
doubleLMapValue = doubleLMap [1,2,3]

myfoldr f z [] = z
myfoldr f z (x:xs) = f x (foldr f z xs)

sumList = myfoldr (+) 0
prodList = myfoldr (*) 1

sumListValue = sumList [1,2,3,4]
prodListValue = prodList [1,2,3,4]

main :: IO ()
main = do
  print sqrValue
  print hypotsqValue
  print incValue
  print doubleValue
  print composeValue
  print twiceValue
  print twiceValue2
  print ((\x -> x + 1) 41)
  print incLValue
  print doubleLValue
  print incLMapValue
  print doubleLMapValue
  print sumListValue
  print prodListValue
