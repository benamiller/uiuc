mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (a:as) = f a : mymap f as

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f x [] = x
myfoldl f x (y:ys) = myfoldl f (f x y) ys
