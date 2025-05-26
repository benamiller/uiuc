mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (a:as) = f a : mymap f as

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f x [] = x
myfoldl f x (y:ys) = myfoldl f (f x y) ys

palindromeCheck :: [String] -> [Bool]
palindromeCheck [] = []
palindromeCheck list = map palindromeCheckHelp list
palindromeCheckHelp string = string == reverse string

declist :: Num a => [a] -> [a]
declist [] = []
declist list = map (\x -> x - 1) list

data Turtle a = Down (Turtle a) (Turtle a) | Up a
  deriving show

listifyTurtle :: Turtle a -> [a]
listifyTurtle (Up val) = [val]
listifyTurtle (Down left right) = listifyTurtle left ++ listifyTurtle right

isBalanced :: Turtle a -> Bool
isBalanced (Up _) = True
isBalanced (Down left right) =
  let balanced = isBalanced left && isBalanced right
    sameType = case (left, right) of
      (Up _, Up _) -> True
      (Down _ _, Down _ _) -> True
      _ -> False
  in balanced && sameType
