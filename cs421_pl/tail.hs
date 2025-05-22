fun1 [] = 0
fun1 (x:xs) | even x = fun1 xs - 1 | odd x = fun1 xs + 1

fun2 1 = 0
fun2 n = 1 + fun2 (n `div` 2)

fun3 1 = 1
fun3 2 = 1
fun3 n = fun3 (n-1) + fun3 (n-2)

fun1_tail xx = aux xx 0
  where
    aux [] a = a
    aux (x:xs) a | even x = aux xs (a-1) | odd x = aux xs (a+1)
