fun1 [] = 0
fun1 (x:xs) | even x = fun1 xs - 1 | odd x = fun1 xs + 1

fun2 1 = 0
fun2 n = 1 + fun2 (n `div` 2)

fun3 1 = 1
fun3 2 = 1
fun3 n = fun3 (n-1) + fun3 (n-2)
