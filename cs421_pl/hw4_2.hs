0. < t := a + b ; a := t ; b := a , s > || s3 (Seq)

1. < t := a + b , s > || s1 (Assign)
2. < a + b , s > ||e 30 (Arith)
3. < a , s > ||e 10 (Var)
3. < b , s > ||e 20 (Var)

1. < a := t ; b := a , s1 > || s3 (Seq)

2. < a := t , s1 > || s2 (Assign)
3. < t , s1 > ||e 30 (Var)

2. < b := a , s2 > || s3 (Assign)
3. < a , s2 > ||e 30 (Var)


s  = {a := 10 , b := 20}
s1 = {a := 10 , b := 20 , t := 30}
s2 = {a := 30 , b := 20 , t := 30}
s3 = {a := 30 , b := 30 , t := 30}
