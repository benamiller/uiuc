0. < if x < y then m:=x*x else m:=y*y fi, s> || s2 (If1)
1. < x < y, s > ||b true (Comp)
2. < x, s> ||e 10 (Var)
2. < y, s> ||e 20 (Var)
1. < m:=x*x, s > || s2 (Assign)
2. < x*x, s > ||e 100 (Arith)
3. < x, s > ||e 10 (Var)
3. < x, s > ||e 10 (Var)

s = {x:=10,y:=20}
s2 = {x:=10,y:=20,m:=100}
