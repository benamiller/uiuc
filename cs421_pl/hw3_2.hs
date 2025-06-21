data Exp = IntExp Integer
         | IntOpExp String Exp Exp
         | FunExp [String] Exp
         | AppExp Exp [Exp]
         | VarExp String
   deriving(Show,Eq)

type Env = [(String, Val)]

data Val = IntVal Integer
         | Closure [String] Exp Env
         | ExnVal String
   deriving(Show, Eq)

intOps = [("+",(+)), ("-", (-)), ("*", (*)), ("/", (div))]

liftIntOp :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp _ _           _           = ExnVal "not and IntVal!"

insert :: String -> Val -> Env -> Env
insert s v env = (s,v):env

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = lookup op intOps
     in liftIntOp f v1 v2

eval (VarExp var) env = 
    case lookup var env of 
        Just val -> val
        Nothing -> IntVal 0
