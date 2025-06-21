data Exp = IntExp Integer
         | IntOpExp String Exp Exp
         | BoolExp Bool
         | BoolOpExp String Exp Exp
         | IfExp Exp Exp Exp
         | VarExp String
   deriving(Show,Eq)

type Env = [(String, Val)]

data Val = IntVal Integer
         | BoolVal Bool
         | ExnVal String
   deriving(Show, Eq)

intOps :: [([Char], Integer -> Integer -> Integer)]
intOps = [("+",(+)), ("-", (-)), ("*", (*)), ("/", (div))]

boolOps :: [([Char], Bool -> Bool -> Bool)]
boolOps = [("||",(||)), ("&&", (&&))]

liftIntOp :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp _ _           _           = ExnVal "not and IntVal!"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal b1) (BoolVal b2) = BoolVal (f b1 b2)
liftBoolOp _ _            _            = BoolVal False

insert :: String -> Val -> Env -> Env
insert s v env = (s,v):env

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = lookup op intOps
     in liftIntOp f v1 v2

eval (BoolExp b) _ = BoolVal b
eval (BoolOpExp op e1 e2) env  = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = lookup op boolOps
     in liftBoolOp f v1 v2

eval (VarExp var) env = 
    case lookup var env of 
        Just val -> val
        Nothing -> IntVal 0

eval (IfExp cond e1 e2) env =
    let boolval = eval cond env
    in case boolval of
        BoolVal True -> eval e1 env
        BoolVal False -> eval e2 env
        IntVal 0 -> eval e2 env
        IntVal _ -> eval e1 env
        exnVal -> exnVal
