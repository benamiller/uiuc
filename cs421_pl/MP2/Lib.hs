module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i 
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of
    Just val -> val
    Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (v1, v2) of
        (ExnVal _, _) -> v1
        (_, ExnVal _) -> v2
        (IntVal i1, IntVal i2) ->
            if op == "/" && i2 == 0
                then ExnVal "Cannot divide by 0"
                else liftIntOp (fromJust $ H.lookup op intOps) v1 v2
        _ -> ExnVal "Unliftable"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (v1, v2) of
        (ExnVal _, _) -> v1
        (_, ExnVal _) -> v2
        (BoolVal _, BoolVal _) -> liftBoolOp (fromJust $ H.lookup op boolOps) v1 v2
        _ -> ExnVal "Unliftable"

eval (CompOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (v1, v2) of
        (ExnVal _, _) -> v1
        (_, ExnVal _) -> v2
        (IntVal _, IntVal _) -> liftCompOp (fromJust $ H.lookup op compOps) v1 v2
        _ -> ExnVal "Unliftable"

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    case eval e1 env of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        ExnVal s -> ExnVal s
        _ -> ExnVal "Condition not Bool"

--- ### Functions and Function Application

checkExceptions :: [Val] -> Maybe Val
checkExceptions [] = Nothing
checkExceptions (v@(ExnVal _):_) = Just v
checkExceptions (_:vs) = checkExceptions vs

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env =
    case eval e1 env of
        CloVal params body cloEnv ->
            let argVals = map (`eval` env) args
            in case checkExceptions argVals of
                Just exn -> exn
                Nothing ->
                    let newBindings = H.fromList $ zip params argVals
                        newEnv = H.union newBindings cloEnv
                    in eval body newEnv
        ExnVal s -> ExnVal s
        _ -> ExnVal "No closure"

--- ### Let Expressions

eval (LetExp pairs body) env =
    let newVals = map (\(_, e) -> eval e env) pairs
    in case checkExceptions newVals of
        Just exn -> exn
        Nothing ->
            let newNames = map fst pairs
                newBindings = H.fromList $ zip newNames newVals
                newEnv = H.union newBindings env
            in eval body newEnv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env =
    let val = eval e env
    in case val of
        ExnVal s -> (show val, penv, env)
        _ -> ("", penv, H.insert var val env)

--- ### Sequencing

exec (SeqStmt statements) penv env =
    foldl'
        (\(out, p, ev) statement ->
            let (newOut, newP, newEv) = exec statement p ev
            in (out ++ newOut, newP, newEv)
        )
        ("", penv, env)
        statements
    where
        foldl' f z [] = z
        foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
    case eval e1 env of
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        ExnVal s -> (s, penv, env)
        _ -> ("Non-bool probably", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env =
    let newPenv = H.insert name p env
    in ("", newPenv, env)

exec (CallStmt name args) penv env =
    case H.lookup name penv of
        Nothing -> ("Procedure " ++ name ++ " is totally undefined", penv, env)
        Just (ProcedureStmt _ params body) ->
            let argVals = map (`eval` env) args
            in case checkExceptions argVals of
                Just (ExnVal s) -> ("Exn: " ++ s, penv, env)
                _ ->
                    let newBindings = H.fromList $ zip params argVals
                        callEnv = H.union newBindings env
                    in exec body penv callEnv
        _ -> ("Found a non-procedure, but weird", penv, env)
