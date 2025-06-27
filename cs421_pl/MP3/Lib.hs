--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk x k = factk (x-1) (\v -> k (x * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk list keven kodd = aux list keven kodd
    where
        aux :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
        aux [x] ke ko =
            if even x
                then ke x
                else ko x
        aux (x:xs) ke ko =
            if even x
                then aux xs (\v -> ke (x + v)) ko
                else aux xs ke (\v -> ko (x + v))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (AppExp _ _) = False
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (IfExp cond e1 e2) = isSimple cond && isSimple e1 && isSimple e2

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp exp k n = case exp of
--- #### Define `cpsExp` for Integer and Variable Expressions
    IntExp _ -> (AppExp k exp, n)
    VarExp _ -> (AppExp k exp, n)
--- #### Define `cpsExp` for Application Expressions
    AppExp f e ->
        if isSimple e then
            (AppExp (AppExp f e) k, n)
        else
            let (v, n') = gensym n
                k' = LamExp v (AppExp (AppExp f (VarExp v)) k)
            in cpsExp e k' n'
--- #### Define `cpsExp` for Operator Expressions
    OpExp op e1 e2 ->
        case (isSimple e1, isSimple e2) of
            (True, True) ->
                (AppExp k (OpExp op e1 e2), n)
            (False, True) ->
                let (v, n') = gensym n
                    k' = LamExp v (AppExp k (OpExp op (VarExp v) e2))
                in cpsExp e1 k' n'
            (True, False) ->
                let (v, n') = gensym n
                    k' = LamExp v (AppExp k (OpExp op e1 (VarExp v)))
                in cpsExp e2 k' n'
            (False, False) ->
                let (v1, n1) = gensym n
                    (v2, n2) = gensym n1
                    k_e2 = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                    (e2_trans, n3) = cpsExp e2 k_e2 n2
                    k_e1 = LamExp v1 e2_trans
                in cpsExp e1 k_e1 n3
--- #### Define `cpsExp` for If Expressions
    IfExp cond e1 e2 ->
        if isSimple cond then
            let (t', n1) = cpsExp e1 k n
                (e', n2) = cpsExp e2 k n1
            in (IfExp cond t' e', n2)
        else
            let (v, n1) = gensym n
                (t', n2) = cpsExp e1 k n1
                (e', n3) = cpsExp e2 k n2
                k' = LamExp v (IfExp (VarExp v) t' e')
            in cpsExp cond k' n3

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl = undefined
