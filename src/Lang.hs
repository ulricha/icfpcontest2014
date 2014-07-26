module Lang where

import Control.Monad.State

import Gcc
import GccMacros

--------------------------------------------------------------------------------
-- Language definition

type Ident = String

type Binding = (Ident, [Ident], Expr)

data Val = IntV Int
         | NilV
         deriving (Show)

data Expr = Let Ident Expr
          | App2 BinOp Expr Expr
          | App1 UnOp Expr
          | Cond Expr Expr Expr
          | Lit Val
          | Var Ident
          deriving (Show)

data Prog = Letrec [Binding] Expr deriving (Show)

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Eq
           | Gt
           | GtE
           | Cons
           deriving (Show)

data UnOp = Car
          | Cdr
          | IsNil
          deriving (Show)

binOp :: BinOp -> GccProgram String ()
binOp o = case o of
    Add  -> add
    Sub  -> sub
    Mul  -> mul
    Div  -> div_
    Eq   -> ceq
    Gt   -> cgt
    GtE  -> cgte
    Cons -> cons

unOp :: UnOp -> GccProgram String ()
unOp o = case o of
    Car   -> car
    Cdr   -> cdr
    IsNil -> macro_isnil

--------------------------------------------------------------------------------
-- Constructors

not :: Expr -> Expr
not e = Cond (App2 Eq e (int 0)) (int 1) (int 0)

and :: Expr -> Expr -> Expr
and e1 e2 = Cond (App2 Eq e1 (int 0)) (int 0) e2

or :: Expr -> Expr -> Expr
or e1 e2 = Cond (App2 Eq e1 (int 0)) e2 (int 1)

int :: Int -> Expr
int = Lit . IntV

nil :: Expr
nil = Lit NilV


--------------------------------------------------------------------------------
-- SECD^WGCC compiler

data CompileState = CS { labelSupply :: Int }

freshLabel :: Compile String
freshLabel = do 
    s <- get
    put $ s { labelSupply = labelSupply s + 1 }
    return $ "label" ++ show (labelSupply s)

type Compile a = StateT CompileState (GccProgram String) a

{-
if c t e =>
<<compile c>>

sel <true> <false>

<true>:
<<compile t>>
join

<false>:
<<compile e>>
join

-}

compile :: Expr -> Compile ()
compile (Lit (IntV i)) = lift $ ldc i
compile (Lit NilV)     = lift macro_nil
compile (App2 o e1 e2) = do
    compile e1
    compile e2
    lift $ binOp o
compile (App1 o e)     = do
    compile e
    lift $ unOp o
compile (Cond c t e)   = do
    compile c
    thenLabel <- freshLabel
    elseLabel <- freshLabel
    lift $ sel thenLabel elseLabel

    lift $ label thenLabel
    compile t
    lift join_

    lift $ label elseLabel
    compile e
    lift join_

initCompileState :: CompileState
initCompileState = CS 0

doCompile :: Expr -> GccProgram String ()
doCompile e = evalStateT (compile e) initCompileState
    
