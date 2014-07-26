{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, OverloadedStrings #-}

module Lang where

import Control.Monad.State

import Gcc
import GccMacros

import GHC.Exts (IsString(..))

--------------------------------------------------------------------------------
-- Language definition

type Ident = String

data Val = IntV Int
         | NilV
         deriving (Show)

data Expr = Let Ident Expr Expr
          | App2 BinOp Expr Expr
          | App1 UnOp Expr
          | Cond Expr Expr Expr
          | Lit Val
          | Var Ident
          | Lambda [Ident] Expr
          | AppL Expr [Expr]
          deriving (Show)

int :: Int -> Expr
int = Lit . IntV

nil :: Expr
nil = Lit NilV

data Prog = Letrec [(Ident, Expr)] Expr deriving (Show)

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

binApp :: BinOp -> Expr -> Expr -> Expr
binApp op = App2 op

instance Num Expr where
    (+) = binApp Add
    (-) = binApp Sub
    (*) = binApp Mul
    abs = undefined
    fromInteger = Lit . IntV . fromInteger
    signum = undefined

(.>), (.>=), (.==) :: Expr -> Expr -> Expr
(.>) = binApp Gt
(.>=) = binApp GtE
(.==) = binApp Eq

not_ :: Expr -> Expr
not_ e = Cond (App2 Eq e 0) 1 0

(.&&) :: Expr -> Expr -> Expr
(.&&) e1 e2 = Cond (App2 Eq e1 0) 0 e2

(.||) :: Expr -> Expr -> Expr
(.||) e1 e2 = Cond (App2 Eq e1 0) e2 1

(?) :: Expr -> (Expr, Expr) -> Expr
cond ? (thenCase, elseCase) = Cond cond thenCase elseCase


class FunApp f args | f -> args where
    (.$.) :: f -> args -> Expr

instance FunApp BinOp (Expr, Expr) where
    f .$. (x, y) = App2 f x y

instance FunApp UnOp Expr where
    f .$. x = App1 f x

instance FunApp Expr [Expr] where
    f .$. xs = AppL f xs

instance IsString Expr where
    fromString = Var
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
compile (Let ident e1 e2) = do
    compile $ AppL (Lambda [ident] e2) [e1]

compile (App2 o e1 e2) = do
    compile e1
    compile e2
    lift $ binOp o

compile (App1 o e) = do
    compile e
    lift $ unOp o

compile (Cond c t e) = do
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

compile (Lit (IntV i)) = lift $ ldc i
compile (Lit NilV) = lift macro_nil

compile (Var ident) = do
    error "compile.Var"

compile (Lambda idents expr) = do
    error "compile.Lambda"

compile (AppL e1 e2) = do
    error "compile.AppL"

initCompileState :: CompileState
initCompileState = CS 0

doCompile :: Expr -> GccProgram String ()
doCompile e = evalStateT (compile e) initCompileState
