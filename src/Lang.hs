{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, OverloadedStrings #-}

module Lang where

import Control.Monad.RWS
import Data.Maybe
import Control.Applicative

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

binOp :: BinOp -> GccInst String
binOp o = case o of
    Add  -> ADD
    Sub  -> SUB
    Mul  -> MUL
    Div  -> DIV
    Eq   -> CEQ
    Gt   -> CGT
    GtE  -> CGTE
    Cons -> CONS

unOp :: UnOp -> GccInst String
unOp o = case o of
    Car   -> CAR
    Cdr   -> CDR
    IsNil -> undefined

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

-- type Compile a = StateT CompileState (GccProgram String) a

type Env = [(Ident, Int)]

type Compile a = RWS Env [GccInst String] CompileState a

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

compile :: Expr -> Compile [GccInst String]
compile (Let ident e1 e2) = do
    compile $ AppL (Lambda [ident] e2) [e1]

compile (App2 o e1 e2) = do
    c1 <- compile e1
    c2 <- compile e2
    return $ binOp o : (c1 ++ c2)

compile (App1 o e) = do
    c <- compile e
    return $ unOp o : c

compile (Cond c t e) = do
    cc <- compile c
    
    thenLabel <- toSection t [JOIN]
    elseLabel <- toSection e [JOIN]

    return $ cc ++ [SEL thenLabel elseLabel]

compile (Lambda args body) = do
    let argEnv = zip args [1..]
    bodyLabel <- local (++ argEnv) $ toSection body [RTN]
    return [LDF bodyLabel]
    
compile (Lit (IntV i)) = return $ [LDC i]
compile (Lit NilV) = return $ [LDC 0xdeadbeef]

compile (Var ident) = do
    -- FIXME get rid of fromJust
    varPos <- fromJust <$> (asks $ lookup ident)
    return $ [LD undefined varPos]

compile (Lambda idents expr) = do
    error "compile.Lambda"

compile (AppL e1 e2) = do
    error "compile.AppL"

toSection :: Expr -> [GccInst String] -> Compile String
toSection e suffix = do
    lab     <- freshLabel
    section <- compile e
    tell $ LABEL lab : section ++ suffix
    return $ lab

initCompileState :: CompileState
initCompileState = CS 0

doCompile :: Expr -> [GccInst String]
doCompile e = main ++ sections
  where
    (main, sections) = evalRWS (compile e) [] initCompileState
