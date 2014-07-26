{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}

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

data Prog = Letrec [(Ident, Expr)] Expr
          | Expr Expr
          deriving (Show)

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

binOp :: BinOp -> GccInst
binOp o = case o of
    Add  -> ADD
    Sub  -> SUB
    Mul  -> MUL
    Div  -> DIV
    Eq   -> CEQ
    Gt   -> CGT
    GtE  -> CGTE
    Cons -> CONS

unOp :: UnOp -> GccInst
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

--------------------------------------------------------------------------------
-- Environments


type ScopeDepth = Int
type FrameField = Int
type Frame      = Int
type FrameRef   = (Frame, FrameField)

type EnvFrame = [(Ident, ScopeDepth -> FrameRef)]

type Env = [EnvFrame]

extendEnv :: [Ident] -> Env -> Env
extendEnv args frames = localFrame : frames
  where 
    localFrame = zipWith (\a i -> (a, \d -> (d, i))) args [0..]

enterScope :: [Ident] -> Compile a -> Compile a
enterScope args m = local (\e -> extendEnv args e) m

searchFrames :: Ident -> Env -> Maybe FrameRef
searchFrames name env = go 0 env name
  where
    go :: ScopeDepth -> Env -> Ident -> Maybe FrameRef
    go i (frame : frames) name = 
        case lookup name frame of
            Just ref -> Just $ ref i
            Nothing  -> go (i + 1) frames name
    go _ [] _ = Nothing

lookupEnv :: Ident -> Compile (Maybe FrameRef)
lookupEnv name = asks (searchFrames name)

type Compile a = RWS Env [GccInst] CompileState a

compile :: Expr -> Compile [GccInst]
compile (Let ident e1 e2) = do
    compile $ AppL (Lambda [ident] e2) [e1]

compile (App2 o e1 e2) = do
    c1 <- compile e1
    c2 <- compile e2
    return $ c1 ++ c2 ++ [binOp o]

compile (App1 o e) = do
    c <- compile e
    return $ c ++ [unOp o]

compile (Cond c t e) = do
    cc <- compile c
    
    thenLabel <- toSection t [JOIN]
    elseLabel <- toSection e [JOIN]

    return $ cc ++ [SEL thenLabel elseLabel]

compile (Lambda args body) = do
    let argEnv = zip args [1..]
    bodyLabel <- enterScope args $ toSection body [RTN]
    return [LDF bodyLabel]
    
compile (Lit (IntV i)) = return $ [LDC i]
compile (Lit NilV) = return $ [LDC 0xdeadbeef]

compile (Var name) = do
    -- FIXME get rid of fromJust
    (frame, frameField) <- fromJust <$> lookupEnv name
    return $ [LD frame frameField]

compile (AppL fun es) = do
    -- Note: partial application is not allowed
    let arity = length es
    argsCode <- concat <$> mapM compile es
    funCode  <- compile fun
    return $ argsCode ++ funCode ++ [AP arity]

compileProg :: Prog -> Compile [GccInst]
compileProg (Letrec bindings body) = do
    let nrBindings = length bindings
        names      = map fst bindings
    bindingsCode <- concat <$> mapM (\(n, e) -> enterScope names $ compile e) bindings
    
    mainLabel    <- enterScope names $ toSection body [RTN]

    return $ [DUM nrBindings] 
             ++ bindingsCode 
             ++ [LDF mainLabel, RAP nrBindings]

compileProg (Expr e) = compile e
    
     
{-
letrec x = e1
       y = e2
       z = e3
in e
=>
enrich environment as follows:
create a new env frame in which all local letrec names are bound:
env' = [x = \i -> ld i 0
       ,y = \i -> ld i 1
       ,z = \i -> ld i 2
       ] : env

if bindings refer to lambda expressions, further frames will be
created for those.  

AND

DUM 3   ; Number of bindings in letrec
<<e1>>  ; compile in env'
<<e2>>  ; compile in env'
<<e3>>  ; compile in env'
LDF main ; load function wrapper for main expression
RAP 3   ; number of bindings in letrec
RTN
main:
<<e>>   ; compile in env'
RTN
-} 

    
toSection :: Expr -> [GccInst] -> Compile String
toSection e suffix = do
    lab     <- freshLabel
    section <- compile e
    tell $ LABEL lab : section ++ suffix
    return $ lab

initCompileState :: CompileState
initCompileState = CS 0

initEnv :: Env
initEnv = []

doCompile :: Prog -> [GccInst]
doCompile e = main ++ [RTN] ++ sections
  where
    (main, sections) = evalRWS (compileProg e) initEnv initCompileState

test1 :: Expr
test1 = Let "f" (Lambda ["x"] ("x" + "x")) (Var "f" .$. [42])

test2 :: Prog
test2 = Letrec [ ("to", Lambda ["x"] (Var "go" .$. [Var "x" - 1]))
               , ("go", Lambda ["y"] (Var "to" .$. [Var "y" + 1]))
               ]
               (Var "go" .$. [1])
