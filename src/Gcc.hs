{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free

data GccInst label
    = LDC Int
    | LD Int Int
    | ADD
    | SUB
    | MUL
    | DIV
    | CEQ
    | CGT
    | CGTE
    | ATOM
    | CONS
    | CAR
    | CDR
    | SEL
    | JOIN
    | LDF label
    | AP label
    | RTN
    | DUM Int
    | RAP Int
    deriving (Show)
    -- | LABEL label
     
data GccInstruction label cont
    = Inst (GccInst label) cont
    | Stop
    deriving (Show, Functor)

instList :: GccProgram a -> [GccInst Int]
instList (Pure _)          = []
instList (Free (Inst i c)) = i : instList c

type GccProgram = Free (GccInstruction Int)
type GccProg = Free (GccInstruction String)


ldc :: Int -> GccProgram ()
ldc n = liftF $ Inst (LDC n) ()

ld :: Int -> Int -> GccProgram ()
ld n i = liftF $ Inst (LD n i) ()

add :: GccProgram ()
add = liftF $ Inst ADD ()

sub :: GccProgram ()
sub = liftF $ Inst SUB ()

mul :: GccProgram ()
mul = liftF $ Inst MUL ()

div :: GccProgram ()
div = liftF $ Inst DIV ()

ceq :: GccProgram ()
ceq = liftF $ Inst CEQ ()

cgt :: GccProgram ()
cgt = liftF $ Inst CGT ()

cgte :: GccProgram ()
cgte = liftF $ Inst CGTE ()

atom :: GccProgram ()
atom = liftF $ Inst ATOM ()

cons :: GccProgram ()
cons = liftF $ Inst CONS ()

car :: GccProgram ()
car = liftF $ Inst CAR ()

cdr :: GccProgram ()
cdr = liftF $ Inst CDR ()

sel :: GccProgram ()
sel = liftF $ Inst SEL ()

join_ :: GccProgram ()
join_ = liftF $ Inst JOIN ()

ldf :: Int -> GccProgram ()
ldf i = liftF $ Inst (LDF i) ()

ap :: Int -> GccProgram ()
ap i = liftF $ Inst (AP i) ()

rtn :: GccProgram ()
rtn = liftF $ Inst RTN ()

dum :: Int -> GccProgram ()
dum i = liftF $ Inst (DUM i) ()

rap :: Int -> GccProgram ()
rap i = liftF $ Inst (RAP i) ()

stop :: GccProgram ()
stop = liftF $ Stop


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

codeGen :: GccProgram a -> [String]
codeGen p = map showInst $ instList p

showInst :: Show a => GccInst a -> String
showInst (LDC n) = ("LDC " ++ show n)
showInst (LD n i) = ("LCD " ++ show n ++ " " ++ show i)
showInst (ADD) = "ADD "
showInst (SUB) = "SUB "
showInst (DIV) = "DIV "
showInst (MUL) = "MUL "
showInst (CEQ) = "CEQ "
showInst (CGT) = "CGT "
showInst (CGTE) = "CGTE "
showInst (ATOM) = "ATOM "
showInst (CONS) = "CONS "
showInst (CAR) = "CAR "
showInst (CDR) = "CDR "
showInst (SEL) = "SEL "
showInst (JOIN) = "JOIN "
showInst (LDF n) = ("LDF " ++ show n)
showInst (AP n) = ("AP " ++ show n)
showInst (RTN) = "RTN "
showInst (DUM n) = ("DUM " ++ show n)
showInst (RAP n) = ("RAP " ++ show n)


----------------------------------------------------------------------
-- run time
----------------------------------------------------------------------

data DataStack
data ControlStack
data EnvironmentFrame
data DataHeap

data GccProgState = GPS { ds :: DataStack
                        , cs :: ControlStack
                        , ef :: EnvironmentFrame
                        , dh :: DataHeap
                        }


----------------------------------------------------------------------
-- docks
----------------------------------------------------------------------

{-
stupidAI :: GccProg ()
stupidAI = do
    ldc 4
    ldf "body"
    stop
    -- label "body"
    ldc 5
    rtn
-}


{-

0 ldc 4
1 jmp 4
; body:
2 ldc 5
3 rtn
4 ldf 2
5 stop

-}



{-

main :: IO ()
-- main = putStrLn $ unlines $ codeGen stupidAI
main = putStrLn "foo"
-}
