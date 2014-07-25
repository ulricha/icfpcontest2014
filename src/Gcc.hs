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
    | LABEL label
    deriving (Show)
     
data GccInstruction label cont
    = Inst (GccInst label) cont
    | Stop
    deriving (Show, Functor)

type GccProgram l = Free (GccInstruction l)
type GccProg = GccProgram String

ldc :: Int -> GccProgram l ()
ldc n = liftF $ Inst (LDC n) ()

ld :: Int -> Int -> GccProgram l ()
ld n i = liftF $ Inst (LD n i) ()

add :: GccProgram l ()
add = liftF $ Inst ADD ()

sub :: GccProgram l ()
sub = liftF $ Inst SUB ()

mul :: GccProgram l ()
mul = liftF $ Inst MUL ()

div :: GccProgram l ()
div = liftF $ Inst DIV ()

ceq :: GccProgram l ()
ceq = liftF $ Inst CEQ ()

cgt :: GccProgram l ()
cgt = liftF $ Inst CGT ()

cgte :: GccProgram l ()
cgte = liftF $ Inst CGTE ()

atom :: GccProgram l ()
atom = liftF $ Inst ATOM ()

cons :: GccProgram l ()
cons = liftF $ Inst CONS ()

car :: GccProgram l ()
car = liftF $ Inst CAR ()

cdr :: GccProgram l ()
cdr = liftF $ Inst CDR ()

sel :: GccProgram l ()
sel = liftF $ Inst SEL ()

join_ :: GccProgram l ()
join_ = liftF $ Inst JOIN ()

ldf :: l -> GccProgram l ()
ldf i = liftF $ Inst (LDF i) ()

ap :: l -> GccProgram l ()
ap i = liftF $ Inst (AP i) ()

rtn :: GccProgram l ()
rtn = liftF $ Inst RTN ()

dum :: Int -> GccProgram l ()
dum i = liftF $ Inst (DUM i) ()

rap :: Int -> GccProgram l ()
rap i = liftF $ Inst (RAP i) ()

stop :: GccProgram l ()
stop = liftF $ Stop

label :: l -> GccProgram l ()
label l = liftF $ Inst (LABEL l) ()


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

instList :: GccProgram l a -> [GccInst l]
instList (Pure _)          = []
instList (Free (Inst i c)) = i : instList c

codeGen :: Show l => GccProgram l a -> [String]
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

stupidAI :: GccProgram String ()
stupidAI = do
    ldc 4
    ldf "body"
    stop
    label "body"
    ldc 5
    rtn


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
