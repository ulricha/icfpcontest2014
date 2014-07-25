{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free

data GccInst label 
    = LDC Int
    | LD Int
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
    | STOP
    -- | LABEL label
     
data GccInstruction label cont
    = Inst GccInst Label const
    deriving (Show, Functor)



type GccProgram = Free (GccInstruction Int)
type GccProg = Free (GccInstruction String)


ldc :: Int -> GccProgram ()
ldc n = liftF $ LDC n ()

ld :: Int -> Int -> GccProgram ()
ld n i = liftF $ LD n i ()

add :: GccProgram ()
add = liftF $ ADD ()

sub :: GccProgram ()
sub = liftF $ SUB ()

mul :: GccProgram ()
mul = liftF $ MUL ()

div :: GccProgram ()
div = liftF $ DIV ()

ceq :: GccProgram ()
ceq = liftF $ CEQ ()

cgt :: GccProgram ()
cgt = liftF $ CGT ()

cgte :: GccProgram ()
cgte = liftF $ CGTE ()

atom :: GccProgram ()
atom = liftF $ ATOM ()

cons :: GccProgram ()
cons = liftF $ CONS ()

car :: GccProgram ()
car = liftF $ CAR ()

cdr :: GccProgram ()
cdr = liftF $ CDR ()

sel :: GccProgram ()
sel = liftF $ SEL ()

join_ :: GccProgram ()
join_ = liftF $ JOIN ()

ldf :: Int -> GccProgram ()
ldf i = liftF $ LDF i ()

ap :: Int -> GccProgram ()
ap i = liftF $ AP i ()

rtn :: GccProgram ()
rtn = liftF $ RTN ()

dum :: Int -> GccProgram ()
dum i = liftF $ DUM i ()

rap :: Int -> GccProgram ()
rap i = liftF $ RAP i ()

stop :: GccProgram ()
stop = liftF $ STOP


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

progToProgram :: GccProg () -> GccProgram ()
progToProgram prog = program
  where
    instructionList :: [GccInstruction
    instructionList (Free instruction) = ...


getContinuation :: GccInstruction label cont -> cont
getContinuation (LDC n c) = c


codeGen :: GccProgram () -> [String]
codeGen (Pure _) = []
codeGen (Free (LDC n c)) = ("LDC " ++ show n) : codeGen c
codeGen (Free (LD n i c)) = ("LCD " ++ show n ++ " " ++ show i) : codeGen c
codeGen (Free (ADD c)) = "ADD " : codeGen c
codeGen (Free (SUB c)) = "SUB " : codeGen c
codeGen (Free (DIV c)) = "DIV " : codeGen c
codeGen (Free (MUL c)) = "MUL " : codeGen c
codeGen (Free (CEQ c)) = "CEQ " : codeGen c
codeGen (Free (CGT c)) = "CGT " : codeGen c
codeGen (Free (CGTE c)) = "CGTE " : codeGen c
codeGen (Free (ATOM c)) = "ATOM " : codeGen c
codeGen (Free (CONS c)) = "CONS " : codeGen c
codeGen (Free (CAR c)) = "CAR " : codeGen c
codeGen (Free (CDR c)) = "CDR " : codeGen c
codeGen (Free (SEL c)) = "SEL " : codeGen c
codeGen (Free (JOIN c)) = "JOIN " : codeGen c
codeGen (Free (LDF n c)) = ("LDF " ++ show n) : codeGen c
codeGen (Free (AP n c)) = ("AP " ++ show n) : codeGen c
codeGen (Free (RTN c)) = "RTN " : codeGen c
codeGen (Free (DUM n c)) = ("DUM " ++ show n) : codeGen c
codeGen (Free (RAP n c)) = ("RAP " ++ show n) : codeGen c
codeGen (Free (STOP)) = ["STOP\n"]


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

stupidAI :: GccProg ()
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



main :: IO ()
main = putStrLn $ unlines $ codeGen stupidAI
