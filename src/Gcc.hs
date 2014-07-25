{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free

data GccInstruction cont
        = LDC Int cont
        | LD Int Int cont
        | ADD cont
        | SUB cont
        | MUL cont
        | DIV cont
        | CEQ cont
        | CGT cont
        | CGTE cont
        | ATOM cont
        | CONS cont
        | CAR cont
        | CDR cont
        | SEL cont
        | JOIN cont
        | LDF Int cont
        | AP Int cont
        | RTN cont
        | DUM Int cont
        | RAP Int cont
        | STOP
        deriving (Show, Functor)

type GccProgram = Free GccInstruction
data DataStack
data ControlStack
data EnvironmentFrame
data DataHeap


data GccProgState = GPS { ds :: DataStack
                        , cs :: ControlStack
                        , ef :: EnvironmentFrame
                        , dh :: DataHeap
                        }


ldc :: Int -> GccProgram ()
ldc n = liftF $ LDC n ()

ld :: Int -> Int -> GccProgram ()
ld n i = liftF $ LD n i ()

add, sub, mul, div, ceq, cgt, cgte, atom, cons, car, cdr, sel :: GccProgram ()
add = liftF $ ADD ()
sub = liftF $ SUB ()
mul = liftF $ MUL ()
div = liftF $ DIV ()
ceq = liftF $ CEQ ()
cgt = liftF $ CGT ()
cgte = liftF $ CGTE ()
atom = liftF $ ATOM ()
cons = liftF $ CONS ()
car = liftF $ CAR ()
cdr = liftF $ CDR ()
sel = liftF $ SEL ()

stop :: GccProgram ()
stop = liftF $ STOP

stupidAI :: GccProgram ()
stupidAI = do
    ldc 4

--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

codeGen :: GccProgram n -> String
codeGen (Pure n) = "\n"
codeGen (Free (LDC n c)) = "LDC " ++ show n ++ " ;\n" ++ codeGen c
codeGen (Free (LDC n c)) = "LDC " ++ show n ++ " ;\n" ++ codeGen c
codeGen (Free (LD n i c)) = "LCD " ++ show n ++ " " ++ show i ++ " ;\n" ++ codeGen c
codeGen (Free (ADD c)) = "ADD " ++ "\n" ++ codeGen c
codeGen (Free (SUB c)) = "SUB " ++ "\n" ++ codeGen c
codeGen (Free (DIV c)) = "DIV " ++ "\n" ++ codeGen c
codeGen (Free (MUL c)) = "MUL " ++ "\n" ++ codeGen c
codeGen (Free (CEQ c)) = "CEQ " ++ "\n" ++ codeGen c
codeGen (Free (CGT c)) = "CGT " ++ "\n" ++ codeGen c
codeGen (Free (CGTE c)) = "CGTE " ++ "\n" ++ codeGen c
codeGen (Free (ATOM c)) = "ATOM " ++ "\n" ++ codeGen c
codeGen (Free (CONS c)) = "CONS " ++ "\n" ++ codeGen c
