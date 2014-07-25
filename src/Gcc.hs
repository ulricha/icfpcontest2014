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

codeGen :: GccProgram n -> String
codeGen (Pure n) = "\n"
codeGen (Free (LDC n c)) = "LDC " ++ show n ++ "\n" ++ codeGen c
codeGen (Free (LD n i c)) = "LCD " ++ show n ++ " " ++ show i ++ "\n" ++ codeGen c
codeGen (Free (ADD c)) = "ADD " ++ "\n" ++ codeGen c
codeGen (Free (SUB c)) = "SUB " ++ "\n" ++ codeGen c
codeGen (Free (DIV c)) = "DIV " ++ "\n" ++ codeGen c
codeGen (Free (MUL c)) = "MUL " ++ "\n" ++ codeGen c
codeGen (Free (CEQ c)) = "CEQ " ++ "\n" ++ codeGen c
codeGen (Free (CGT c)) = "CGT " ++ "\n" ++ codeGen c
codeGen (Free (CGTE c)) = "CGTE " ++ "\n" ++ codeGen c
codeGen (Free (ATOM c)) = "ATOM " ++ "\n" ++ codeGen c
codeGen (Free (CONS c)) = "CONS " ++ "\n" ++ codeGen c
codeGen (Free (CAR c)) = "CAR " ++ "\n" ++ codeGen c
codeGen (Free (CDR c)) = "CDR " ++ "\n" ++ codeGen c
codeGen (Free (SEL c)) = "SEL " ++ "\n" ++ codeGen c
codeGen (Free (JOIN c)) = "JOIN " ++ "\n" ++ codeGen c
codeGen (Free (LDF n c)) = "LDF " ++ show n ++ "\n" ++ codeGen c
codeGen (Free (AP n c)) = "AP " ++ show n ++ "\n" ++ codeGen c
codeGen (Free (RTN c)) = "RTN " ++ "\n" ++ codeGen c
codeGen (Free (DUM n c)) = "DUM " ++ show n ++ "\n" ++ codeGen c
codeGen (Free (RAP n c)) = "RAP " ++ show n ++ "\n" ++ codeGen c
codeGen (Free (STOP)) = "STOP\n"


----------------------------------------------------------------------
-- docks
----------------------------------------------------------------------

stupidAI :: GccProgram ()
stupidAI = do
    ldc 4
    stop


main :: IO ()
main = putStrLn $ codeGen stupidAI
