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

stupidAI :: GccProgram ()
stupidAI = do
    ldc 4
