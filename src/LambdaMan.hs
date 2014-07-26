module GccExercises where

import Gcc
import GccMacros

f = putStrLn . codeGen

lm1 :: GccProgram String ()
lm1 = do
    ldf "process_world"
    ap 2

    ldc 0  -- state
    ldf "step_func"  -- step function closure
    cons
    rtn

    label "process_world"
    rtn

    label "step_func"
    ldc 0  -- state
    ldc 1  -- move right
    cons
    rtn
