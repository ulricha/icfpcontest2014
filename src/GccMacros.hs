module GccMacros where

import Gcc

--------------------------------------------------------------------------------
-- Helper macros
    
fun_call :: Int -> String -> GccProgram String ()
fun_call arity label = do
    ldf label
    ap arity

fun_copy :: GccProgram String ()
fun_copy = do
    label "copy"
    ld 0 0
    ld 0 0
    rtn

macro_nil :: GccProgram String ()
macro_nil = ldc 0xdeadbeef

macro_isnil :: GccProgram String ()
macro_isnil = atom
