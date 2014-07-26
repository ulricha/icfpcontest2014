module GccMacros where

import Gcc

--------------------------------------------------------------------------------
-- Helper macros
    
fun_call :: Int -> String -> GccProgram ()
fun_call arity label = do
    ldf label
    ap arity

fun_copy :: GccProgram ()
fun_copy = do
    label "copy"
    ld 0 0
    ld 0 0
    rtn

macro_nil :: GccProgram ()
macro_nil = ldc 0xdeadbeef

macro_isnil :: GccProgram ()
macro_isnil = atom
