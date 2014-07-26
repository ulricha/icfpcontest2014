module GccExercises where

import Gcc
import GccMacros

goto :: GccProgram ()
goto = do
    dum 2
    ldf "go"
    ldf "to"
    ldf "main"
    rap 2
    rtn

    label "main"
    ldc 1
    ld 0 0
    ap 1
    rtn

    label "to"
    ld 0 0
    ldc 1
    sub
    ld 1 0
    ap 1
    rtn

    label "go"
    ld 0 0
    ldc 1
    add
    ld 1 1
    ap 1
    rtn

cond :: GccProgram ()
cond = do
    ldc 5
    ldc 3
    cgte
    sel "true" "false"
    rtn
    label "true"
    ldc 42
    join_
    label "false"
    ldc 23
    join_

mklisths :: Int -> [Int]
mklisths i =
    if i == 0 then [] else i : mklisths (i - 1)

isempty :: GccProgram ()
isempty = do
    ldc 23
    ldc 42
    cons
    fun_call 1 "copy"
    cdr
    atom
    ldc 0
    ceq
    rtn
    fun_copy

addrec :: GccProgram ()
addrec = do
    ldc 1
    ldf "add"
    ap 1
    rtn
    label "add"
    ld 0 0
    ldc 1
    add
    fun_call 1 "copy"
    ldc 42
    ceq
    sel "stop" "rec"
    rtn
    label "stop"
    join_
    label "rec"
    ldf "add"
    ap 1
    join_
    fun_copy

-- Faculty

fac_test :: Int -> GccProgram ()
fac_test i = do
    ldc i
    fun_call 1 "fac"
    rtn
    fun_fac

-- fac n = if n == 0 then 1 else n * fac (n - 1)
fun_fac :: GccProgram ()
fun_fac = do
    label "fac"
    ld 0 0
    ldc 0
    ceq
    sel "zero" "nonzero"
    rtn
    label "zero"
    ldc 1
    join_
    label "nonzero"
    ld 0 0
    ld 0 0
    ldc 1
    sub
    fun_call 1 "fac"
    mul
    join_

-- list of integers

intlist_test :: Int -> GccProgram ()
intlist_test i = do
    ldc i
    fun_call 1 "intlist"
    rtn
    fun_intlist

-- intlist i = if i = 0 then [] else i : intlist (i - 1)
fun_intlist :: GccProgram ()
fun_intlist = do
    label "intlist"
    ld 0 0
    ldc 0
    ceq
    sel "intlist_zero" "intlist_rec"
    rtn

    label "intlist_zero"
    macro_nil
    join_

    label "intlist_rec"
    -- n
    ld 0 0

    -- intlist (n - 1)
    ld 0 0
    ldc 1
    sub
    fun_call 1 "intlist"

    -- i : intlist i - 1
    cons
    join_

-- list length
length_test :: Int -> GccProgram ()
length_test i = do
    ldc i

    fun_call 1 "intlist"
    fun_call 1 "length"
    rtn
    
    fun_intlist
    fun_length
    
-- length xs = if null xs then 0 else 1 + length tail xs
fun_length :: GccProgram ()
fun_length = do
    label "length"

    ld 0 0
    macro_isnil
    sel "length_zero" "length_rec"
    rtn
    
    label "length_zero"
    ldc 0
    join_

    label "length_rec"
    ld 0 0
    cdr
    fun_call 1 "length"
    ldc 1
    add
    join_
