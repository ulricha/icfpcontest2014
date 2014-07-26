module GccExercises where

import Gcc
import GccMacros

f = putStrLn . codeGen

lm1 :: GccProgram String ()
lm1 = do
    ld 0 0  -- initial game state
    ld 0 1  -- ghost code
    ldf "process_world"
    ap 2

    ldc 0  -- state
    ldf "step_func"  -- step function closure
    cons
    rtn

    label "process_world"
    -- ld 0 0  -- initial game state
    -- ld 0 1  -- ghost code

    ld 0 0  -- initial game state
    car  -- map
    ldf "dump_map"
    ap 1
    rtn

    label "step_func"
    -- ld 0 0  -- ai state
    -- ld 0 1  -- game state

    ldc 0  -- state
    ldc 1  -- move right
    cons
    rtn

    -- (leaves data stack intact.)
    label "dump_map"
    -- ld 0 0 -- map

    ld 0 0
    ldc 1
    ldc 0
    ldf "obj_at_pos"
    ap 3
    dbug

    ld 0 0
    ldc 1
    ldc 1
    ldf "obj_at_pos"
    ap 3
    dbug

    ld 0 0
    ldc 1
    ldc 2
    ldf "obj_at_pos"
    ap 3
    dbug
    rtn

    -- macros
    obj_at_pos


obj_at_pos :: GccProgram String ()
obj_at_pos = do
    label "obj_at_pos"
    -- ld 0 0  -- map
    -- ld 0 1  -- x
    -- ld 0 2  -- y

    ld 0 0
    ld 0 2
    ldf "obj_at_pos_find_elem"
    ap 2
    ld 0 1
    ldf "obj_at_pos_find_elem"
    ap 2
    rtn

    label "obj_at_pos_find_elem"
    ld 0 0  -- remaining elems
    ld 0 1  -- index
    sel "obj_at_pos_find_elem_1" "obj_at_pos_find_elem_0"
    rtn
    label "obj_at_pos_find_elem_0"  -- yes-zero
    car
    join_
    label "obj_at_pos_find_elem_1"  -- non-zero
    cdr
    ld 0 1
    ldc 1
    sub
    ldf "obj_at_pos_find_elem"
    ap 2
    join_
