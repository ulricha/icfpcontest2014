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
    rtn

    label "step_func"
    -- ld 0 0  -- ai state
    ld 0 1  -- game state
    car     -- map
    ldf "dump_map"
    ap 1

    ldc 0  -- state
    ldc 1  -- move right
    cons
    rtn

    -- (leaves data stack intact.)
    label "dump_map"
    ld 0 0
    dbug
    ld 0 0
    car
    dbug
    ld 0 0
    rtn


    -- not working yet: i have to decrement the y / x coordinates in
    -- the frame in accordance with cdr-ing through the rows / cells.
    -- this cannot be done on the stack directly, and frames can only
    -- be written to by writing more function calls.  this is quite
    -- tedious!

    label "obj_at_pos"
    -- ld 0 0  -- map
    -- ld 0 1  -- x
    -- ld 0 2  -- y

    -- load map, y coord, 0 onto stack, and conditionally jump.
    ld 0 0
    ld 0 2
    ldc 0
    sel "obj_at_pos__1" "obj_at_pos__2"
    rtn

    -- y == 0: keep current first line on stack; jump to x search.
    label "obj_at_pos__1"
    car
    ld 0 1
    ldc 0
    sel "obj_at_pos__3" "obj_at_pos__4"
    join

    -- y /= 0: move to next line, repeat.
    label "obj_at_pos__2"
    cdr
    ld 0 2
    ldc 0
    sel "obj_at_pos__1" "obj_at_pos__2"
    join

    -- x == 0: keep current first cell on stack; done!
    label "obj_at_pos__3"
    car
    join

    -- x /= 0: move to next cell, repeat.
    label "obj_at_pos__4"
    cdr
    ld 0 1
    ldc 0
    sel "obj_at_pos__3" "obj_at_pos__4"
    join
