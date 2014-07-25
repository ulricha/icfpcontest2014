{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free

data GccInstruction label cont
        -- primitive instructions
        = LDC Int cont
        | LD Int Int cont  -- (should the first arg be 'Char', the register name?)
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
        | LDF label cont
        | AP label cont
        | RTN cont
        | DUM Int cont
        | RAP Int cont
        | STOP

        -- symbolic labels
        | LABEL label cont

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

label :: Int -> GccProgram ()
label i = liftF $ LABEL i ()


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

{-
progToProgram :: GccProg () -> GccProgram ()
progToProgram prog = error "program"
  where
    instructionList :: GccProg () -> [GccProg ()]
    instructionList (Free instruction) =
        case getContinuation instruction of
            Nothing -> []
            Just cont -> instruction : instructionList cont
-}


getContinuation :: GccInstruction label cont -> Maybe cont
getContinuation (LDC _ cont) = Just cont
getContinuation (LD _ _ cont) = Just cont
getContinuation (ADD cont) = Just cont
getContinuation (SUB cont) = Just cont
getContinuation (MUL cont) = Just cont
getContinuation (DIV cont) = Just cont
getContinuation (CEQ cont) = Just cont
getContinuation (CGT cont) = Just cont
getContinuation (CGTE cont) = Just cont
getContinuation (ATOM cont) = Just cont
getContinuation (CONS cont) = Just cont
getContinuation (CAR cont) = Just cont
getContinuation (CDR cont) = Just cont
getContinuation (SEL cont) = Just cont
getContinuation (JOIN cont) = Just cont
getContinuation (LDF label cont) = Just cont
getContinuation (AP label cont) = Just cont
getContinuation (RTN cont) = Just cont
getContinuation (DUM _ cont) = Just cont
getContinuation (RAP _ cont) = Just cont
getContinuation (LABEL label cont) = Just cont
getContinuation (STOP) = Nothing


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

{-
stupidAI :: GccProg ()
stupidAI = do
    ldc 4
    ldf "body"
    stop
    label "body"
    ldc 5
    rtn
-}

{-

0 ldc 4
1 jmp 4
; body:
2 ldc 5
3 rtn
4 ldf 2
5 stop

-}



-- main :: IO ()
-- main = putStrLn $ unlines $ codeGen stupidAI
