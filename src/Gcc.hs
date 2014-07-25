{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free
import Control.Applicative
import Data.List

type Line = Int

-- | Instructions that reference absolute line numbers instead of
-- string labels
data GccCInst
    = C_LDC Int
    | C_LD Int Int
    | C_ADD
    | C_SUB
    | C_MUL
    | C_DIV
    | C_CEQ
    | C_CGT
    | C_CGTE
    | C_ATOM
    | C_CONS
    | C_CAR
    | C_CDR
    | C_SEL
    | C_JOIN
    | C_LDF Line
    | C_AP Line
    | C_RTN
    | C_DUM Int
    | C_RAP Int
    deriving (Show)

data GccInst label
    = LDC Int
    | LD Int Int
    | ADD
    | SUB
    | MUL
    | DIV
    | CEQ
    | CGT
    | CGTE
    | ATOM
    | CONS
    | CAR
    | CDR
    | SEL
    | JOIN
    | LDF label
    | AP label
    | RTN
    | DUM Int
    | RAP Int
    | LABEL label
    deriving (Show)

data GccInstruction label cont
    = Inst (GccInst label) cont
    | Stop
    deriving (Show, Functor)


type GccProgram l = Free (GccInstruction l)
type GccProg = GccProgram String

ldc :: Int -> GccProgram l ()
ldc n = liftF $ Inst (LDC n) ()

ld :: Int -> Int -> GccProgram l ()
ld n i = liftF $ Inst (LD n i) ()

add :: GccProgram l ()
add = liftF $ Inst ADD ()

sub :: GccProgram l ()
sub = liftF $ Inst SUB ()

mul :: GccProgram l ()
mul = liftF $ Inst MUL ()

div :: GccProgram l ()
div = liftF $ Inst DIV ()

ceq :: GccProgram l ()
ceq = liftF $ Inst CEQ ()

cgt :: GccProgram l ()
cgt = liftF $ Inst CGT ()

cgte :: GccProgram l ()
cgte = liftF $ Inst CGTE ()

atom :: GccProgram l ()
atom = liftF $ Inst ATOM ()

cons :: GccProgram l ()
cons = liftF $ Inst CONS ()

car :: GccProgram l ()
car = liftF $ Inst CAR ()

cdr :: GccProgram l ()
cdr = liftF $ Inst CDR ()

sel :: GccProgram l ()
sel = liftF $ Inst SEL ()

join_ :: GccProgram l ()
join_ = liftF $ Inst JOIN ()

ldf :: l -> GccProgram l ()
ldf i = liftF $ Inst (LDF i) ()

ap :: l -> GccProgram l ()
ap i = liftF $ Inst (AP i) ()

rtn :: GccProgram l ()
rtn = liftF $ Inst RTN ()

dum :: Int -> GccProgram l ()
dum i = liftF $ Inst (DUM i) ()

rap :: Int -> GccProgram l ()
rap i = liftF $ Inst (RAP i) ()

stop :: GccProgram l ()
stop = liftF $ Stop

label :: l -> GccProgram l ()
label l = liftF $ Inst (LABEL l) ()

label :: Int -> GccProgram ()
label i = liftF $ LABEL i ()


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

instList :: GccProgram l a -> [GccInst l]
instList (Pure _)          = []
instList (Free (Inst i c)) = i : instList c
instList (Free Stop)       = []

{-
1. associate every non-label inst with number
2. Associate inst following label with label name
-}



codeGen :: Show l => GccProgram l a -> [String]
codeGen p = map showInst $ instList p

showInst :: Show a => GccInst a -> String
showInst (LDC n) = ("LDC " ++ show n)
showInst (LD n i) = ("LCD " ++ show n ++ " " ++ show i)
showInst (ADD) = "ADD "
showInst (SUB) = "SUB "
showInst (DIV) = "DIV "
showInst (MUL) = "MUL "
showInst (CEQ) = "CEQ "
showInst (CGT) = "CGT "
showInst (CGTE) = "CGTE "
showInst (ATOM) = "ATOM "
showInst (CONS) = "CONS "
showInst (CAR) = "CAR "
showInst (CDR) = "CDR "
showInst (SEL) = "SEL "
showInst (JOIN) = "JOIN "
showInst (LDF n) = ("LDF " ++ show n)
showInst (AP n) = ("AP " ++ show n)
showInst (RTN) = "RTN "
showInst (DUM n) = ("DUM " ++ show n)
showInst (RAP n) = ("RAP " ++ show n)
showInst (LABEL l) = "LABEL " ++ show l



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

stupidAI :: GccProgram String ()
stupidAI = do
    ldc 4
    ldf "body"
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



{-

main :: IO ()
-- main = putStrLn $ unlines $ codeGen stupidAI
main = putStrLn "foo"
-}

--------------------------------------------------------------------------------
-- Transform string labels into proper line numbers

enumInsts :: [GccInst String] -> [Either (Line, GccInst String) String]
enumInsts insts = reverse acc
  where
    (acc, _) = foldl' enumInst ([], 1) insts

    enumInst (r, line) (LABEL label) = (Right label : r, line)
    enumInst (r, line) inst          = (Left (line, inst) : r, line + 1)

-- | Associate every label with the line number of its following
-- instruction.
assocLabelLine :: [Either (Line, GccInst String) String] -> [(String, Line)]
assocLabelLine (Left _ : is)                       = assocLabelLine is
assocLabelLine (Right label : Left (line, _) : is) = (label, line) : assocLabelLine is
assocLabelLine []                                  = []
assocLabelLine _                                   = error "assocLabelLine"

mapLabels :: [(String, Line)] -> [GccInst String] -> Maybe [GccCInst]
mapLabels _   [] = pure []
mapLabels env (LABEL _ : insts) = mapLabels env insts
mapLabels env (LDF label : insts) = do
    line   <- lookup label env
    cinsts <- mapLabels env insts
    return $ C_LDF line : cinsts
mapLabels env (AP label : insts) = do
    line   <- lookup label env
    cinsts <- mapLabels env insts
    return $ C_AP line : cinsts
mapLabels env (LDC i : insts) = (:) (C_LDC i) <$> mapLabels env insts
mapLabels env (LD i1 i2: insts) = (:) (C_LD i1 i2) <$> mapLabels env insts
mapLabels env (ADD : insts) = (:) C_ADD <$> mapLabels env insts
mapLabels env (SUB : insts) = (:) C_SUB <$> mapLabels env insts
mapLabels env (MUL : insts) = (:) C_MUL <$> mapLabels env insts
mapLabels env (DIV : insts) = (:) C_DIV <$> mapLabels env insts
mapLabels env (CEQ : insts) = (:) C_CEQ <$> mapLabels env insts
mapLabels env (CGT : insts) = (:) C_CGT <$> mapLabels env insts
mapLabels env (CGTE : insts) = (:) C_CGTE <$> mapLabels env insts
mapLabels env (ATOM : insts) = (:) C_ATOM <$> mapLabels env insts
mapLabels env (CONS : insts) = (:) C_CONS <$> mapLabels env insts
mapLabels env (CAR : insts) = (:) C_CAR <$> mapLabels env insts
mapLabels env (CDR : insts) = (:) C_CDR <$> mapLabels env insts
mapLabels env (SEL : insts) = (:) C_SEL <$> mapLabels env insts
mapLabels env (JOIN : insts) = (:) C_JOIN <$> mapLabels env insts
mapLabels env (RTN : insts) = (:) C_RTN <$> mapLabels env insts
mapLabels env (DUM i : insts) = (:) (C_DUM i) <$> mapLabels env insts
mapLabels env (RAP i : insts) = (:) (C_RAP i) <$> mapLabels env insts

-- | Turn string labels into proper line numbers
lineLabels :: [GccInst String] -> Maybe [GccCInst]
lineLabels insts = mapLabels lineEnv insts
  where
    lineEnv = assocLabelLine $ enumInsts insts
