{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free
import Control.Applicative
import Data.List
import Data.Maybe

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
    | C_SEL Line Line
    | C_JOIN
    | C_LDF Line
    | C_AP Int
    | C_RTN
    | C_DUM Int
    | C_RAP Int

instance Show GccCInst where
    show (C_LDC n) = ("LDC " ++ show n)
    show (C_LD n i) = ("LD " ++ show n ++ " " ++ show i)
    show (C_ADD) = "ADD "
    show (C_SUB) = "SUB "
    show (C_DIV) = "DIV "
    show (C_MUL) = "MUL "
    show (C_CEQ) = "CEQ "
    show (C_CGT) = "CGT "
    show (C_CGTE) = "CGTE "
    show (C_ATOM) = "ATOM "
    show (C_CONS) = "CONS "
    show (C_CAR) = "CAR "
    show (C_CDR) = "CDR "
    show (C_SEL t f) = "SEL " ++ show t ++ " " ++ show f
    show (C_JOIN) = "JOIN "
    show (C_LDF n) = ("LDF " ++ show n)
    show (C_AP n) = ("AP " ++ show n)
    show (C_RTN) = "RTN "
    show (C_DUM n) = ("DUM " ++ show n)
    show (C_RAP n) = ("RAP " ++ show n)

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
    | SEL label label
    | JOIN
    | LDF label
    | AP Int
    | RTN
    | DUM Int
    | RAP Int
    | LABEL label
    deriving (Show)

data GccInstruction label cont
    = Inst (GccInst label) cont
    -- | Stop  (removed because the implementation is broken and according to specs it is deprecated)
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

sel :: l -> l -> GccProgram l ()
sel t f = liftF $ Inst (SEL t f) ()

join_ :: GccProgram l ()
join_ = liftF $ Inst JOIN ()

ldf :: l -> GccProgram l ()
ldf i = liftF $ Inst (LDF i) ()

ap :: Int -> GccProgram l ()
ap i = liftF $ Inst (AP i) ()

rtn :: GccProgram l ()
rtn = liftF $ Inst RTN ()

dum :: Int -> GccProgram l ()
dum i = liftF $ Inst (DUM i) ()

rap :: Int -> GccProgram l ()
rap i = liftF $ Inst (RAP i) ()

label :: l -> GccProgram l ()
label l = liftF $ Inst (LABEL l) ()



--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

instList :: GccProgram l a -> [GccInst l]
instList (Pure _)          = []
instList (Free (Inst i c)) = i : instList c

codeGen :: GccProgram String a -> String
codeGen p = intercalate "\n" $ map show $ fromJust $ lineLabels $ instList p

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
    ldc 23
    ldc 42
    ldc 5
    ldf "addsub"
    ap 3
    rtn
    label "addsub"
    ld 0 2
    ld 0 1
    ld 0 0
    add
    sub
    rtn

--------------------------------------------------------------------------------
-- Transform string labels into proper line numbers

enumInsts :: [GccInst String] -> [Either (Line, GccInst String) String]
enumInsts insts = reverse acc
  where
    (acc, _) = foldl' enumInst ([], 0) insts

    enumInst (r, line) (LABEL lab) = (Right lab : r, line)
    enumInst (r, line) inst        = (Left (line, inst) : r, line + 1)

-- | Associate every label with the line number of its following
-- instruction.
assocLabelLine :: [Either (Line, GccInst String) String] -> [(String, Line)]
assocLabelLine (Left _ : is)                   = assocLabelLine is
assocLabelLine (Right lab : Left (line, _) : is) = (lab, line) : assocLabelLine is
assocLabelLine []                              = []
assocLabelLine _                               = error "assocLabelLine"

mapLabels :: [(String, Line)] -> [GccInst String] -> Maybe [GccCInst]
mapLabels _   [] = pure []
mapLabels env (LABEL _ : insts) = mapLabels env insts
mapLabels env (LDF lab : insts) = do
    line   <- lookup lab env
    cinsts <- mapLabels env insts
    return $ C_LDF line : cinsts
mapLabels env (AP i : insts) = (:) (C_AP i) <$> mapLabels env insts
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
mapLabels env (SEL t f : insts) = do
    lineTrue <- lookup t env
    lineFalse <- lookup f env
    cinsts <- mapLabels env insts
    return $ C_SEL lineTrue lineFalse : cinsts
mapLabels env (JOIN : insts) = (:) C_JOIN <$> mapLabels env insts
mapLabels env (RTN : insts) = (:) C_RTN <$> mapLabels env insts
mapLabels env (DUM i : insts) = (:) (C_DUM i) <$> mapLabels env insts
mapLabels env (RAP i : insts) = (:) (C_RAP i) <$> mapLabels env insts

-- | Turn string labels into proper line numbers
lineLabels :: [GccInst String] -> Maybe [GccCInst]
lineLabels insts = mapLabels lineEnv insts
  where
    lineEnv = assocLabelLine $ enumInsts insts
