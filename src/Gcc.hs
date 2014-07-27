{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleInstances   #-}

module Gcc where

import Control.Monad.Free
import Control.Applicative
import Data.List
import Text.Printf

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
    | C_DBUG

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
    show (C_DBUG) = "DBUG "

type Label = String

data GccInst
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
    | SEL Label Label
    | JOIN
    | LDF Label
    | AP Int
    | RTN
    | DUM Int
    | RAP Int
    | DBUG
    | LABEL Label

instance Show GccInst where
    show (LDC n) = ("LDC " ++ show n)
    show (LD n i) = ("LD " ++ show n ++ " " ++ show i)
    show (ADD) = "ADD "
    show (SUB) = "SUB "
    show (DIV) = "DIV "
    show (MUL) = "MUL "
    show (CEQ) = "CEQ "
    show (CGT) = "CGT "
    show (CGTE) = "CGTE "
    show (ATOM) = "ATOM "
    show (CONS) = "CONS "
    show (CAR) = "CAR "
    show (CDR) = "CDR "
    show (SEL t f) = "SEL " ++ show t ++ " " ++ show f
    show (JOIN) = "JOIN "
    show (LDF n) = ("LDF " ++ show n)
    show (AP n) = ("AP " ++ show n)
    show (RTN) = "RTN "
    show (DUM n) = ("DUM " ++ show n)
    show (RAP n) = ("RAP " ++ show n)
    show (DBUG) = "DBUG "
    show (LABEL l) = l ++ ":"

data GccInstruction cont
    = Inst GccInst cont
    -- | Stop  (removed because the implementation is broken and according to specs it is deprecated)
    deriving (Show, Functor)


type GccProgram = Free GccInstruction
type GccProg = GccProgram

ldc :: Int -> GccProgram ()
ldc n = liftF $ Inst (LDC n) ()

ld :: Int -> Int -> GccProgram ()
ld n i = liftF $ Inst (LD n i) ()

add :: GccProgram ()
add = liftF $ Inst ADD ()

sub :: GccProgram ()
sub = liftF $ Inst SUB ()

mul :: GccProgram ()
mul = liftF $ Inst MUL ()

div_ :: GccProgram ()
div_ = liftF $ Inst DIV ()

ceq :: GccProgram ()
ceq = liftF $ Inst CEQ ()

cgt :: GccProgram ()
cgt = liftF $ Inst CGT ()

cgte :: GccProgram ()
cgte = liftF $ Inst CGTE ()

atom :: GccProgram ()
atom = liftF $ Inst ATOM ()

cons :: GccProgram ()
cons = liftF $ Inst CONS ()

car :: GccProgram ()
car = liftF $ Inst CAR ()

cdr :: GccProgram ()
cdr = liftF $ Inst CDR ()

sel :: Label -> Label -> GccProgram ()
sel t f = liftF $ Inst (SEL t f) ()

join_ :: GccProgram ()
join_ = liftF $ Inst JOIN ()

ldf :: Label -> GccProgram ()
ldf i = liftF $ Inst (LDF i) ()

ap :: Int -> GccProgram ()
ap i = liftF $ Inst (AP i) ()

rtn :: GccProgram ()
rtn = liftF $ Inst RTN ()

dum :: Int -> GccProgram ()
dum i = liftF $ Inst (DUM i) ()

rap :: Int -> GccProgram ()
rap i = liftF $ Inst (RAP i) ()

dbug :: GccProgram ()
dbug = liftF $ Inst DBUG ()

label :: Label -> GccProgram ()
label l = liftF $ Inst (LABEL l) ()


--------------------------------------------------------------------------
-- CodeGen
--------------------------------------------------------------------------

instList :: GccProgram a -> [GccInst]
instList (Pure _)          = []
instList (Free (Inst i c)) = i : instList c

codeGen :: [GccInst] -> String
codeGen p = intercalate "\n" . enumerate . pad . map show $ l
  where
    (l :: [GccCInst], env :: [(String, Int)]) = fromEither_ p $ lineLabels p

    pad :: [String] -> [String]
    pad xs = map (\ x -> x ++ (replicate (maximum (map length xs) - length x) ' ')) xs

    enumerate :: [String] -> [String]
    enumerate = map (\ (line :: Int, inst :: String) -> printf "%s ; %8.8i%s" inst line (label line)) . zip [0..]
      where
        env' :: [(Int, String)] = map (\ (name, line) -> (line, name)) env

        label :: Int -> String
        label i = maybe "" (printf " [%s]" :: String -> String) . lookup i $ env'

fromEither_ :: Show b => b -> Either String a -> a
fromEither_ msg (Right x) = x
fromEither_ msg (Left e)  = error $ "codeGen: lineLabels gave error\nmsg:\n" ++ e ++ "\ninput:\n" ++ show msg

printInsts :: [GccInst] -> IO ()
printInsts p = putStrLn $ codeGen p

printLabelInsts :: [GccInst] -> IO ()
printLabelInsts p = putStrLn $ intercalate "\n" $ map show p

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

stupidAI :: GccProgram ()
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

enumInsts :: [GccInst] -> [Either (Line, GccInst) String]
enumInsts insts = reverse acc
  where
    (acc, _) = foldl' enumInst ([], 0) insts

    enumInst (r, line) (LABEL lab) = (Right lab : r, line)
    enumInst (r, line) inst        = (Left (line, inst) : r, line + 1)

-- | Associate every label with the line number of its following
-- instruction.
assocLabelLine :: [Either (Line, GccInst) String] -> [(String, Line)]
assocLabelLine insts = if null bad then result else error $ "assocLabelLine: dupliate labels: " ++ show bad
  where
    result = assocLabelLine' insts
    ls = map fst result
    bad = sort ls \\ nub (sort ls)

assocLabelLine' :: [Either (Line, GccInst) String] -> [(String, Line)]
assocLabelLine' (Left _ : is)                   = assocLabelLine is
assocLabelLine' (Right lab : Left (line, _) : is) = (lab, line) : assocLabelLine is
assocLabelLine' []                              = []
assocLabelLine' _                               = error "assocLabelLine: trailing label"


mapLabels :: [(String, Line)] -> [GccInst] -> Either String [GccCInst]
mapLabels _   [] = pure []
mapLabels env (LABEL _ : insts) = mapLabels env insts
mapLabels env (LDF lab : insts) = do
    line   <- maybe (Left ("mapLabels: unknown function name " ++ show lab)) Right
            $ lookup lab env
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
    lineTrue <- maybe (Left ("mapLabels: unknown 'true' branch name " ++ show t)) Right
              $ lookup t env
    lineFalse <- maybe (Left ("mapLabels: unknown 'false' branch name " ++ show f)) Right
               $ lookup f env
    cinsts <- mapLabels env insts
    return $ C_SEL lineTrue lineFalse : cinsts
mapLabels env (JOIN : insts) = (:) C_JOIN <$> mapLabels env insts
mapLabels env (RTN : insts) = (:) C_RTN <$> mapLabels env insts
mapLabels env (DUM i : insts) = (:) (C_DUM i) <$> mapLabels env insts
mapLabels env (RAP i : insts) = (:) (C_RAP i) <$> mapLabels env insts
mapLabels env (DBUG : insts) = (:) C_DBUG <$> mapLabels env insts

-- | Turn string labels into proper line numbers
lineLabels :: [GccInst] -> Either String ([GccCInst], [(String, Int)])
lineLabels insts = (,lineEnv) <$> mapLabels lineEnv insts
  where
    lineEnv = assocLabelLine $ enumInsts insts
