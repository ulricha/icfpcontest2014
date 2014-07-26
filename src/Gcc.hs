{-# LANGUAGE DeriveFunctor #-}

module Gcc where

import Control.Monad.Free
import Control.Monad.State (State, get, gets, put) -- TODO: strict?
import qualified Data.IntMap as M
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
    | C_SEL Int Int
    | C_JOIN
    | C_LDF Line
    | C_AP Int
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

stop :: GccProgram l ()
stop = liftF $ Stop

label :: l -> GccProgram l ()
label l = liftF $ Inst (LABEL l) ()



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
showInst (SEL t f) = "SEL " ++ show t ++ " " ++ show f
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

type Pointer = Int
data TaggedValue = Int Int | PairPointer Pointer | ClosurePointer Pointer

data HeapElement = Pair TaggedValue TaggedValue | Closure Int EnvironmentFrame

-- type Pair = (TaggedValue, TaggedValue)
data Closure

type DataStack = [TaggedValue]
type ControlStack = [Line]
                              -- Parent Frame
data EnvironmentFrame = Frame (Maybe EnvironmentFrame) [TaggedValue]
data DataHeap = Heap (M.IntMap HeapElement) Int

heapPut :: DataHeap -> HeapElement -> (DataHeap, Pointer)
heapPut (Heap m curPointer) v =
    (Heap (M.insert curPointer v m) (curPointer + 1), curPointer)


heapGet :: DataHeap -> Pointer -> HeapElement
heapGet (Heap m _) pointer = case M.lookup pointer m of
    Just e -> e
    Nothing -> error "looks like this was never allocated"

data GccProgState = GPS { ds :: DataStack
                        , cs :: ControlStack
                        , ef :: EnvironmentFrame
                        , dh :: DataHeap
                        , pc :: Int
                        }


--interpret :: GccProgram l a -> [String]
--interpret p = mapM interpretInst $ instList p

(!!!) :: EnvironmentFrame -> Int -> TaggedValue
Frame _ values !!! n = values !! n


-- TODO: so many partial functions...
interpretInst :: GccCInst -> State GccProgState ()
interpretInst instr = do
    
    GPS dataStack controlStack env heap pc <- get
    
    let pushStack e s = put $ GPS (e : s) controlStack env heap (pc + 1)

    case instr of
        C_LDC n -> pushStack (Int n) dataStack
        C_LD n i -> pushStack (getFrame env n !!! i) dataStack
        C_ADD -> binStackIntOp (+)
        C_SUB -> binStackIntOp (-)
        C_MUL -> binStackIntOp (*)
        C_DIV -> binStackIntOp Prelude.div
        C_CEQ -> binStackIntOp (\a b -> fromEnum $ a == b)
        C_CGT -> binStackIntOp (\a b -> fromEnum $ a > b)
        C_CGTE -> binStackIntOp (\a b -> fromEnum $ a >= b)
        C_ATOM -> case dataStack of
                    [] -> error "stack is empty"
                    Int _ : stack -> pushStack (Int 1) stack
                    _ : stack     -> pushStack (Int 0) stack

        C_CONS -> case dataStack of
                    a : b : stack -> do
                        let (heap', pointer) = heapPut heap (Pair a b)
                        put $ GPS (PairPointer pointer : stack)
                                   controlStack env heap' (pc + 1)
                    _ -> error "not enough values on the stack"

        C_CAR -> case dataStack of
            (PairPointer p) : stack ->
                case heapGet heap p of
                    Pair car cdr -> pushStack car stack
                    _ -> error "CAR: heap value is not a pair"
            _ : _ -> error "CAR: top of stack does not point to a pair"
            [] -> error "CAR: empty stack"

        C_CDR -> case dataStack of
            (PairPointer p) : stack ->
                case heapGet heap p of
                    Pair car cdr -> pushStack cdr stack
                    _ -> error "CAR: heap value is not a pair"
            _ : _ -> error "CAR: top of stack does not point to a pair"
            [] -> error "CAR: empty stack"

        C_SEL t f -> case dataStack of
            Int n : stack -> let pc' = if n /= 0 then t else f in
                             put $ GPS stack controlStack env heap pc'
            _ : _ -> error "SEL: top of the stack is not an int"
            [] -> error "SEL: stack is empty"

        C_JOIN -> case controlStack of
            retAddr : stack -> put $ GPS dataStack stack env heap retAddr
            [] -> error "control stack is empty"

        C_LDF f -> let (heap', pointer) = heapPut heap (Closure f env)
                   in put $ GPS (Int pointer : dataStack)
                                 controlStack env heap' pc

        C_AP n -> 
        
  where

    binStackIntOp :: (Int -> Int -> Int) -> State GccProgState ()
    binStackIntOp op = do
        GPS dataStack controlStack env heap pc <- get
        case dataStack of
            [] ->  error "stack is empty"
            [_] ->  error "stack has only 1 element"
            (Int a : Int b : stack) ->
                put $ GPS (Int (a `op` b) : stack)
                            controlStack env heap (pc + 1)
            _ -> error "tag mismatch"

    getFrame :: EnvironmentFrame -> Int -> EnvironmentFrame
    getFrame f 0 = f
    getFrame (Frame (Just parent) values) n = getFrame parent (n - 1)
    getFrame _ _ = error "frame does not exist"

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
    (acc, _) = foldl' enumInst ([], 0) insts

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
mapLabels env (SEL t f : insts) = do
    lineTrue <- lookup t env
    lineFalse <- lookup f env
    cinsts <- mapLabels env insts
    return $ C_SEL lineTrue lineFalse : cinsts
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
mapLabels env (JOIN : insts) = (:) C_JOIN <$> mapLabels env insts
mapLabels env (RTN : insts) = (:) C_RTN <$> mapLabels env insts
mapLabels env (DUM i : insts) = (:) (C_DUM i) <$> mapLabels env insts
mapLabels env (RAP i : insts) = (:) (C_RAP i) <$> mapLabels env insts

-- | Turn string labels into proper line numbers
lineLabels :: [GccInst String] -> Maybe [GccCInst]
lineLabels insts = mapLabels lineEnv insts
  where
    lineEnv = assocLabelLine $ enumInsts insts
