{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SECD where

import Control.Applicative
import Control.Monad.Free
import Data.Attoparsec as AP
import Data.List
import Data.Monoid
import Data.String.Conversions as SC
import System.Environment
import System.Process
import Text.Show.Pretty

import qualified Data.AttoLisp as AL
import qualified Data.ByteString.Char8 as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as IO

import Gcc
import GccMacros


schemeToGcc :: SC.SBS -> IO (Either String (GccProgram String ()))
schemeToGcc = fmap secdToGcc . schemeToSECD


schemeToSECD :: SC.SBS -> IO SC.SBS
schemeToSECD scheme = do
    secdscheme <- maybe (error "please set shell variable SECD_SCHEME [github.com/EarlGray/SECD]") id .
                  lookup "SECD_SCHEME" <$>
                  getEnvironment
    let cmd = "(secd-compile '" <> scheme <> ")"
    (i, o, e, h) <- runInteractiveProcess secdscheme [] Nothing Nothing
    SBS.hPutStr i cmd
    IO.hFlush i
    IO.hClose i

    let f :: SC.SBS -> SC.SBS
        f = SC.cs . unlines . Prelude.map (Prelude.dropWhile (/= '(')) . lines . SC.cs
    f <$> SBS.hGetContents o


secdToGcc :: SC.SBS -> Either String (GccProgram String ())
secdToGcc = fmap sexpToGcc . eitherResult . parse AL.lisp


-- | i just add this because i fixed the broken Show instance in
-- atto-lisp, and i wanted to have it back.  (-:
prettyShow :: AL.Lisp -> String
prettyShow = f
  where
    f (AL.Symbol a) = T.unpack a
    f (AL.String t) = show (T.unpack t)
    f (AL.Number n) = show n
    f (AL.List l) = "(" ++ intercalate " " (map f l) ++ ")"
    f (AL.DotList l d) = "(" ++ intercalate " " (map f l) ++ " . " ++ f d ++ ")"


----------------------------------------------------------------------
-- the actual SECG-Gcc compiler
----------------------------------------------------------------------

-- order of cons
-- inline lambdas => labels
-- align argument frame addresses

type Routine = (String, GccProg ())

type ScopeFrame = [(T.Text, Int)]

-- | take a parameter list in a secd lambda expression and turn it
-- into a scope frame.
mkScopeFrame :: [AL.Lisp] -> ScopeFrame
mkScopeFrame = (`zip` [0..]) . map (\ (AL.Symbol s) -> s)

scopeLookup :: [ScopeFrame] -> T.Text -> GccProg ()
scopeLookup frames name = f 0 frames
  where
    f i (frame:frames') = maybe (f (i+1) frames') (Gcc.ld i) $ lookup name frame
    f _ [] = error $ "undefined free variable: " ++ show name

-- FIXME: i think gcc does dynamic scoping, sexpToGcc assumes lexical
-- scoping, and the two down agree very well with each other.  must
-- think more about this.  find counter-example?

-- FIXME: the generated code appears to make the runtime crash in some
-- cases.


flipStack :: GccProg ()
flipStack = do
    ldf "_flip_stack"
    ap 2

flipStackAsm :: GccProg ()
flipStackAsm = do
    label "_flip_stack"
    ld 0 1
    ld 0 0
    rtn

-- | in secd, "ap" can have a variable-length parameter list!  the
-- length is only known at run-time, so this has to be handled in gcc
-- assembler.
--
-- traverse the argument list and push everything to the stack in the
-- right order, keeping track of the number of arguments pushed.
-- then, since the number of arguments is a data value, and gcc-ap
-- only accepts a literal number of arguments, branch to individual
-- literal calls ap-calls based on that value.  handle up to
-- 'maxApVarArgs' arguments.
untangleArgv :: GccProg ()
untangleArgv = do
    ldc 0  -- number of args already pushed
    ldf "_untangle_argv"
    ap 2

untangleArgvAsm :: GccProg ()
untangleArgvAsm = do
    label "_untangle_argv"
    -- ld 0 0  -- arg list
    -- ld 0 1  -- var-arg function closure
    -- ld 0 2  -- number of args already pushed

    let llabel :: String -> GccProg ()
        llabel = label . llabels

        llabels :: String -> String
        llabels "" = "_untangle_argv"
        llabels s = "_untangle_argv_" ++ s

    -- if arg list is not a cons, then we have pushed all arguments.
    ld 0 0
    atom
    sel (llabels "done_pushing") (llabels "not_done_pushing")
    rtn

    -- local branches (thankfully, we may keep the environment frame
    -- because it's just sel, not ldf/ap)

    -- push function, push number of arguments i, call "ap i".
    llabel "done_pushing"
    ld 0 1
    ld 0 2
    ldc 1
    ceq
    sel (llabels "1") (llabels "not_1")
    join_

    -- push car of arg list, put re-construct stack, call self.
    llabel "not_done_pushing"
    ld 0 0
    car
    ld 0 0
    cdr
    ld 0 1
    ld 0 2
    ldc 1
    add
    ldf (llabels "")
    ap 3
    join_

    -- branches for calling ap with the right integer literal.
    mapM_ (\ i -> do
              llabel (show i)
              ld 0 1
              ap i
              join_)
          [1..maxApVarArgs]

    mapM_ (\ i -> if i < maxApVarArgs
                      then do
                          llabel ("not_" ++ show i)
                          ld 0 2
                          ldc (i + 1)
                          ceq
                          sel (llabels (show (i + 1))) (llabels $ "not_" ++ (show (i + 1)))
                          join_
                      else do
                          llabel ("not_" ++ show i)
                          ldc 0xdeadbaff
                          dbug
                          join_)
          [1..maxApVarArgs]

    -- try to be informative about this when it happens
    llabel "0"
    ldc 0xdeadbaaf
    dbug
    join_

maxApVarArgs :: Int
maxApVarArgs = 10


-- | comple an secd program into an gcc program.
sexpToGcc :: AL.Lisp -> GccProg ()
sexpToGcc sexp@(AL.List secd) = do
      insts
      flipStackAsm
      untangleArgvAsm
      mapM_ (\ (n, r) -> label n >> r) $ routines
  where
    (_, routines, insts) = lexer [] 0 [] [] secd

    lexer :: [ScopeFrame] -> Int -> [Routine] -> [GccProg ()] -> [AL.Lisp] -> (Int, [Routine], GccProg ())
    lexer scopes label routines insts (AL.Symbol "LDC" : AL.Number i : secd) =
        lexer scopes label routines (insts ++ [ldc (round i)]) secd

    -- LDC may also receive '()' as argument in secd, but in gcc
    -- forces us to make up an integer.
    lexer scopes label routines insts (AL.Symbol "LDC" : AL.List [] : secd) =
        lexer scopes label routines (insts ++ [ldc 0xdeadbeef]) secd

    lexer scopes label routines insts (AL.Symbol "LD" : AL.Number i1 : AL.Number i2 : secd) =
        lexer scopes label routines (insts ++ [ld (round i1) (round i2)]) secd

    lexer scopes label routines insts (AL.Symbol "LD" : AL.Symbol name : secd) =
        lexer scopes label routines (insts ++ [scopeLookup scopes name]) secd

    lexer scopes label routines insts (AL.Symbol (matchUnaryOP -> Just unaryOp) : secd) =
        lexer scopes label routines (insts ++ [unaryOp]) secd

    lexer scopes label routines insts (AL.Symbol "SEL" : AL.List nonzero : AL.List zero : secd) =
        let nzlabel                       = show $ label
            zlabel                        = show $ label + 1
            (label', routines', nzinst)   = lexer scopes (label + 2) [] [] nonzero
            (label'', routines'', zinst)  = lexer scopes label' [] [] zero
            routines'''                   = (nzlabel, nzinst) : (zlabel, zinst)
                                          : routines ++ routines' ++ routines''
            insts'                        = insts ++ [sel nzlabel zlabel]
        in lexer scopes label'' routines''' insts' secd

    lexer scopes label routines insts (AL.Symbol "LDF" : (AL.List [AL.List argv, AL.List body]) : secd) =
        let label'                          = label + 1
            (label'', routines', subinsts)  = lexer (mkScopeFrame argv : scopes) label' [] [] body
            routines''                      = (show label, subinsts) : routines ++ routines'
            insts'                          = insts ++ [ldf (show label)]
        in lexer scopes label'' routines'' insts' secd

    lexer scopes label routines insts (AL.Symbol "AP" : AL.Number i : secd) =
        lexer scopes label routines (insts ++ [ap (round i)]) secd

    lexer scopes label routines insts (AL.Symbol "AP" : secd) =
        lexer scopes label routines (insts ++ [untangleArgv]) secd

    lexer scopes label routines insts (AL.Symbol "DUM" : AL.Number i : secd) =
        lexer scopes label routines (insts ++ [dum (round i)]) secd

    lexer scopes label routines insts (AL.Symbol "RAP" : AL.Number i : secd) =
        lexer scopes label routines (insts ++ [rap (round i)]) secd

    lexer scopes label routines insts [] = (label, routines, sequence_ $ inverseCond insts)

    lexer s l r i x = error $ "sexpToGcc: unmatched pattern!\n" ++ ppShow (s, l, r, i, x)

    matchUnaryOP :: T.Text -> Maybe (GccProg ())
    matchUnaryOP "ADD" = Just add
    matchUnaryOP "SUB" = Just sub
    matchUnaryOP "MUL" = Just mul
    matchUnaryOP "DIV" = Just Gcc.div
    matchUnaryOP "CEQ" = Just ceq
    matchUnaryOP "CGT" = Just cgt
    matchUnaryOP "CGTE" = Just cgte
    matchUnaryOP "ATOM" = Just atom
    matchUnaryOP "CONS" = Just cons
    matchUnaryOP "CAR" = Just car
    matchUnaryOP "CDR" = Just cdr
    matchUnaryOP "JOIN" = Just join_
    matchUnaryOP "RTN" = Just rtn
    matchUnaryOP _ = Nothing

    inverseCond :: [GccProg ()] -> [GccProg ()]
    inverseCond (cons@(Free (Inst CONS _)) : insts) =
        flipStack : cons : inverseCond insts
    inverseCond (x:xs) = x : inverseCond xs
    inverseCond [] = []



x = do
    scheme_sample :: SBS <- SBS.readFile "../scheme/5.scm"
    SBS.putStrLn scheme_sample
    schemeToSECD scheme_sample >>= putStrLn . ppShow
    schemeToGcc scheme_sample >>= \ (Right prog) -> putStrLn . codeGen $ prog
