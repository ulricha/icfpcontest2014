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
import qualified Data.ByteString as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as IO

import Gcc
import GccMacros


schemeToGcc :: SC.SBS -> IO (Either String (GccProgram String ()))
schemeToGcc = fmap secdToGcc . schemeToSECD


schemeToSECD :: SC.SBS -> IO SC.SBS
schemeToSECD scheme = do
    secdscheme <- maybe (error "please set shell variable SECD_SCHEME") id .
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

sexpToGcc :: AL.Lisp -> GccProg ()
sexpToGcc sexp@(AL.List secd) = do
      insts
      flipStackAsm
      mapM_ (\ (n, r) -> label n >> r) $ routines
  where
    (_, routines, insts) = lexer 0 [] [] secd

    lexer :: Int -> [Routine] -> [GccProg ()] -> [AL.Lisp] -> (Int, [Routine], GccProg ())
    lexer label routines insts (AL.Symbol "LDC" : AL.Number i : secd) =
        lexer label routines (insts ++ [ldc (round i)]) secd

    -- LDC may also receive '()' as argument in secd, but in gcc
    -- forces us to make up an integer.
    lexer label routines insts (AL.Symbol "LDC" : AL.List [] : secd) =
        lexer label routines (insts ++ [ldc 0xdeadbeef]) secd

    lexer label routines insts (AL.Symbol "LD" : AL.Number i1 : AL.Number i2 : secd) =
        lexer label routines (insts ++ [ld (round i1) (round i2)]) secd

    lexer label routines insts (AL.Symbol (matchUnaryOP -> Just unaryOp) : secd) =
        lexer label routines (insts ++ [unaryOp]) secd

    lexer label routines insts (AL.Symbol "SEL" : secd) =
        error "sexpToGcc.SEL"

    lexer label routines insts (AL.Symbol "LDF" : (AL.List [AL.List _, AL.List fun]) : secd) =
        -- FIXME: if function arguments are actually used, this won't work any more!
        let label'                          = label + 1
            (label'', routines', subinsts)  = lexer label' [] [] fun
            routines''                      = (show label, subinsts) : routines ++ routines'
            insts'                          = insts ++ [ldf (show label)]
        in lexer label'' routines'' insts' secd

    lexer label routines insts (AL.Symbol "AP" : AL.Number i : secd) =
        lexer label routines (insts ++ [ap (round i)]) secd

    -- in secd, "ap" has a variable list of parameters!  :(
    lexer label routines insts (AL.Symbol "AP" : secd) =
        lexer label routines (insts ++ [{- FIXME -}]) secd

    lexer label routines insts (AL.Symbol "DUM" : AL.Number i : secd) =
        lexer label routines (insts ++ [dum (round i)]) secd

    lexer label routines insts (AL.Symbol "RAP" : AL.Number i : secd) =
        lexer label routines (insts ++ [rap (round i)]) secd

    lexer label routines insts [] = (label, routines, sequence_ $ inverseCond insts)

    lexer l r i x = error $ "sexpToGcc: unmatched pattern!\n" ++ ppShow (l, r, i, x)

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



--- error (ppShow sexp ++ "\n\n" ++ prettyShow sexp)


x = do
    scheme_sample :: SBS <- SBS.readFile "../scheme/1.scm"
    schemeToSECD scheme_sample >>= putStrLn . ppShow
    schemeToGcc scheme_sample >>= \ (Right prog) -> putStrLn . codeGen $ prog
