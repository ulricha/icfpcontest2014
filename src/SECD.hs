{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module SECD where

import Control.Applicative
import Control.Monad.Free
import Data.Attoparsec as AP
import Data.List
import Data.Monoid
import System.Process
import System.Environment
import Text.Show.Pretty

import qualified Data.AttoLisp as AL
import qualified Data.ByteString as SBS
import qualified Data.String.Conversions as SC
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


scheme_sample :: SC.SBS
scheme_sample = mconcat $
    "((lambda (iwstate gcode)" :
    " (let (" :
    " (cons 0" :
    "  (lambda (aistate wstate)" :
    "   '(0 3)))))) 3 3)" :
    []


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

type Routines = [(String, GccProg ())]
type Instructions = [GccProg ()]

sexpToGcc :: AL.Lisp -> GccProg ()
sexpToGcc sexp@(AL.List secd) = do
      sequence $ inverseCond insts
      flipStackAsm
      mapM_ (\ (n, r) -> label n >> r) $ routines
  where
    (routines, insts) = lexer [] [] secd

    lexer :: Routines -> Instructions -> [AL.Lisp] -> (Routines, Instructions)
    lexer routines insts (AL.Symbol "LDC" : AL.Number i : secd) =
        lexer routines (insts ++ [ldc (round i)]) secd

    lexer routines insts (AL.Symbol "LD" : AL.Number i1 : AL.Number i2 : secd) =
        lexer routines (insts ++ [ld (round i1) (round i2)]) secd

    lexer routines insts (AL.Symbol (matchUnaryOP -> Just unaryOp) : secd) =
        lexer routines (insts ++ [unaryOp]) secd

    lexer routines insts (AL.Symbol "SEL" : secd) =
        error "sexpToGcc.SEL"

    lexer routines insts (AL.Symbol "LDF" : secd) =
        error $ "sexpToGcc.LDF\n" ++ ppShow insts

    lexer routines insts (AL.Symbol "AP" : AL.Number i : secd) =
        lexer routines (insts ++ [ap (round i)]) secd

    lexer routines insts (AL.Symbol "DUM" : AL.Number i : secd) =
        lexer routines (insts ++ [dum (round i)]) secd

    lexer routines insts (AL.Symbol "RAP" : AL.Number i : secd) =
        lexer routines (insts ++ [rap (round i)]) secd

    lexer routines insts [] = (routines, insts)

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


{-
  , Symbol "LDF"
  , List
      [ List [ Symbol "iwstate" , Symbol "gcode" ]
      , List
          [ Symbol "LDC"
          , List []
          , Symbol "LDC"
          , Number 0
          , Symbol "CONS"
          , Symbol "LDF"
          , List
              [ List [ Symbol "cons" ]
              , List [ Symbol "LDC" , List [] , Symbol "RTN" ]
              ]
          , Symbol "AP"
          , Symbol "RTN"
          ]
      ]
  , Symbol "AP"
  , Number 2
  ]

-}


--- error (ppShow sexp ++ "\n\n" ++ prettyShow sexp)


x = schemeToGcc scheme_sample
