{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module SECD where

import Control.Applicative
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

sexpToGcc :: AL.Lisp -> GccProg ()
sexpToGcc sexp@(AL.List insts) = f [] insts
  where
    f :: [(String, GccProg ())] -> [AL.Lisp] -> GccProg ()
    f routines (AL.Symbol "LDC" : AL.Number i : insts) =
        ldc (round i) >> f routines insts

    f routines (AL.Symbol "LD" : AL.Number i1 : AL.Number i2 : insts) =
        ld (round i1) (round i2) >> f routines insts

    f routines (AL.Symbol (g -> Just unaryOp) : insts) =
        unaryOp >> f routines insts

    f routines (AL.Symbol "SEL" : insts) =
        error "sexpToGcc.SEL"

    f routines (AL.Symbol "JOIN" : insts) =
        join_ >> f routines insts

    f routines (AL.Symbol "LDF" : insts) =
        error $ "sexpToGcc.LDF\n" ++ ppShow insts

    f routines (AL.Symbol "AP" : AL.Number i : insts) =
        ap (round i) >> f routines insts

    f routines (AL.Symbol "DUM" : AL.Number i : insts) =
        dum (round i) >> f routines insts

    f routines (AL.Symbol "RAP" : AL.Number i : insts) =
        rap (round i) >> f routines insts

    f routines [] = rtn

    g :: T.Text -> Maybe (GccProg ())
    g "ADD" = Just add
    g "SUB" = Just sub
    g "MUL" = Just mul
    g "DIV" = Just Gcc.div
    g "CEQ" = Just ceq
    g "CGT" = Just cgt
    g "CGTE" = Just cgte
    g "ATOM" = Just atom
    g "CONS" = Just cons
    g "CAR" = Just car
    g "CDR" = Just cdr
    g "RTN" = Just rtn
    g _ = Nothing


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
