{-# LANGUAGE OverloadedStrings #-}

module SECD where

import Control.Applicative
import Data.Attoparsec as AP
import Data.List
import Data.Monoid
import System.Process
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
    let secdscheme = "/home/mf/pc/SECD/secdscheme"
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
    "(lambda (iwstate gcode)" :
    " (let (" :
    " (cons 0" :
    "  (lambda (aistate wstate)" :
    "   '(0 3))))))" :
    []


-- | i just add this because i fixed the broken Show instance in
-- atto-lisp, and i wanted to have it back.  (-:
prettyShow :: AL.Lisp -> String
prettyShow = f
  where
    f (AL.Symbol a) = T.unpack a
    f (AL.String t)  = show (T.unpack t)
    f (AL.Number n) = show n
    f (AL.List l) = "(" ++ intercalate " " (map f l) ++ ")"
    f (AL.DotList l d) = "(" ++ intercalate " " (map f l) ++ " . " ++ f d ++ ")"


----------------------------------------------------------------------
-- the actual SECG-Gcc compiler
----------------------------------------------------------------------

-- order of cons
-- definition of ap
-- inline lambdas => labels
-- align argument frame addresses

sexpToGcc :: AL.Lisp -> GccProgram String ()
sexpToGcc sexp = error (ppShow sexp ++ "\n\n" ++ prettyShow sexp)


x = schemeToGcc scheme_sample
