-- | Main entry point to the application.
module Main where

import Term
import Printer
import Parser

-- | The main entry point.
main :: IO ()
main = do
          putStrLn ""
          prettyPrint (prettyTerm (FVarApp "x" [(ConApp "Nil" [FVarApp "y" []])]))
          putStrLn ""
          prettyPrint (parseExpr "f xs ys zs where f Nil (Cons(x,xs)) (Nil) = 0")
