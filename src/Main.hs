-- | Main entry point to the application.
module Main where

import Term
import Printer
import Parser

main :: IO ()
main = do
          putStrLn ""
          -- prettyPrint (prettyTerm (FVarApp "x" [(ConApp "Nil" [FVarApp "y" []])]))
          prettyPrint (parseExpr "let v = 0 in Cons(x,(f xs ys v where f (Cons(x',Cons(x'',xs))) ys v = xs | f Nil Nil v = v)))")
          putStrLn ""
