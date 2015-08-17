-- | Main entry point to the application.
module Main where

import Term
import Printer
import Parser
import EncodeDecode

main :: IO ()
main = do
          putStrLn ""
          let e1 = parseExpr "let v' = 0 in Cons(x,(f xs ys 0 where f (Cons(x',Cons(x'',xs))) ys v = f xs ys v | f Nil Nil v = v')))"
              -- e2 = encode e1
          prettyPrint e1
          -- prettyPrint e2
