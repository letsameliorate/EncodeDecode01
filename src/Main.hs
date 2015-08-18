-- | Main entry point to the application.
module Main where

import Term
import Printer
import Parser
import EncodeDecode

main :: IO ()
main = do
          putStrLn ""
          let e1 = parseExpr "f xt yt v where f E E v = v | f E B(y,yt1,yt2) v = v | f B(x,xt1,xt2) E v = v | f B(x,xt1,xt2) B(y,yt1,yt2) v = p x y (f xt1 yt1 v) (f xt2 yt2 v)"
              e2 = encode e1
          prettyPrint e1
          putStrLn ""
          prettyPrint e2
