-- | Main entry point to the application.
module Main where

import Term
import Printer

-- | The main entry point.
main :: IO ()
main = do
          putStrLn ""
          prettyPrint (prettyTerm (FVarApp "x" [(ConApp "Nil" [])]))
          putStrLn ""
