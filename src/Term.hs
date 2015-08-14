module Term where

type FVar = String
type BVar = Int
type Con  = String
type FunName  = String

type FunApp = (FunName, [Term])
type FunDef = (FunApp, Term)

data Term = FVarApp FVar [Term]
          | BVarApp BVar [Term]
          | ConApp Con [Term]
          | Lambda String Term
          | Let String Term Term
          | FunCall FunApp
          | Where FunApp [FunDef]
 deriving (Show)