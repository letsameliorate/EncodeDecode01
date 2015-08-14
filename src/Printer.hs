module Printer where

import Term
import Aux
import Text.PrettyPrint.HughesPJ

instance Show Term where
    show t = render (prettyTerm t)

prettyTerm (FVarApp x ts) = if ts == []
                            then (text x)
                            else parens ((text x) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (BVarApp i ts) = if ts == []
                            then (text "#") <> (int i)
                            else parens ((int i) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (ConApp c ts) = if ts == []
                           then (text c)
                           else parens ((text c) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm t@(Lambda _ _) = let (xs, t') = stripLambda t
                            in (text "\\") <> (hsep (map text xs) <> (text ".") <> (prettyTerm t'))
prettyTerm (Let x t t') = parens ((((text "let") <+> (text x) <+> (text "=")) <+> (prettyTerm t)) $$ (prettyTerm t'))
prettyTerm (FunCall (f, ts)) = if ts == []
                               then (text f)
                               else parens ((text f) <+> (hcat (punctuate space (map prettyTerm ts))))
prettyTerm (Where (f, ts) fds) = parens (((text f) <+> (hcat (punctuate space (map prettyTerm ts)))) $$ (text "where") $$ (vcat (map prettyFunDef fds)))
                                 where
                                       prettyFunDef (f, ts, t) = ((text f) <+> (hcat (punctuate space (map prettyTerm ts)))) <+> (text "=") <+> (prettyTerm t)

prettyPrint a = print a