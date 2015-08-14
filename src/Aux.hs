module Aux where

import Term

stripLambda (Lambda x t) = let (xs, t') = stripLambda t
                           in ((x:xs), t')
stripLambda t = ([],t)

rename fvs x = if x `elem` fvs
               then rename fvs (x ++ "'")
               else x

