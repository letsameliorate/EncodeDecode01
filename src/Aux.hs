module Aux where

import Term


stripLambda (Lambda x t) = let (xs, t') = stripLambda t
                           in ((x:xs), t')
stripLambda t = ([],t)

rename fvs x = if x `elem` fvs
               then rename fvs (x ++ "'")
               else x

frees t = frees' t []

frees' (FVarApp x ts) fvs = if x `elem` fvs
                            then foldr (\t fvs -> frees' t fvs) fvs ts
                            else foldr (\t fvs -> frees' t fvs) (x:fvs) ts
frees' (BVarApp i ts) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (ConApp c ts) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (Lambda x t) fvs = frees' t fvs
frees' (Let x t t') fvs = frees' t' (frees' t fvs)
frees' (FunCall (f,ts)) fvs = foldr (\t fvs -> frees' t fvs) fvs ts
frees' (Where (f, ts) fds) fvs = foldr (\(f, ts, t) fvs -> frees' t fvs) (foldr (\t fvs -> frees' t fvs) fvs ts) fds

bounds t = bounds' t []
bounds' (FVarApp x ts) bvs = foldr (\t bvs -> bounds' t bvs) bvs ts

shift 0 d t = t
shift i d (FVarApp x ts) = FVarApp x (map (shift i d) ts)
shift i d (BVarApp j ts) = if j >= d
                           then BVarApp (j+i) (map (shift (j+i) d) ts)
                           else BVarApp j (map (shift j d) ts)
shift i d (ConApp c ts) = ConApp c (map (shift i d) ts)
shift i d (Lambda x t) = Lambda x (shift i (d+1) t)
shift i d (Let x t t') = Let x (shift i d t) (shift i (d+1) t')
shift i d (FunCall (f, ts)) = FunCall (f, map (shift i d) ts)
shift i d (Where (f, ts) fds) = Where (f, map (shift i d) ts) (map (\(f,ts,t) -> (f,ts, shift i t t)) fds)
  -- adjust depth in (shift i d t) based on bound variables in ts in (f, ts, t)
