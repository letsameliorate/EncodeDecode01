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

instance Eq Term where
  (==) (FVarApp x ts) (FVarApp x' ts') = (x == x') && (ts == ts')
  (==) (BVarApp i ts) (BVarApp i' ts') = (i == i') && (ts == ts')
  (==) (ConApp c ts) (ConApp c' ts') = (c == c') && (ts == ts')
  (==) (Lambda x t) (Lambda x' t') = (t == t')
  (==) (Let x t1 t2) (Let x' t1' t2') = (t1 == t1') && (t2 == t2')
  (==) (FunCall (f, ts)) (FunCall (f', ts')) = (f == f') && (ts == ts')
  (==) t@(Where (f, ts1) fds) t'@(Where (f', ts1') fds') | match t t' = let (funapps, ts2) = unzip fds
                                                                            (funapps', ts2') = unzip fds'
                                                                            (fs, tss3) = unzip funapps
                                                                            (fs', tss3') = unzip funapps'
                                                                        in (ts1 == ts1') && (fs == fs') && (tss3 == tss3') && (ts2 == ts2')
  (==) t t' = False

match (FVarApp x ts) (FVarApp x' ts') = (x == x') && (length ts == length ts')
match (BVarApp i ts) (BVarApp i' ts') = (i == i') && (length ts == length ts')
match (ConApp c ts) (ConApp c' ts') = (c == c') && (length ts == length ts')
match (Lambda x t) (Lambda x' t') = True
match (Let x t1 t2) (Let x' t1' t2') = True
match (FunCall (f, ts)) (FunCall (f', ts')) = (f == f') && (length ts == length ts')
match (Where (f, ts) fds) (Where (f', ts') fds') = (f == f') && (length ts == length ts') && (length fds == length fds')
match t t' = False

{-|
match (DFreeApp x dts) (DFreeApp x' dts') = (x == x') && (length dts == length dts')
match (DBoundApp i dts) (DBoundApp i' dts') = (i == i') && (length dts == length dts')
match (DConApp c dts) (DConApp c' dts') = (c == c') && (length dts == length dts')
match (DLambda x dt) (DLambda x' dt') = True
match (DLet x dt0 dt1) (DLet x' dt0' dt1') = True
match (DCase csel bs) (DCase csel' bs') = (length bs == length bs') && (all (\((c,xs,bt), (c',xs',bt')) -> ((c == c') && (length xs == length xs'))) (zip bs bs'))
match (DFunApp f dts) (DFunApp f' dts') = (f == f') && (length dts == length dts')
match (DWhere dt dts) (DWhere dt' dts') = (dts == dts')
match dt dt' = False
|-}