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
  (==) (Where fapp fds) (Where fapp' fds') = (fapp == fapp') && (fds == fds')
  (==) t t' = False

{-|
instance Eq DTerm where
    (==) (DFreeApp x dts) (DFreeApp x' dts') = (x == x') && (dts == dts')
    (==) (DBoundApp i dts) (DBoundApp i' dts') = (i == i') && (dts == dts')
    (==) (DConApp c dts) (DConApp c' dts') = (c == c') && (dts == dts')
    (==) (DLambda x dt) (DLambda x' dt') = (x == x') && (dt == dt')
    (==) (DLet x dt0 dt1) (DLet x' dt0' dt1') = (dt0 == dt0') && (dt1 == dt1')
    (==) dt@(DCase csel bs) dt'@(DCase csel' bs') | match dt dt' = (csel == csel') && (all (\((c,xs,bt), (c',xs',bt')) -> (bt == bt')) (zip bs bs'))
    (==) (DFunApp f dts) (DFunApp f' dts') = (f == f') && (dts == dts')
    (==) (DWhere dt dts) (DWhere dt' dts') = (dt == dt') && (dts == dts')
    (==) dt dt1 = False


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