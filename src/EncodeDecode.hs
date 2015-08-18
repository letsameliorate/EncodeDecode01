module EncodeDecode where

import Term

encode (Left a) = Left a
encode (Right a) = Right (encode' a)

encode' (FVarApp x ts) c= FVarApp x (map encode' ts)
encode' 
encode' (Where (f, ts) fds) = let f' = "encode_" ++ f
                                  ts' = ts
                                  fds' = fds
                              in (Where (f', ts') fds')