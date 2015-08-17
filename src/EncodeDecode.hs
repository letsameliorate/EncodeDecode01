module EncodeDecode where

import Term

encode (Where (f, ts) fds) vs = let f' = "encode_" ++ f
                                    ts' = ts
                                    fds' = fds
                                in (Where (f', ts') fds')