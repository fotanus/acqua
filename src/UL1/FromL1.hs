module UL1.FromL1 where

import L1.Language as L1
import UL1.Language as UL1

fromL1 :: L1.Term -> UL1.Term
fromL1 _ = UL1.Ident "x"
