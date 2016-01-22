module AcquaIR.Transformations.AddWaits where

import AcquaIR.Language as IR

addWaits :: IR.Program -> IR.Program
addWaits [] = []
addWaits (bb:bbs) =
  bb':(addWaits bbs)
  where
    BB l n' c t = bb
    bb' = BB l n' (addWaits' c [])  t
    addWaits' [] vars  = if null vars then [] else [Wait]
    addWaits' (c':cs') vars = case c' of
                           Call ret _ _ -> c':(addWaits' cs' (ret:vars))
                           AssignI n _ -> if elem n vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           AssignL n _ -> if elem n vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           AssignV n1 n2 -> if elem n1 vars || elem n2 vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           IR.Op n1 _ n2 -> if elem n1 vars || elem n2 vars
                                            then Wait:c':(addWaits' cs' [])
                                            else c':(addWaits' cs' vars)
                           _ -> c':(addWaits' cs' vars)

