module Compiler.Transformations.AddWaits where

import AcquaIR.Language as IR


blocksWithCall :: IR.Program -> [BasicBlock]
blocksWithCall [] = []
blocksWithCall (bb:bbs) =
  let
    callFold c = (|| case c of {Call _ _ -> True; _ -> False})
  in
    if foldr callFold False (commands bb)
    then bb:(blocksWithCall bbs)
    else blocksWithCall bbs


blocksToWait :: IR.Program -> [Label]
blocksToWait prog = (map (\bb-> case (terminator bb) of { Goto l -> l }) (blocksWithCall prog))

addWaits :: IR.Program -> IR.Program
addWaits p = aaddWaits p (blocksToWait p)

aaddWaits :: IR.Program -> [Label] -> IR.Program
aaddWaits [] _ = []
aaddWaits (bb:bbs) btw =
  bb':(aaddWaits bbs btw)
  where
    BB l n' cs t = bb
    AssignV waitVar _ = head cs
    bb' = if l `elem` btw
          then BB l n' ((head cs):(addWaits' (tail cs) [waitVar])) t
          else bb
    addWaits' [] vars  = []
    addWaits' (c':cs') vars = case c' of
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

