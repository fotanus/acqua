module AcquaIR.Transformations.EliminateRedundantVars where

import AcquaIR.Language as IR

eliminateRedundantVars :: IR.Program -> IR.Program
eliminateRedundantVars [] = []
eliminateRedundantVars (bb:bbs) =
  bb':(eliminateRedundantVars bbs)
  where
    BB l n cs t = bb
    bb' = BB l n (eliminateBBCallNextVar cs) t

eliminateBBCallNextVar :: [Command] -> [Command]
eliminateBBCallNextVar [] = []
eliminateBBCallNextVar (c:cs) = case (c,cs) of
                                      ((Call _ l env),(AssignV x1 _):cs') ->
                                            (Call x1 l env):(eliminateBBCallNextVar cs')
                                      (_,_) -> c:(eliminateBBCallNextVar cs)


