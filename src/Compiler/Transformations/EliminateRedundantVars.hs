module Compiler.Transformations.EliminateRedundantVars where

import AcquaIR.Language as IR

eliminateRedundantVars :: IR.Program -> IR.Program
eliminateRedundantVars [] = []
eliminateRedundantVars (bb:bbs) =
  bb':(eliminateRedundantVars bbs)
  where
    BB l n cs t = bb
    bb' = BB l n (eliminateResp (eliminateCallReuse cs)) t

eliminateCallReuse :: [Command] -> [Command]
eliminateCallReuse [] = []
eliminateCallReuse (c:cs) = case (c,cs) of
                                      ((Call _ l env),(AssignV x1 _):cs') ->
                                            (Call x1 l env):(eliminateCallReuse cs')
                                      (_,_) -> c:(eliminateCallReuse cs)

eliminateResp :: [Command] -> [Command]
eliminateResp [] = []
eliminateResp (c:cs) = case (c,cs) of
                                  ((AssignV "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignV n2 n1):(eliminateResp cs')
                                  ((AssignI "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignI n2 n1):(eliminateResp cs')
                                  ((AssignL "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignL n2 n1):(eliminateResp cs')
                                  (_,_) -> c:(eliminateResp cs)
