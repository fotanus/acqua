module Compiler.Transformations.EliminateRedundantVars where

import AcquaIR.Language as IR

eliminateRedundantVars :: IR.Program -> IR.Program
eliminateRedundantVars p = eliminateCallVars $ eliminateVarsOnBB p

eliminateCallVars :: IR.Program -> IR.Program
eliminateCallVars p = _eliminateCallVars p p

_eliminateCallVars :: IR.Program -> IR.Program -> IR.Program
_eliminateCallVars [] p = p
_eliminateCallVars (bb:bbs) p =
    if null (commands bb)
      then _eliminateCallVars bbs p
      else case head (commands bb) of
         Call resp closure -> case terminator bb of
                              Goto l ->
                                let back = lookupBB p l
                                in if null (commands back)
                                   then _eliminateCallVars bbs p
                                   else case head (commands back) of
                                        AssignV var "resp" ->
                                            _eliminateCallVars bbs p'
                                          where 
                                            p' = updateBB newBack $ updateBB newCall p
                                            newCall = bb { commands = [Call var closure] }
                                            newBack = back { commands = tail (commands back) }
         _ -> _eliminateCallVars bbs p

eliminateVarsOnBB :: IR.Program -> IR.Program
eliminateVarsOnBB [] = []
eliminateVarsOnBB (bb:bbs) =
    bb':(eliminateVarsOnBB bbs)
  where
    bb' = bb { commands = (eliminateVarsOnCmds (commands bb)) }


eliminateVarsOnCmds :: [Command] -> [Command]
eliminateVarsOnCmds [] = []
eliminateVarsOnCmds (c:cs) = case (c,cs) of
                                  ((AssignV "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignV n2 n1):(eliminateVarsOnCmds cs')
                                  ((AssignI "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignI n2 n1):(eliminateVarsOnCmds cs')
                                  ((AssignL "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignL n2 n1):(eliminateVarsOnCmds cs')
                                  (_,_) -> c:(eliminateVarsOnCmds cs)
