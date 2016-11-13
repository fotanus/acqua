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
      else case last (commands bb) of
         Call _ callRecord ->
             let
                labelNum = drop (length "then") (label bb)
                back = lookupBB p ("back" ++ labelNum)
                origBB = lookupBBWithIfForCall p (label bb)
             in if null (commands back)
                then _eliminateCallVars bbs p
                else case head (commands back) of
                     AssignV var _ ->
                         _eliminateCallVars bbs p'
                       where
                         p' = updateBB newBack $ updateBB newCall $ updateBB newOrig p
                         newCall = bb { commands = (head (commands bb)):[Call var callRecord] }
                         newBack = back { commands = tail (commands back) }
                         newOrig = origBB { commands = ((commands origBB) ++ [AssignV var origClosName]) }
                         SetCallRecordParam origClosName _ _ = (reverse (commands origBB))!!1
                     _ -> error $ "first command on call block must be assignv " ++ (show (commands back))
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
                                  ((Op "resp" n1 opc n2),(AssignV n3 "resp"):cs') ->
                                    (Op n3 n1 opc n2):(eliminateVarsOnCmds cs')
                                  (_,_) -> c:(eliminateVarsOnCmds cs)
