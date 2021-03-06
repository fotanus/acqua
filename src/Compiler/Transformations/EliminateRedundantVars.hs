module Compiler.Transformations.EliminateRedundantVars where

import AcquaIR.Language as IR

eliminateRedundantVars :: IR.Program -> IR.Program
eliminateRedundantVars p = eliminateCallVars $ eliminateVarsOnBB p

eliminateCallVars :: IR.Program -> IR.Program
eliminateCallVars p = _eliminateCallVars p p


removeDeletes :: [Command] -> [Command]
removeDeletes [] = []
removeDeletes ((Delete _):cmds) = removeDeletes cmds
removeDeletes (c:cmds) = c:cmds

deleteCommands :: [Command] -> [Command]
deleteCommands ((Delete x):cmds) = (Delete x):(removeDeletes cmds)
deleteCommands _ = []

_eliminateCallVars :: IR.Program -> IR.Program -> IR.Program
_eliminateCallVars [] p = p
_eliminateCallVars (bb:bbs) p =
    if null (commands bb)
      then _eliminateCallVars bbs p
      else case last (commands bb) of
         Call _ callRecord retFn ->
             let
                labelNum = drop (length "then") (label bb)
                back = lookupBB p ("back" ++ labelNum)
                origBB = lookupBBWithIfForCall p (label bb)
             in if null (removeDeletes (commands back))
                then _eliminateCallVars bbs p
                else case head (removeDeletes (commands back)) of
                     AssignV var _   -> _eliminateCallVars bbs p'
                                        where
                                           p' = updateBB newBack $ updateBB newCall $ updateBB newOrig p
                                           newCall = bb { commands = (head (commands bb)):[Call var callRecord retFn] }
                                           newBack = back { commands = (deleteCommands (commands back)) ++ (tail (removeDeletes (commands back))) }
                                           newOrig = origBB { commands = ((commands origBB) ++ [AssignV var origClosName]) }
                                           SetCallRecordParam origClosName _ _ = (reverse (commands origBB))!!1
                     InnerCopy var _ -> _eliminateCallVars bbs p'
                                        where
                                           p' = updateBB newBack $ updateBB newCall $ updateBB newOrig p
                                           newCall = bb { commands = (head (commands bb)):[Call var callRecord retFn] }
                                           newBack = back { commands = (deleteCommands (commands back)) ++ (tail (removeDeletes (commands back))) }
                                           newOrig = origBB { commands = ((commands origBB) ++ [AssignV var origClosName]) }
                                           SetCallRecordParam origClosName _ _ = (reverse (commands origBB))!!1
                     _               -> error $ "first command on call block must be assignv " ++ (show (commands back))
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
                                  ((InnerCopy "resp" n1),(InnerCopy n2 "resp"):cs') ->
                                    (InnerCopy n2 n1):(eliminateVarsOnCmds cs')
                                  ((AssignI "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignI n2 n1):(eliminateVarsOnCmds cs')
                                  ((AssignL "resp" n1),(AssignV n2 "resp"):cs') ->
                                    (AssignL n2 n1):(eliminateVarsOnCmds cs')
                                  ((Op "resp" n1 opc n2),(AssignV n3 "resp"):cs') ->
                                    (Op n3 n1 opc n2):(eliminateVarsOnCmds cs')
                                  (_,_) -> c:(eliminateVarsOnCmds cs)
