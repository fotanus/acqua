module Compiler.Transformations.AddEnvNews where

import Data.Set
import AcquaIR.Language
import AcquaIR.BasicBlockDependencyTable

addEnvNews :: Program -> Program
addEnvNews [] = []
addEnvNews (bb:bbs) = (bb:bbs)
  where
    graph = parseTable (bb:bbs)
    names = namesPerBasicBlock (bb:bbs)

namesPerBasicBlock :: Program -> [(Label, (Set Name))]
namesPerBasicBlock [] = []
namesPerBasicBlock (bb:bbs) = ((label bb), (namesPerCmds (commands bb))) : (namesPerBasicBlock bbs)

namesPerCmds :: [Command] -> Set Name
namesPerCmds [] = empty
namesPerCmds (cmd:cmds) = union (namesPerCmd cmd) (namesPerCmds cmds)

namesPerCmd :: Command -> Set Name
namesPerCmd cmd = case cmd of
                      Call n1 n2 _ -> insert n1 (insert n2 empty)
                      Op n1 _ n2 -> insert n1 (insert n2 empty)
                      AssignI n1 _ -> insert n1 empty
                      AssignV n1 n2 -> insert n1 (insert n2 empty)
                      _ -> empty




