module AcquaIR.BasicBlockDependencyTable where

import AcquaIR.Language

type DependencyTable = [(Label, [Label])]

parseTable :: Program -> DependencyTable
parseTable p = _parseTable p p

_parseTable :: Program -> Program -> DependencyTable
_parseTable [] _ = []
_parseTable (bb:bbs) p =
  let l = label bb
  in (l, (findNeighbors bb p)) : (_parseTable bbs p)

findNeighbors :: BasicBlock -> Program -> [Label]
findNeighbors (BB _ _ cmds t) p =  (labelForCommands cmds) ++ (labelForTerminator t)

labelForTerminator :: Terminator -> [Label]
labelForTerminator (Goto l) = [l]
labelForTerminator (If _ l) = [l]
labelForTerminator _ = []

labelForCommands :: [Command] -> [Label]
labelForCommands [] = []
labelForCommands (cmd:cmds) = (labelForCommand cmd) ++ (labelForCommands cmds)

labelForCommand :: Command -> [Label]
labelForCommand (AssignV "fn" l) = [l]
labelForCommand _ = []
