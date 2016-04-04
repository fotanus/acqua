module AcquaIR.BasicBlockDependencyTable where

import AcquaIR.Language
import Data.Set (Set, empty, union, difference, insert, toList)
import Data.Maybe


-- Search a name on a name dependency table
searchNames :: Name -> NameDependencyTable -> [Name]
searchNames n ndt = fromJust $ lookup n ndt

-- this table shows what names can be reacheable for a given basic block in every possible execution.
-- It does not include the variables on this basic block.
-- example: [("main",["var0","var1","x","resp","fibo","fn","var2","var3","var4","var5","var6","var7","x"]),
-- ("_fn_0",["resp","fibo","fn","var2","var3","var4","var5","var6","var7","x"]),("dummy0",[]),("back0",[]),("then0",[])]
--
type NameDependencyTable = [(Label, [Name])]

namesDependencyTable :: Program -> NameDependencyTable
namesDependencyTable p = map labelListToNames dependencyTable
  where
    labelListToNames entry = ((fst entry), (concat (map namesForLabel (snd entry))))
    namesForLabel l = fromJust (lookup l names)
    dependencyTable = basicBlockDependencyTable p
    names = namesOnBasicBlocks p


-- returns the names present on each program basic block.
-- example: [("main",fromList ["fibo","fn","resp","var8","var9"]),("_fn_0",fromList ["var0","var1","x"]),
-- ("dummy0",fromList ["fibo","fn","var2","var3","var4","var5","var6","var7","x"]),("back0",fromList []),("then0",fromList ["resp"])]
namesOnBasicBlocks :: Program -> NameDependencyTable
namesOnBasicBlocks [] = []
namesOnBasicBlocks (bb:bbs) = ((label bb), (toList (namesPerCmds (commands bb)))) : (namesOnBasicBlocks bbs)

namesPerCmds :: [Command] -> Set Name
namesPerCmds [] = empty
namesPerCmds (cmd:cmds) = (namesPerCmd cmd) `union` (namesPerCmds cmds)  

namesPerCmd :: Command -> Set Name
namesPerCmd cmd = case cmd of
                      Call n1 n2 _ -> insert n1 (insert n2 empty)
                      Op n1 _ n2 -> insert n1 (insert n2 empty)
                      AssignI n1 _ -> insert n1 empty
                      AssignV n1 n2 -> insert n1 (insert n2 empty)
                      _ -> empty


-- this table shows what basic blocks can be reached from a given basic block
-- example:
-- [("main",["_fn_0","then0","back0"]),("_fn_0",["then0","back0"]),("dummy0",["back0"]),("back0",[]),("then0",["back0"])]

type BBDependencyTable = [(Label, [Label])]

basicBlockDependencyTable :: Program -> BBDependencyTable
basicBlockDependencyTable p = _basicBlockDependencyTable p p

_basicBlockDependencyTable :: Program -> Program -> BBDependencyTable
_basicBlockDependencyTable [] _ = []
_basicBlockDependencyTable (bb:bbs) p =
  let l = label bb
  in (l, (findNeighbors bb p)) : (_basicBlockDependencyTable bbs p)

findNeighbors :: BasicBlock -> Program -> [Label]
findNeighbors (BB l _ cmds t) p = thisBBLabels ++ neighborLabels
  where
    thisBBLabels = (labelForCommands cmds) ++ (labelForTerminator t l p)
    neighborLabels = concat (map (\l' -> findNeighbors (findBB l' p) p) thisBBLabels)

labelForTerminator :: Terminator -> Label -> Program -> [Label]
labelForTerminator (Goto l) _ _ = [l]
labelForTerminator (If _ l) thisBBLabel p = [l] ++ [(nextBBLabel thisBBLabel p)]
labelForTerminator _ _ _ = []

labelForCommands :: [Command] -> [Label]
labelForCommands [] = []
labelForCommands (cmd:cmds) = (labelForCommand cmd cmds) ++ (labelForCommands cmds)

labelForCommand :: Command -> [Command] -> [Label]
labelForCommand (AssignL _ l) ((AssignV "fn" _):_) = [l]
labelForCommand _ _ = []

findBB :: Label -> Program -> BasicBlock
findBB l [] = error ("searching for non-existing bb " ++ l)
findBB l (bb:bbs) = if (label bb) == l
                      then bb
                      else findBB l bbs

nextBBLabel :: Label -> Program -> Label
nextBBLabel l [] = error ("searching for non-existing bb (or there is an if in the last bb): " ++ l)
nextBBLabel l (_:[]) = error ("searching for non-existing bb (or there is an if in the last bb): " ++ l)
nextBBLabel l (bb:bb2:bbs) = if (label bb) == l
                      then (label bb2)
                      else nextBBLabel l (bb2:bbs)
