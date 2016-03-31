module Compiler.Transformations.AddFreeVariables where

import Data.List (intersect)

import AcquaIR.Language
import AcquaIR.BasicBlockDependencyTable

addFreeVariables :: Program -> Program
addFreeVariables p  = _addFreeVariables p reacheableNamesPerBB namesPerBB
  where
    namesPerBB = namesOnBasicBlocks p
    reacheableNamesPerBB = namesDependencyTable p

_addFreeVariables :: Program -> NameDependencyTable -> NameDependencyTable -> Program
_addFreeVariables [] _ _ = []
_addFreeVariables (bb:bbs) reacheableNamesPerBB namesPerBB =
  let
    (BB l n cmds t) = bb
    cmds' = addEnvAdds cmds ((searchNames l reacheableNamesPerBB) `intersect` (searchNames l namesPerBB))
  in
    (BB l n cmds' t) : _addFreeVariables bbs reacheableNamesPerBB namesPerBB
  where
    addEnvAdds [] _ = []
    addEnvAdds (cmd:cmds) names = case cmd of
                                (Call n1 n2 envId) -> (map (\n -> EnvAddL envId n n) names) ++ [(Call n1 n2 envId)] ++ (addEnvAdds cmds names)
                                _                -> cmd : (addEnvAdds cmds names)
