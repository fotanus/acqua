module Compiler.Transformations.AddEnvNews where

import Data.List (intersect)
import Data.Maybe
import Debug.Trace


import AcquaIR.Language
import AcquaIR.BasicBlockDependencyTable

addEnvNews :: Program -> Program
addEnvNews p  = _addEnvNews p reacheableNamesPerBB namesPerBB
  where
    namesPerBB = namesOnBasicBlocks p
    reacheableNamesPerBB = namesDependencyTable p  

_addEnvNews :: Program -> NameDependencyTable -> NameDependencyTable -> Program
_addEnvNews [] _ _ = []
_addEnvNews (bb:bbs) reacheableNamesPerBB n2 = 
  let
    (BB l n cmds t) = bb
    cmds' = addEnvAdds cmds ((searchNames l reacheableNamesPerBB) `intersect` (searchNames l n2))
  in
    (BB l n cmds' t) : _addEnvNews bbs reacheableNamesPerBB n2
  where
    addEnvAdds [] _ = []
    addEnvAdds (cmd:cmds) names = case cmd of
                                (Call n1 n2 envId) -> (map (\n -> EnvAddL envId n n) names) ++ [(Call n1 n2 envId)] ++ (addEnvAdds cmds names)
                                _                -> cmd : (addEnvAdds cmds names)



