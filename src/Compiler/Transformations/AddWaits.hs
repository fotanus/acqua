module Compiler.Transformations.AddWaits where

import AcquaIR.Language as IR
import Debug.Trace

-- Function that should be called to add the waits in a program
addWaits :: IR.Program -> IR.Program
addWaits p = addWaitCommands p (blocksToWait p)

-- Given a program, gets the basic blocks which contain at least one call command.
-- Due to the compilation, those blocks are always a call followed by a goto.
blocksWithCall :: IR.Program -> [BasicBlock]
blocksWithCall [] = []
blocksWithCall (bb:basicblocks) =
    if length (commands bb) >= 2
      then case (commands bb) !! 1  of
           Call _ _ _ -> bb:(blocksWithCall basicblocks)
           _          -> blocksWithCall basicblocks
      else blocksWithCall basicblocks


-- for a given block which contains the call command, extract the block that will
-- be analised to add a Wait. This is possible because the compiled blocks with call always
-- have the same structure: A call followed by a goto. The next block always have as the first
-- command one assign from resp (result from call) to the variable name which we need to wait.
blocksToWait :: IR.Program -> [(Label,Name,Bool)]
blocksToWait prog = map extractLabelAndVar (blocksWithCall prog)
  where
    extractLabelAndVar bb =
        (l,v,retsFn)
      where
        Goto l = terminator bb
        Call v _ retsFn = last (commands bb)

-- For a given program and a set of labels and names, go through the basic blocks with the given
-- labels and add wait before the respectively variable is used. It is possible that a wait
-- should not be add on the same basic block given, for instance, there is a if before the variable
-- is used. In such cases, more Label,Name pairs are add to allow the wait to be add on other basic
-- blocks
addWaitCommands :: IR.Program -> [(Label,Name,Bool)] -> IR.Program
addWaitCommands p []          = p
addWaitCommands p ((lab,name,retsFn):basicblocks) =
    addWaitCommands p' bbs'
  where
    hasWait [] = False
    hasWait (c:cs) = if c == Wait then True else hasWait cs

    p' = addWaitOnBlock lab p name retsFn
    bbs' = if p == p' && not (hasWait (commands (lookupBB p' lab)))
           then (checkForExtraBlocks lab p name retsFn) ++ basicblocks
           else basicblocks



    -- for a given label, finds the block and add the wait command on it.
    -- This function only goes through the basic blocks and delegate the actual
    -- work for other functions
    addWaitOnBlock _ [] _ _             = error "Basic block not found"
    addWaitOnBlock l (bb:bbs) n retsFn  = if (label bb) == l
                                          then (addWaitCommand bb n retsFn):bbs
                                          else bb:(addWaitOnBlock l bbs n retsFn)
    addWaitCommand bb n retsFn =
        bb { commands = commands' }
      where
        -- needs cleanup
        removeDeletes [] = []
        removeDeletes (c:cs) = case c of Delete _ -> removeDeletes cs; _ -> c:(removeDeletes cs)
        commands' = if null (removeDeletes (commands bb))
                    then case (terminator bb) of
                      Return _ -> [Wait]
                      _        -> []
                    else (insertWait (commands bb) n retsFn)


    -- insert a wait command on the command list if applicable
    insertWait [] _ retsFn           = []
    insertWait (c:cs) varName retsFn =
      let
        insertCommands = if retsFn
                         then [Wait, OuterCopy varName varName]
                         else [Wait]
      in
        case c of
        Wait                  ->  c:cs
        AssignI n _           ->  if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        AssignL n _           ->  if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        OuterCopy n1 n2       ->  if varName == n1 || varName == n2
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        InnerCopy n1 n2       ->  if varName == n1 || varName == n2
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        AssignV n1 n2         ->  if varName == n1 || varName == n2
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        IR.Op _ n1 _ n2       ->  if varName == n1 || varName == n2
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        GetCallRecordFn n _       -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        GetCallRecordMissing n _  -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        GetCallRecordCount n _    -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        SetCallRecordFn n _       -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        SetCallRecordMissing n _  -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        SetCallRecordCount n _    -> if varName == n
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        SetCallRecordParam n _ n' -> if varName == n || varName == n'
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        Concat _ n n'             -> if varName == n || varName == n'
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        Concat3 _ n n' n''        -> if varName == n || varName == n' || varName == n''
                                  then insertCommands++[c]++cs
                                  else c:(insertWait cs varName retsFn)
        -- TODO: other list commands

        _ -> c:(insertWait cs name retsFn)

    -- This function assumes that the wait could not be add on a basic block with label l,
    -- and checks the basic block terminator to define what other basic blocks the program
    -- can reach from this one. It then creates a pair (Label,Name) to be evaluated and find
    -- the correct place to add the label
    checkForExtraBlocks _ [] _ retsFn       = error "Basic block not Found"
    checkForExtraBlocks l (bb:bbs) n retsFn = if (label bb) == l
                                              then case (terminator bb) of
                                                   Return _ -> []
                                                   Goto l' -> [(l',n,retsFn)]
                                                   If _ l' -> [(l',n,retsFn), ((label (head bbs)),n,retsFn)]
                                                   Empty -> error "block ends with emtpy"
                                              else checkForExtraBlocks l bbs n retsFn

