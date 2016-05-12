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
blocksWithCall (bb:bbs) =
  let
    callFold c = (|| case c of {Call _ _ -> True; _ -> False})
  in
    if foldr callFold False (commands bb)
    then bb:(blocksWithCall bbs)
    else blocksWithCall bbs


-- for a given block which contains the call command, extract the block that will
-- be analised to add a Wait. This is possible because the compiled blocks with call always
-- have the same structure: A call followed by a goto. The next block always have as the first
-- command one assign from resp (result from call) to the variable name which we need to wait.
blocksToWait :: IR.Program -> [(Label,Name)]
blocksToWait prog = map extractLabelAndVar (blocksWithCall prog)
  where
    extractLabelAndVar bb = case (terminator bb) of
                            Goto l -> (l, (getFirstAssignVar l prog))

    getFristAssignVar l []       = error $ "Can't find label " ++ l ++ " on program"
    getFirstAssignVar l (bb:bbs) = if (label bb) == l
                                   then case (head (commands bb)) of
                                        AssignV var _ -> var
                                        _             -> error $ "Block " ++ l ++ " do not start with an assignment"
                                   else getFirstAssignVar l bbs



-- For a given program and a set of labels and names, go through the basic blocks with the given
-- labels and add wait before the respectively variable is used. It is possible that a wait
-- should not be add on the same basic block given, for instance, there is a if before the variable
-- is used. In such cases, more Label,Name pairs are add to allow the wait to be add on other basic
-- blocks
addWaitCommands :: IR.Program -> [(Label,Name)] -> IR.Program
addWaitCommands p []          = p
addWaitCommands p ((l,n):bbs) =
    addWaitCommands p' bbs'
  where
    p' = addWaitOnBlock l p n
    bbs' = if p == p'
           then (checkForExtraBlocks l p n) ++ bbs
           else bbs



    -- for a given label, finds the block and add the wait command on it.
    -- This function only goes through the basic blocks and delegate the actual
    -- work for other functions
    addWaitOnBlock l [] n       = error "Basic block not found"
    addWaitOnBlock l (bb:bbs) n = if (label bb) == l
                                  then (addWaitCommand bb n):bbs
                                  else bb:(addWaitOnBlock l bbs n)

    addWaitCommand bb n =
        bb { commands = commands' }
      where
        firstAssignment = head (commands bb) -- this might not work for basic blocks added by checkForExtraBlocks
        remainingCommands = tail (commands bb)
        commands' = if null (commands bb)
                    then []
                    else firstAssignment:(insertWait remainingCommands n)


    -- insert a wait command on the command list if applicable
    insertWait [] _ = []
    insertWait (c:cs) name =
        case c of
        Wait          -> c:cs
        AssignI n _   -> if name == n
                         then Wait:c:cs
                         else c:(insertWait cs name)
        AssignL n _   -> if name == n
                         then Wait:c:cs
                         else c:(insertWait cs name)
        AssignV n1 n2 -> if name == n1 || name == n2
                         then Wait:c:cs
                         else c:(insertWait cs name)
        IR.Op n1 _ n2 -> if name == n1 || name == n2
                         then Wait:c:cs
                         else c:(insertWait cs name)
        _ -> c:(insertWait cs name)

    -- This function assumes that the wait could not be add on a basic block with label l,
    -- and checks the basic block terminator to define what other basic blocks the program
    -- can reach from this one. It then creates a pair (Label,Name) to be evaluated and find
    -- the correct place to add the label
    checkForExtraBlocks _ [] _     = error "Basic block not Found"
    checkForExtraBlocks l (bb:bbs) n = if (label bb) == l
                                       then case (terminator bb) of
                                            Return _ -> []
                                            Goto lab -> [(lab,n)]
                                            If _ lab -> [(lab,n), ((label (head bbs)),n)]
                                       else checkForExtraBlocks l bbs n

