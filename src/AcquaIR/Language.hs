module AcquaIR.Language where

import AcquaIR.Colors

type Label = String
type Name = String
type EnvId = String

type Program = [BasicBlock]

data BasicBlock = BB {
  label :: Label,
  size :: Int,
  commands :: [Command],
  terminator :: Terminator
  } deriving (Eq,Ord,Show,Read)

data Command
  = Call Name Name
  | Op Name OpCode Name
  | AssignI Name Int
  | AssignL Name Label
  | AssignV Name Name
  | NewClosure Name Int
  | SetClosureFn Name Label
  | GetClosureFn Name Label
  | SetClosureMissing Name Name
  | SetClosureMissingI Name Int
  | GetClosureMissing Name Name
  | SetClosureCount Name Name
  | SetClosureCountI Name Int
  | GetClosureCount Name Name
  | SetClosureParam Name Name Name
  | SetClosureParamI Name Int Name
  | SetClosureParamIL Name Int Name
  | GetClosureParam Name Int Name
  | Wait
  deriving (Eq,Ord,Show,Read)

data Terminator
  = Goto Label
  | Return Name
  | If Name Label
  | Empty
  deriving (Eq,Ord,Show,Read)

data OpCode
  = And
  | Or
  | Add
  | Sub
  | Mult
  | Div
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Lesser
  | LesserEqual
  deriving (Eq,Ord,Show,Read)

lookupBB :: Program -> Label -> BasicBlock
lookupBB [] l = error $ "lookup bb with label " ++ l ++ " not found"
lookupBB (bb:bbs) l = if (label bb) == l
                      then bb
                      else lookupBB bbs l

lookupBBWithIfForCall :: Program -> Label -> BasicBlock
lookupBBWithIfForCall [] n = error $ "lookup bb that originates call to " ++ (show n) ++ " not found"
lookupBBWithIfForCall (bb:bbs) l = case terminator bb of
                            If _ lab -> if lab == l
                                        then bb
                                        else lookupBBWithIfForCall bbs l
                            _ -> lookupBBWithIfForCall bbs l

updateBB :: BasicBlock -> Program -> Program
updateBB _ [] = error $ "Could not find basic block to be updated"
updateBB newBB (bb:bbs) = if (label bb) == (label newBB)
                          then newBB:bbs
                          else bb:(updateBB newBB bbs )


printProgram :: Program -> String
printProgram p = foldr (++) "" (map printBasicBlock p)

ident :: String
ident = "    "

printBasicBlock :: BasicBlock -> String
printBasicBlock (BB l n cs t) = (printLabel l n) ++ (printCommands cs) ++ (printTerminator t)

printLabel :: Label -> Int -> String
printLabel l n = (yellow l) ++ " (" ++ (show n) ++ "):\n"

printCommands :: [Command] -> String
printCommands cs = foldr (++) "" (map printCommand cs)

printCommand :: Command -> String
printCommand (Call name1 name2) = ident ++ (blue (name1 ++ " = Call " ++ name2)) ++ "\n"
printCommand (Op name1 op name2) = ident ++ "resp = " ++ name1 ++ " " ++ (printOpCode op) ++ " " ++ name2 ++ "\n"
printCommand (AssignI name n) = ident ++ name ++ " = " ++ (show n) ++ "\n"
printCommand (AssignL name l) = ident ++ name ++ " = \"" ++ l ++ "\"\n"
printCommand (AssignV name1 name2) = ident ++ name1 ++ " = " ++ name2 ++ "\n"
printCommand (Wait) = ident ++ (blue "Wait") ++ "\n"
printCommand (NewClosure n1 i) = ident ++ n1 ++  " = " ++ (red "NewClosure ") ++ (show i) ++ "\n"
printCommand (SetClosureFn n1 l) = ident ++ "SetClosureFn " ++ n1 ++ " " ++ l ++ "\n"
printCommand (GetClosureFn n1 l) = ident ++ l ++ " = GetClosureFn " ++ n1 ++ "\n"
printCommand (SetClosureMissing n1 n2) = ident ++ "SetClosureMissing " ++ n1 ++ " " ++ n2 ++ "\n"
printCommand (SetClosureMissingI n1 i) = ident ++ "SetClosureMissingI " ++ n1 ++ " " ++ (show i) ++ "\n"
printCommand (GetClosureMissing n1 n2) = ident ++ n2 ++ " = GetClosureMissing " ++ n1 ++ "\n"
printCommand (SetClosureCount n1 n2) = ident ++ "SetClosureCount " ++ n1 ++ " " ++ n2 ++ "\n"
printCommand (SetClosureCountI n1 i) = ident ++ "SetClosureCountI " ++ n1 ++ " " ++ (show i) ++ "\n"
printCommand (GetClosureCount n1 n2) = ident ++ n2 ++ " = GetClosureCount " ++ n1 ++ "\n"
printCommand (SetClosureParam n1 n2 n3) = ident ++ (red "SetClosureParam ") ++ n1 ++ " " ++ n2 ++ " " ++ n3 ++  "\n"
printCommand (SetClosureParamIL n1 n2 n3) = ident ++ "SetClosureParamIL " ++ n1 ++ " " ++ (show n2) ++ " " ++ n3 ++  "\n"
printCommand (SetClosureParamI n1 n2 n3) = ident ++ "SetClosureParamIL " ++ n1 ++ " " ++ (show n2) ++ " " ++ n3 ++  "\n"
printCommand (GetClosureParam n1 n2 n3) = ident ++ n3 ++ " = " ++ (cyan "GetClosureParam ") ++ n1 ++ " " ++ (show n2) ++ "\n"

printTerminator :: Terminator -> String
printTerminator (Goto l) = ident ++ (green ("goto " ++ l)) ++ "\n"
printTerminator (Return name) = ident ++ (green ("return " ++ name)) ++ "\n"
printTerminator (If name l) = ident ++ (green ("if " ++ name ++ " goto " ++ l)) ++ "\n"
printTerminator Empty = ident ++ "empty\n"

printOpCode :: OpCode -> String
printOpCode And = "&&"
printOpCode Or = "||"
printOpCode Add = "+"
printOpCode Sub = "-"
printOpCode Mult = "*"
printOpCode Div = "/"
printOpCode Equal = "=="
printOpCode NotEqual = "!="
printOpCode Greater = ">"
printOpCode GreaterEqual = ">="
printOpCode Lesser = "<"
printOpCode LesserEqual = "<="
