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
  | GetClosureParam Name Name
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
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Lesser
  | LesserEqual
  deriving (Eq,Ord,Show,Read)

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
printOpCode Equal = "=="
printOpCode NotEqual = "!="
printOpCode Greater = ">"
printOpCode GreaterEqual = ">="
printOpCode Lesser = "<"
printOpCode LesserEqual = "<="
