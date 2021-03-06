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
  = Call Name Name Bool
  | CallL Name Name Name
  | Wait
  | Op Name Name OpCode Name
  | AssignI Name Int
  | AssignL Name Label
  | AssignV Name Name
  -- lists
  | NewList Name Int
  | NewListN Name Name
  | ListSet Name Int Int
  | ListGet Name Name Name
  | ListSetN Name Int Name
  | ListSetNN Name Name Name
  | Head Name Name
  | Last Name Name
  | Tail Name Name
  | Concat Name Name Name
  | Concat3 Name Name Name Name
  | Length Name Name
  | Map Name Name Name
  | SMap Name Name Name Name
  | Slice Name Name Name Name
  | Filter Name Name Name
  -- call record
  | NewCallRecord Name Int
  | SetCallRecordFn Name Label
  | GetCallRecordFn Name Label
  | SetCallRecordMissing Name Name
  | SetCallRecordMissingI Name Int
  | GetCallRecordMissing Name Name
  | SetCallRecordCount Name Name
  | SetCallRecordCountI Name Int
  | GetCallRecordCount Name Name
  | SetCallRecordParam Name Name Name
  | SetCallRecordParamI Name Int Name
  | SetCallRecordParamIL Name Int Name
  | GetCallRecordParam Name Int Name
  | InnerCopy Name Name
  | OuterCopy Name Name
  | Delete Name
  -- pus
  | GetNPU Name
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
  | Mod
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
printLabel l _ = (yellow l) ++ ":\n"

printCommands :: [Command] -> String
printCommands cs = foldr (++) "" (map printCommand cs)

printCommand :: Command -> String
printCommand (Call name1 name2 _) = ident ++ (blue (name1 ++ " = Call " ++ name2)) ++ "\n"
printCommand (CallL name1 idx name2) = ident ++ (blue (name1 ++ "[" ++ idx ++ "] = Call " ++ name2)) ++ "\n"
printCommand (Op res name1 op name2) = ident ++ res ++ " = " ++ name1 ++ " " ++ (printOpCode op) ++ " " ++ name2 ++ "\n"
printCommand (AssignI name n) = ident ++ name ++ " = " ++ (show n) ++ "\n"
printCommand (AssignL name l) = ident ++ name ++ " = \"" ++ l ++ "\"\n"
printCommand (AssignV name1 name2) = ident ++ name1 ++ " = " ++ name2 ++ "\n"
printCommand (Wait) = ident ++ (blue "Wait") ++ "\n"
printCommand (NewCallRecord n1 i) = ident ++ n1 ++  " = " ++ (red "NewCallRecord ") ++ (show i) ++ "\n"
printCommand (SetCallRecordFn n1 l) = ident ++ "SetCallRecordFn " ++ n1 ++ " " ++ l ++ "\n"
printCommand (GetCallRecordFn n1 l) = ident ++ l ++ " = GetCallRecordFn " ++ n1 ++ "\n"
printCommand (SetCallRecordMissing n1 n2) = ident ++ "SetCallRecordMissing " ++ n1 ++ " " ++ n2 ++ "\n"
printCommand (SetCallRecordMissingI n1 i) = ident ++ "SetCallRecordMissingI " ++ n1 ++ " " ++ (show i) ++ "\n"
printCommand (GetCallRecordMissing n1 n2) = ident ++ n2 ++ " = GetCallRecordMissing " ++ n1 ++ "\n"
printCommand (SetCallRecordCount n1 n2) = ident ++ "SetCallRecordCount " ++ n1 ++ " " ++ n2 ++ "\n"
printCommand (SetCallRecordCountI n1 i) = ident ++ "SetCallRecordCountI " ++ n1 ++ " " ++ (show i) ++ "\n"
printCommand (GetCallRecordCount n1 n2) = ident ++ n2 ++ " = GetCallRecordCount " ++ n1 ++ "\n"
printCommand (SetCallRecordParam n1 n2 n3) = ident ++ (red "SetCallRecordParam ") ++ n1 ++ " " ++ n2 ++ " " ++ n3 ++  "\n"
printCommand (SetCallRecordParamIL n1 n2 n3) = ident ++ "SetCallRecordParamIL " ++ n1 ++ " " ++ (show n2) ++ " " ++ n3 ++  "\n"
printCommand (SetCallRecordParamI n1 n2 n3) = ident ++ "SetCallRecordParamIL " ++ n1 ++ " " ++ (show n2) ++ " " ++ n3 ++  "\n"
printCommand (GetCallRecordParam n1 n2 n3) = ident ++ n3 ++ " = " ++ (cyan "GetCallRecordParam ") ++ n1 ++ " " ++ (show n2) ++ "\n"
printCommand (Delete n1) = ident ++ "Delete "  ++ n1 ++ "\n"
printCommand (OuterCopy n1 n2) = ident ++ n1 ++  " = " ++ "OuterCopy " ++ n2 ++ "\n"
printCommand (InnerCopy n1 n2) = ident ++ n1 ++  " = " ++ "InnerCopy " ++ n2 ++ "\n"
printCommand (NewList n1 i) = ident ++ n1 ++  " = " ++ "NewList " ++ (show i) ++ "\n"
printCommand (NewListN n1 n2) = ident ++ n1 ++  " = " ++ "NewList " ++ n2 ++ "\n"
printCommand (ListSet n1 i1 i2) = ident ++ "ListSet " ++ n1 ++ " " ++ (show i1) ++ " " ++ (show i2) ++ "\n"
printCommand (ListSetN n1 i1 n2) = ident ++ "ListSetN " ++ n1 ++ " " ++ (show i1) ++ " " ++ n2 ++ "\n"
printCommand (ListSetNN n1 n2 n3) = ident ++ "ListSetN " ++ n1 ++ " " ++ n2 ++ " " ++ n3 ++ "\n"
printCommand (ListGet n1 n2 n3) = ident ++ n1 ++ " = ListGet " ++ n2 ++ " " ++ n3 ++ "\n"
printCommand (Head n1 n2) = ident ++ n1 ++ " = " ++ "head " ++ n2 ++ "\n"
printCommand (Tail n1 n2) = ident ++ n1 ++ " = " ++ "tail " ++ n2 ++ "\n"
printCommand (Last n1 n2) = ident ++ n1 ++ " = " ++ "last " ++ n2 ++ "\n"
printCommand (Concat res name1 name2) = ident ++ res ++ " = Concat " ++ name1 ++ " " ++ name2 ++ "\n"
printCommand (Concat3 res name1 name2 name3) = ident ++ res ++ " = Concat " ++ name1 ++ " " ++ name2 ++ " " ++ name3 ++ "\n"
printCommand (Map res name1 name2) = ident ++ res ++ " = Map " ++ name1 ++ " " ++ name2 ++ "\n"
printCommand (SMap res name1 name2 name3) = ident ++ res ++ " = SMap " ++ name1 ++ " " ++ name2 ++ " " ++ name3 ++ "\n"
printCommand (Slice res name1 name2 name3) = ident ++ res ++ " = Slice " ++ name1 ++ " " ++ name2 ++ " " ++ name3 ++ "\n"
printCommand (Filter res name1 name2) = ident ++ res ++ " = Filter " ++ name1 ++ " " ++ name2 ++ "\n"
printCommand (Length res name1) = ident ++ res ++ " = Length " ++ name1 ++ "\n"

printCommand (GetNPU name1) = ident ++ name1 ++ " = GetNPU\n"

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
printOpCode Mod = "mod"
printOpCode GreaterEqual = ">="
printOpCode Lesser = "<"
printOpCode LesserEqual = "<="
