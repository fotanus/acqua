module AcquaIR.Language where

type Label = String
type Name = String
type EnvId = String

type Program = [BasicBlock]

data BasicBlock
  = BB Label Int [Command] Terminator
  deriving (Eq,Ord,Show,Read)

data Command
  = EnvAddI EnvId Name Int
  | EnvAddL EnvId Name Label
  | EnvNew EnvId Int
  | Call Name Name EnvId
  | Op Name OpCode Name
  | AssignI Name Int
  | AssignL Name Label
  | AssignV Name Name
  | Wait
  deriving (Eq,Ord,Show,Read)

data Terminator
  = Goto Label
  | Return Name
  | If Name Label
  deriving (Eq,Ord,Show,Read)

data OpCode
  = Add
  | Sub
  deriving (Eq,Ord,Show,Read)

