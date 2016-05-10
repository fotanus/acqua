module UL1.Language where

type Name = String

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

data Term
  = Op Term OpCode Term
  | Num Int
  | Ident Name
  | App Term Term
  | Let Name Term Term
  | Letrec Name Term Term
  | If Term Term Term
  | Fn [Name] Term
  | Param Int
  deriving (Eq,Ord,Show,Read)
