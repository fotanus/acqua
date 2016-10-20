module L1.Language where

type Name = String

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

data ListItem
  = ListNum Int
  | ListIdent Name
  deriving (Eq,Ord,Show,Read)

data Term
  = Op Term OpCode Term
  | Num Int
  | Ident Name
  | App Term Term
  | Let Name Term Term
  | Letrec Name Term Term
  | If Term Term Term
  | Fn [Name] Term [Name]
  | List [ListItem]
  | Head Term
  | Tail Term
  | Last Term
  | Length Term
  | Concat Term Term
  | Map Term Term
  | Zip Term Term Term
  | Filter Term Term
  deriving (Eq,Ord,Show,Read)
