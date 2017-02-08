module L1.Language where

type Name = String

-- There are three types: Integer, function and Unknown. Unknown is a type used when we don't have enough information on the
-- program to determine the type, and might be used as a placeholder while determining the type of a program.
data Type
  = IntT
  | FnT [Type] Type
  | ListT
  | UnknownT
  deriving (Eq,Ord,Show,Read)

data OpCode
  = And
  | Or
  | Add
  | Sub
  | Mult
  | Div
  | Mod
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
  | RecList [ListItem]
  deriving (Eq,Ord,Show,Read)

type Annotations = (Type, [Name])

defaultAnnotations :: Annotations
defaultAnnotations = (UnknownT, [])

data Term
  = Op Term OpCode Term Annotations
  | Num Int Annotations
  | Ident Name Annotations
  | App Term Term Annotations
  | MultiApp Term [Term] Annotations
  | Let Name Term Term Annotations
  | Letrec Name Term Term Annotations
  | If Term Term Term Annotations
  | Fn [Name] Term Annotations
  | List [ListItem] Annotations
  | Head Term Annotations
  | Tail Term Annotations
  | Last Term Annotations
  | Length Term Annotations
  | Concat Term Term Annotations
  | Concat3 Term Term Term Annotations
  | Map Term Term Annotations
  | Slice Term Term Term Annotations
  | Filter Term Term Annotations
  deriving (Eq,Ord,Show,Read)
