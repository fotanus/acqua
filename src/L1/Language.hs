module L1.Language where

type Name = String

-- There are three types: Integer, function and Unknown. Unknown is a type used when we don't have enough information on the
-- program to determine the type, and might be used as a placeholder while determining the type of a program.
data Type
  = IntT
  | FnT [Type] Type
  | ListT
  | UnknownT
  | QUT Name
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

type Annotations = (Type, [(Name,Type)])

defaultAnnotations :: Annotations
defaultAnnotations = (UnknownT, [])

setType :: Term -> Type -> Term
setType (Op t1 op t2 (_,fv)) typ = (Op t1 op t2 (typ,fv))
setType (Num n (_,fv)) typ = (Num n (typ,fv))
setType (Ident n (_,fv)) typ = (Ident n (typ,fv))
setType (App t1 t2 (_,fv)) typ = (App t1 t2 (typ,fv))
setType (MultiApp t1 lt (_,fv)) typ = (MultiApp t1 lt (typ,fv))
setType (Let n t1 t2 (_,fv)) typ = (Let n t1 t2 (typ,fv))
setType (Letrec n t1 t2 (_,fv)) typ = (Letrec n t1 t2 (typ,fv))
setType (If t1 t2 t3 (_,fv)) typ = (If t1 t2 t3 (typ,fv))
setType (Fn arg t1 (_,fv)) typ = (Fn arg t1 (typ,fv))
setType (List elms (_,fv)) typ = (List elms (typ,fv))
setType (Head t1 (_,fv)) typ = (Head t1 (typ,fv))
setType (Tail t1 (_,fv)) typ = (Tail t1 (typ,fv))
setType (Last t1 (_,fv)) typ = (Last t1 (typ,fv))
setType (Length t1 (_,fv)) typ = (Length t1 (typ,fv))
setType (Concat t1 t2 (_,fv)) typ = (Concat t1 t2 (typ,fv))
setType (Concat3 t1 t2 t3 (_,fv)) typ = (Concat3 t1 t2 t3 (typ,fv))
setType (Map t1 t2 (_,fv)) typ = (Map t1 t2 (typ,fv))
setType (Slice t1 t2 t3 (_,fv)) typ = (Slice t1 t2 t3 (typ,fv))
setType (Filter t1 t2 (_,fv)) typ = (Filter t1 t2 (typ,fv))

getType :: Term -> Type
getType (Op _ _ _ (typ,_)) = typ
getType (Num _ (typ,_)) = typ
getType (Ident _ (typ,_)) = typ
getType (App _ _ (typ,_)) = typ
getType (MultiApp _ _ (typ,_)) = typ
getType (Let _ _ _ (typ,_)) = typ
getType (Letrec _ _ _ (typ,_)) = typ
getType (If _ _ _ (typ,_)) = typ
getType (Fn _ _ (typ,_)) = typ
getType (List _ (typ,_)) = typ
getType (Head _ (typ,_)) = typ
getType (Tail _ (typ,_)) = typ
getType (Last _ (typ,_)) = typ
getType (Length _ (typ,_)) = typ
getType (Concat _ _ (typ,_)) = typ
getType (Concat3 _ _ _ (typ,_)) = typ
getType (Map _ _ (typ,_)) = typ
getType (Slice _ _ _ (typ,_)) = typ
getType (Filter _ _ (typ,_)) =  typ

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
