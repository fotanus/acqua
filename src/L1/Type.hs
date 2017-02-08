module L1.Type where

import Logger
import L1.Language as L1

-- This table lists identifiers and some data about its type. It holds the pre-calculated type for this identifier
-- the base term which was used to calculate this identifier type, and if it is recursive or not. The base term is saved
-- because we might ocasionally try to get a more precise type if we have more information on this name, for instance in
-- a function application.
type IdentifierTypeTable = [(Name, (Type, Term, Bool))]

-- internal function to test if two types are the same or one of them is unknown
isTypeOrUnknown :: Type -> Type -> Bool
isTypeOrUnknown t1 t2 = case (t1,t2) of
                        ((FnT t11 t12),(FnT t21 t22)) -> foldr (&&) True (map (\par-> isTypeOrUnknown (fst par) (snd par)) (zip t11 t21)) && (isTypeOrUnknown t12 t22)
                        _                             -> t1 == t2 || t1 == UnknownT || t2 == UnknownT


-- inferType takes a name, a term and one table of names and try to infer its type based on the operations
-- that it encounters using this name. For instance, if the name is used with a plus operation, it is clear that
-- this name type is an integer.
inferType :: Name -> Term -> IdentifierTypeTable  -> Type
inferType _ (Num _ _) _ = UnknownT
inferType _ (Ident _ _) _ = UnknownT
inferType _ (List _ _) _ = UnknownT
inferType n (Op e1 _ e3 _) nameTypes =
    case (e1,e3) of
    ((Ident n1 _),_)  | n == n1 ->  IntT
    (_,(Ident n1 _))  | n == n1 ->  IntT
    _               -> if inferType n e1 nameTypes == UnknownT
                       then inferType n e3 nameTypes
                       else inferType n e1 nameTypes
inferType n (Fn names e1 _) nameTypes =
    if n `elem` names
    then UnknownT
    else inferType n e1 nameTypes
inferType n (App e1 e2 _) nameTypes =
    case e1 of
    (Ident n1 _)  | n == n1 ->  FnT [(typeCheck e2 nameTypes)] UnknownT
    _         -> if inferType n e1 nameTypes == UnknownT
                 then inferType n e2 nameTypes
                 else inferType n e1 nameTypes
inferType n (If e1 e2 e3 _) nameTypes =
    case e1 of 
    (Ident n1 _)  | n == n1 ->  IntT
    _           -> if inferType n e2 nameTypes == UnknownT
                   then inferType n e3 nameTypes
                   else inferType n e2 nameTypes
inferType n (Let _ e1 e2 _) nameTypes =
    if inferType n e1 nameTypes == UnknownT
    then inferType n e2 nameTypes
    else inferType n e1 nameTypes
inferType n (Letrec _ e1 e2 _) nameTypes =
    if inferType n e1 nameTypes == UnknownT
    then inferType n e2 nameTypes
    else inferType n e1 nameTypes
inferType n (Head e1 _) _ =
    case e1 of
    (Ident n1 _)  | n == n1 ->  ListT 
    _             ->  UnknownT
inferType n (Tail e1 _) _ =
    case e1 of 
    (Ident n1 _)  | n == n1 ->  ListT
    _             ->  UnknownT
inferType n (Last e1 _) _ =
    case e1 of 
    (Ident n1 _)  | n == n1 ->  ListT
    _             -> UnknownT
inferType n (Length e1 _) _ =
    case e1 of 
    (Ident n1 _)  | n == n1 ->  ListT
    _              -> UnknownT
inferType n (Concat e1 e2 _) _ =
    case (e1,e2) of 
    ((Ident n1 _),_)  | n == n1 ->  ListT
    (_,(Ident n1 _))  | n == n1 ->  ListT
    _               -> UnknownT
inferType n (Concat3 e1 e2 e3 _) _ =
    case (e1,e2,e3) of 
    ((Ident n1 _),_,_) | n == n1 -> ListT
    (_,(Ident n1 _),_) | n == n1 -> ListT
    (_,_,(Ident n1 _)) | n == n1 -> ListT
    _                 -> UnknownT
inferType n (Map e1 e2 _) nameTypes =
    case e1 of
    (Ident n1 _) | n == n1 -> FnT [(typeCheck e2 nameTypes)] UnknownT
    _           -> inferType n e2 nameTypes

inferType n (Slice e1 e2 e3 _) nameTypes =
    case (e1,e2,e3) of
    ((Ident n1 _),_,_) | n == n1 -> ListT
    (_,(Ident n1 _),_) | n == n1 -> IntT
    (_,_,(Ident n1 _)) | n == n1 -> IntT
    _                 -> if inferType n e2 nameTypes == UnknownT
                         then inferType n e3 nameTypes
                         else inferType n e2 nameTypes
inferType n (Filter e1 e2 _) nameTypes =
    case e1 of
    (Ident n1 _) | n == n1 -> FnT [(typeCheck e2 nameTypes)] UnknownT
    _                      -> inferType n e2 nameTypes


-- typeCheck takes a term and a table and returns the type of the term. It might infer
-- the free variable types in the body.
typeCheck :: Term -> IdentifierTypeTable -> Type
typeCheck (Num _ _) _ = IntT
typeCheck (List _ _) _ = ListT
typeCheck (Head e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then IntT
    else error "Head on something that is not a list"

typeCheck (Last e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then IntT
    else error "Tail on something that is not a list"

typeCheck (Tail e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then ListT
    else error "Tail on something that is not a list"

typeCheck (Length e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then IntT
    else error "Tail on something that is not a list"

typeCheck (Concat e1 e2 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) && (isTypeOrUnknown (typeCheck e2 nameTypes) ListT)
    then ListT
    else error "Concating something that is not a list"

typeCheck (Concat3 e1 e2 e3 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) &&
     (isTypeOrUnknown (typeCheck e2 nameTypes) ListT) &&
     (isTypeOrUnknown (typeCheck e3 nameTypes) ListT)
    then ListT
    else error "Concating something that is not a list"

typeCheck (Slice e1 e2 e3 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) && (isTypeOrUnknown (typeCheck e2 nameTypes) IntT)  && (isTypeOrUnknown (typeCheck e3 nameTypes) IntT)
    then ListT
    else error "Slice with wrong types"

typeCheck (Map e1 e2 _) nameTypes =
    if typeCheck e2 nameTypes == ListT
      then case e1 of
           Fn _ _ _ -> ListT
           Ident n _ -> case lookup n nameTypes of
                      Just _  -> ListT
                      Nothing -> error $ "Type error: Mapping undefined identifier"
           _       -> error $ "Type error: Can't map " ++ (show e1) ++ " to " ++ (show e2)
      else error $ "Type error: Can only map to a list"

typeCheck (Filter e1 e2 _) nameTypes =
    if isTypeOrUnknown (typeCheck e2 nameTypes) ListT
    then case e1 of
           Fn _ _ _ -> ListT
           Ident n _ -> case lookup n nameTypes of
                      Just _  -> ListT
                      Nothing -> error $ "Type error: Mapping undefined identifier"
           _       -> error $ "Type error: Can't filter " ++ (show e2) ++ " with function " ++ (show e1)
    else error $ "Type error: Can only filter a list " ++ (show e2)

typeCheck (Ident n _) nameTypes = case lookup n nameTypes of
                                     Just (t,_,_) -> t
                                     Nothing -> UnknownT

typeCheck (Fn names e1 _) nameTypes = FnT (map (\n-> inferType n e1 nameTypes) names) (typeCheck e1 nameTypes)

typeCheck (App e1 e2 _) nameTypes =
  let
    e2type = typeCheck e2 nameTypes
    (FnT t1 t2) =  case e1 of
        Ident n _ -> case lookup n nameTypes of
            Just (t,e,rec) -> if rec
                              then t
                              else case e of
                                   Fn names _ _ -> typeCheck e ((map (\nam-> (nam, (e2type, e2, rec))) names) ++ nameTypes)
                                   _        -> typeCheck e nameTypes
            _              -> error $ "Type error: Applying undefined identifier " ++ (show n) ++ " " ++ (show e2)
        Fn names _ _  -> typeCheck e1 ((map (\n-> (n,(e2type, e2, False))) names) ++ nameTypes)
        App _ _ _ -> typeCheck e1 nameTypes
        _       -> error $ "Type error: Can't apply " ++ (show e1) ++ "to " ++ (show e2)
  in
    if isTypeOrUnknown e2type (head t1)
    then if null (tail t1) then t2 else FnT (tail t1) t2
    else error $ "Type error: Argument " ++ (show (typeCheck e2 nameTypes)) ++ " does not match type."

typeCheck (Op e1 _ e2 _) nameTypes =
    if isTypeOrUnknown (typeCheck e1 nameTypes) IntT
    then if isTypeOrUnknown (typeCheck e2 nameTypes) IntT
         then IntT
         else error "right expression is not int on operation"
    else error "left expresison is not int on operation"

typeCheck (If e1 e2 e3 _) nameTypes =
    if isTypeOrUnknown (typeCheck e1 nameTypes) IntT
    then if isTypeOrUnknown (typeCheck e2 nameTypes) (typeCheck e3 nameTypes)
         then typeCheck e2 nameTypes
         else error "Two branches of if don't have the same type"
    else error "expression on if is not int"

typeCheck (Let n e1 e2 _) nameTypes =
  let
    nameTypes' = traceShowId $ (n,(typeCheck e1 nameTypes, e1,False)):nameTypes
  in
    typeCheck e2 nameTypes'

typeCheck (Letrec n e1 e2 _) nameTypes =
  let
    tempNameTypes = (n,(FnT [UnknownT] UnknownT,e1,True)):nameTypes
    nameTypes' = (n,(typeCheck e1 tempNameTypes,e1,True)):nameTypes
  in
    typeCheck e2 nameTypes'
