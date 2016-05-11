module L1.Type where

import L1.Language as L1

-- There are three types: Integer, function and Unknown. Unknown is a type used when we don't have enough information on the
-- program to determine the type, and might be used as a placeholder while determining the type of a program.
data Type
  = IntT
  | FnT Type Type
  | UnknownT 
  deriving (Eq,Ord,Show,Read)

-- This table lists identifiers and some data about its type. It holds the pre-calculated type for this identifier
-- the base term which was used to calculate this identifier type, and if it is recursive or not. The base term is saved
-- because we might ocasionally try to get a more precise type if we have more information on this name, for instance in
-- a function application.
type IdentifierTypeTable = [(Name, (Type, Term, Bool))]

-- internal function to test if two types are the same or one of them is unknown
isTypeOrUnknown :: Type -> Type -> Bool
isTypeOrUnknown t1 t2 = case (t1,t2) of
                        ((FnT t11 t12),(FnT t21 t22)) -> (isTypeOrUnknown t11 t21) && (isTypeOrUnknown t12 t22)
                        _                             -> t1 == t2 || t1 == UnknownT || t2 == UnknownT


-- inferType takes a name, a term and one table of names and try to infer its type based on the operations
-- that it encounters using this name. For instance, if the name is used with a plus operation, it is clear that
-- this name type is an integer.
inferType :: Name -> Term -> IdentifierTypeTable  -> Type
inferType _ (Num _) _ = UnknownT
inferType _ (Ident _) _ = UnknownT
inferType n (Op e1 _ e3) nameTypes =
    if e1 == (Ident n) || e3 == (Ident n)
    then IntT
    else if inferType n e1 nameTypes == UnknownT
         then inferType n e3 nameTypes
         else inferType n e1 nameTypes
inferType n (Fn name e1) nameTypes =
    if name == n
    then UnknownT
    else inferType n e1 nameTypes
inferType n (App e1 e2) nameTypes =
    if e1 == (Ident n)
    then (FnT (typeCheck e2 nameTypes) UnknownT)
    else if inferType n e1 nameTypes == UnknownT
         then inferType n e2 nameTypes
         else inferType n e1 nameTypes
inferType n (If e1 e2 e3) nameTypes =
    if e1 == (Ident n)
    then IntT
    else if inferType n e2 nameTypes == UnknownT
         then inferType n e3 nameTypes
         else inferType n e2 nameTypes
inferType n (Let _ e1 e2) nameTypes =
    if inferType n e1 nameTypes == UnknownT
    then inferType n e2 nameTypes
    else inferType n e1 nameTypes
inferType n (Letrec _ e1 e2) nameTypes =
    if inferType n e1 nameTypes == UnknownT
    then inferType n e2 nameTypes
    else inferType n e1 nameTypes


-- typeCheck takes a term and a table and returns the type of the term. It might infer
-- the free variable types in the body.
typeCheck :: Term -> IdentifierTypeTable -> Type
typeCheck (Num _) _ = IntT
typeCheck (Ident n) nameTypes = case lookup n nameTypes of
                                     Just (t,_,_) -> t
                                     Nothing -> UnknownT

typeCheck (Fn name e1) nameTypes = FnT (inferType name e1 nameTypes) (typeCheck e1 nameTypes)

typeCheck (App e1 e2) nameTypes =
  let
    e2type = typeCheck e2 nameTypes
    (FnT t1 t2) = case e1 of
        Ident n -> case lookup n nameTypes of
            Just (t,e,rec) -> if rec
                              then t
                              else case e of
                                   Fn var _ -> typeCheck e ((var, (e2type, e2, rec)):nameTypes)
                                   _        -> typeCheck e nameTypes
            _              -> error "Applying undefined identifier"
        Fn n _  -> typeCheck e1 ((n,(e2type, e2, False)):nameTypes)
        App _ _ -> typeCheck e1 nameTypes
        _       -> error $ "Can't apply " ++ (show e1) ++ "to " ++ (show e2)
  in
    if isTypeOrUnknown e2type t1
    then t2
    else error $ "Argument " ++ (show e2) ++ " type " ++ (show (typeCheck e2 nameTypes)) ++ " does not match type " ++ (show t1)


typeCheck (Op e1 _ e2) nameTypes =
    if isTypeOrUnknown (typeCheck e1 nameTypes) IntT
    then if isTypeOrUnknown (typeCheck e2 nameTypes) IntT
         then IntT
         else error "right expression is not int on operation"
    else error "left expresison is not int on operation"

typeCheck (If e1 e2 e3) nameTypes =
    if isTypeOrUnknown (typeCheck e1 nameTypes) IntT
    then if isTypeOrUnknown (typeCheck e2 nameTypes) (typeCheck e3 nameTypes)
         then typeCheck e2 nameTypes
         else error "Two branches of if don't have the same type"
    else error "expression on if is not int"

typeCheck (Let n e1 e2) nameTypes =
  let
    nameTypes' = (n,(typeCheck e1 nameTypes, e1,False)):nameTypes
  in
    typeCheck e2 nameTypes'

typeCheck (Letrec n e1 e2) nameTypes =
  let
    tempNameTypes = (n,(FnT UnknownT UnknownT,e1,True)):nameTypes
    nameTypes' = (n,(typeCheck e1 tempNameTypes,e1,True)):nameTypes
  in
    typeCheck e2 nameTypes'
