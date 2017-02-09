module L1.Type where

import Data.List
import qualified Text.Show.Pretty as P

import Logger
import L1.Language as L1

-- This table lists identifiers and some data about its type. It holds the pre-calculated type for this identifier
-- the base term which was used to calculate this identifier type, and if it is recursive or not. The base term is saved
-- because we might ocasionally try to get a more precise type if we have more information on this name, for instance in
-- a function application.
type IdentifierTypeTable = [(Name, (Type, Term, Bool))]

-- internal function to test if two types are the same or one of them is unknown
isTypeOrUnknown :: Type -> Type -> Bool
isTypeOrUnknown (FnT t11 t12) (FnT t21 t22) = foldr (&&) True
                                                    (map (\(a,b)-> isTypeOrUnknown a b) (zip t11 t21))
                                                     && (isTypeOrUnknown t12 t22)
isTypeOrUnknown t1 t2 = case (t1,t2) of
                        ((QUT _),_) -> True
                        (_,(QUT _)) -> True
                        (t1,t2) -> t1 == t2 || t1 == UnknownT || t2 == UnknownT

isTypeOrQUnknown :: Type -> Type -> Bool
isTypeOrQUnknown  (FnT t11 t12) (FnT t21 t22) = foldr (&&) True
                                                    (map (\(a,b)-> isTypeOrQUnknown a b) (zip t11 t21))
                                                     && (isTypeOrQUnknown t12 t22)
isTypeOrQUnknown t1 t2 = case (t1,t2) of
                        ((QUT _),_) -> True
                        (_,(QUT _)) -> True
                        _ -> False

defineUnknown :: Type -> Type -> Type
defineUnknown UnknownT t2 = t2

hasUnknown :: Type -> Bool
hasUnknown UnknownT = True
hasUnknown (FnT t1 t2) = hasUnknown t2 || foldr (||) False (map (\t-> hasUnknown t) t1)
hasUnknown _ = False

-- first the base type, then the type to be found, and finally the type to be used as substitution
subUnknown :: Type -> Type -> Type -> Type
subUnknown (FnT t' t'') t2 t3 = FnT (map (\tt-> (subUnknown tt t2 t3)) t') (subUnknown t'' t2 t3)
subUnknown t1 t2 t3 = if t1 == t2 then t3 else t1

-- returns a list of binds for QUTs resulting from the type being applied in the function.
matchType :: Type -> Type -> [(Type,Type)]
matchType (QUT x) t2 = [((QUT x), t2)]
matchType UnknownT _ = []
matchType (FnT t11 t12) (FnT t21 t22) = (concat (map (\(t',t'')-> matchType t' t'' ) (zip t11 t21))) ++ (matchType t12 t22)
matchType IntT UnknownT = [] -- this is a hack to allow running the simulator with entry x
matchType t1 t2 = if t1 == t2 then [] else error $ "type mismatch " ++ (show t1) ++ ", and " ++ (show t2)


-- from two types, which one is better?
betterQualifiedType :: Type -> Type -> Type
betterQualifiedType UnknownT t2 = t2
betterQualifiedType t1 UnknownT = t1
betterQualifiedType (QUT x) t2 = t2
betterQualifiedType t1 (QUT x) = t1
betterQualifiedType t1 _  = t1





-- inferType takes a name, a term and one table of names and try to infer its type based on the operations
-- that it encounters using this name. For instance, if the name is used with a plus operation, it is clear that
-- this name type is an integer.
inferType :: Name -> Term -> IdentifierTypeTable  -> Type
inferType _ (Num _ _) _ = UnknownT
inferType n (Ident n1 _) _ = if n == n1 then QUT n else UnknownT
inferType _ (List _ _) _ = UnknownT

-- this might be generalizable. It is used to fix the return type of a function with some look ahead.
inferType n (Op (App e1 e2 _) _ e3 _) nameTypes =
    case (e1,e2,e3) of
    ((Ident n1 _),_,_)  | n == n1 -> FnT [(typeCheck e2 nameTypes)] IntT
    (_,(Ident n1 _),_)  | n == n1 -> FnT [(typeCheck e2 nameTypes)] IntT
    (_,_,(Ident n1 _))  | n == n1 -> IntT
    _               -> if inferType n e1 nameTypes == UnknownT
                       then inferType n e3 nameTypes
                       else inferType n e1 nameTypes

inferType n (Op e1 _ e3 _) nameTypes =
    case (e1,e3) of
    ((Ident n1 _),_)  | n == n1 -> IntT
    (_,(Ident n1 _))  | n == n1 -> IntT
    _               -> if inferType n e1 nameTypes == UnknownT
                       then inferType n e3 nameTypes
                       else inferType n e1 nameTypes

inferType n (Fn names e1 _) nameTypes =
  let
    paramNameTypes = map (\n->(n,((inferType n e1 nameTypes), (Ident n defaultAnnotations), False))) names
    nameTypes' = (paramNameTypes++nameTypes)
  in
    if n `elem` names
    then UnknownT
    else inferType n e1 nameTypes'


inferType n (App e1 e2 _) nameTypes =
    case (e1,e2) of
    ((Ident n1 _),_)  | n == n1 ->  FnT [(typeCheck e2 nameTypes)] (QUT ("return_"++n))
    (_,(Ident n1 _))  | n == n1 ->  case (typeCheck e1 nameTypes) of
                                          FnT t1 _ -> head t1
                                          _        -> QUT n
    _         -> if inferType n e1 nameTypes == UnknownT
                 then inferType n e2 nameTypes
                 else inferType n e1 nameTypes

inferType n (If e1 e2 e3 _) nameTypes =
    case e1 of
    (Ident n1 _)  | n == n1 ->  IntT
    _           -> betterQualifiedType (inferType n e2 nameTypes) (inferType n e3 nameTypes)

inferType n (Let _ e1 e2 _) nameTypes =
    if inferType n e1 nameTypes == UnknownT
    then inferType n e2 nameTypes
    else inferType n e1 nameTypes

inferType n (Letrec n1 e1 e2 _) nameTypes =
  let
    tempNameTypes = (n1,(FnT [UnknownT] UnknownT,e1,True)):nameTypes
    nameTypes' = (n1,(typeCheck e1 tempNameTypes,e1,True)):nameTypes
  in
    if inferType n e1 nameTypes' == UnknownT
    then inferType n e2 nameTypes'
    else inferType n e1 nameTypes'

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
    else error "Type error: Head on something that is not a list"

typeCheck (Last e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then IntT
    else error "Type error: Tail on something that is not a list"

typeCheck (Tail e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then ListT
    else error "Type error: Tail on something that is not a list"

typeCheck (Length e1 _) nameTypes =
  if isTypeOrUnknown (typeCheck e1 nameTypes) ListT
    then IntT
    else error "Type error: Tail on something that is not a list"

typeCheck (Concat e1 e2 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) && (isTypeOrUnknown (typeCheck e2 nameTypes) ListT)
    then ListT
    else error "Type error: Concating something that is not a list"

typeCheck (Concat3 e1 e2 e3 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) &&
     (isTypeOrUnknown (typeCheck e2 nameTypes) ListT) &&
     (isTypeOrUnknown (typeCheck e3 nameTypes) ListT)
    then ListT
    else error "Type error: Concating something that is not a list"

typeCheck (Slice e1 e2 e3 _) nameTypes =
  if (isTypeOrUnknown (typeCheck e1 nameTypes) ListT) && (isTypeOrUnknown (typeCheck e2 nameTypes) IntT)  && (isTypeOrUnknown (typeCheck e3 nameTypes) IntT)
    then ListT
    else error "Type error: Slice with wrong types"

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

typeCheck (Fn names e1 _) nameTypes =
  let
    paramTypes = map (\n->(inferType n e1 nameTypes)) names
    paramNameTypes = map (\n->(n,((inferType n e1 nameTypes), (Ident n defaultAnnotations), False))) names
    resultTypes = typeCheck e1 (paramNameTypes++nameTypes)
  in
    FnT paramTypes resultTypes



typeCheck (App e1 e2 _) nameTypes =
  let
    e2type = typeCheck e2 nameTypes
    (e1type1,e1type2) = case typeCheck e1 nameTypes of
                        FnT t1 t2 -> (t1, t2)
                        UnknownT -> ([UnknownT], UnknownT)
                        QUT _ -> ([UnknownT], UnknownT)
                        _ -> error $ "what is this? " ++ (show (typeCheck e1 nameTypes)) ++ "\n" ++ (show e1)

    retType = if (length e1type1) == 1
              then e1type2
              else FnT (tail e1type1) e1type2
  in
    if isTypeOrUnknown e2type (head e1type1)
    then if e1type1 == [UnknownT]
         then subUnknown retType (head e1type1) e2type
         else let
                newVars = matchType (head e1type1) e2type
                sbst [] t1 = t1
                sbst ((org,rpl):xs) t1 = sbst xs (subUnknown t1 org rpl)
              in
                sbst newVars retType
    else error $ "Parameter do not match. Expected " ++ (show e1type1) ++ " but was " ++ (show e2type) ++ ".\n" ++ (show e1) ++ "\n" ++ (show e2)
    
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
    nameTypes' = (n,(typeCheck e1 nameTypes, e1,False)):nameTypes
  in
    typeCheck e2 nameTypes'

typeCheck (Letrec n e1 e2 _) nameTypes =
  let
    tempNameTypes = (n,(FnT [UnknownT] UnknownT,e1,True)):nameTypes
    nameTypes' = (n,(typeCheck e1 tempNameTypes,(Ident n defaultAnnotations),True)):nameTypes
  in
    typeCheck e2 nameTypes'



-- Fill the types on a term AST.
fillTypes :: Term -> IdentifierTypeTable -> Term
fillTypes (Num n a) _ = setType (Num n a) IntT
fillTypes (List e a) _ = setType (List e a) ListT
fillTypes (Ident n a) nameTypes =
  let t = case lookup n nameTypes of
          Just (t',_,_) -> t'
          Nothing -> UnknownT
  in setType (Ident n a) t

fillTypes (Head e1 a) nameTypes =
  let e1' = fillTypes e1 nameTypes
  in setType (Head e1' a) IntT -- (typeCheck e1 nameTypes) -- check list type

fillTypes (Last e1 a) nameTypes =
  let e1' = fillTypes e1 nameTypes
  in setType (Last e1' a) (typeCheck e1 nameTypes) -- check list type

fillTypes (Tail e1 a) nameTypes =
  let e1' = fillTypes e1 nameTypes
  in setType (Tail e1' a) ListT -- check list type

fillTypes (Length e1 a) nameTypes =
  let e1' = fillTypes e1 nameTypes
  in setType (Length e1' a) IntT -- check list type

fillTypes (Concat e1 e2 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
  in setType (Concat e1' e2' a) ListT -- check list type

fillTypes (Concat3 e1 e2 e3 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
     e3' = fillTypes e3 nameTypes
  in setType (Concat3 e1' e2' e3' a) ListT -- check list type


fillTypes (Slice e1 e2 e3 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
     e3' = fillTypes e3 nameTypes
  in setType (Slice e1' e2' e3' a) ListT -- check list type


fillTypes (Map e1 e2 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
  in setType (Map e1' e2' a) ListT -- checkListType

fillTypes (Filter e1 e2 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
  in setType (Filter e1' e2' a) ListT -- checkListType

fillTypes (Fn names e1 a) nameTypes =
  let
     paramTypes = map (\n-> (n, ((inferType n e1 nameTypes), e1, False))) names
     e1' = fillTypes e1 (paramTypes ++ nameTypes)
  in setType (Fn names e1' a) (typeCheck (Fn names e1 a) (paramTypes++nameTypes))

fillTypes (App e1 e2 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
  in setType (App e1' e2' a) (typeCheck (App e1 e2 a) nameTypes)

fillTypes (Op e1 opc e2 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
  in setType (Op e1' opc e2' a) IntT

fillTypes (If e1 e2 e3 a) nameTypes =
  let
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes
     e3' = fillTypes e3 nameTypes
  in setType (If e1' e2' e3' a) (typeCheck (If e1 e2 e3 a) nameTypes)

fillTypes (Let n e1 e2 a) nameTypes =
  let
     nameTypes' = (n,(typeCheck e1 nameTypes,e1,False)):nameTypes
     e1' = fillTypes e1 nameTypes
     e2' = fillTypes e2 nameTypes'
  in setType (Let n e1' e2' a) (typeCheck (Let n e1 e2 a) nameTypes')

fillTypes (Letrec n e1 e2 a) nameTypes =
  let
    tempNameTypes = (n,(FnT [UnknownT] UnknownT,e1,True)):nameTypes
    nameTypes' = (n,(typeCheck e1 tempNameTypes,e1,True)):nameTypes
    e1' = fillTypes e1 nameTypes'
    e2' = fillTypes e2 nameTypes'
  in setType (Letrec n e1' e2' a) (typeCheck (Letrec n e1 e2 a) nameTypes')
