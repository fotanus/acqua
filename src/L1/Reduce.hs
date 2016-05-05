module L1.Reduce where

import L1.Language as L1
import Logger
import Text.Show.Pretty

-- data OpCode
--   = And
--   | Or
--   | Add
--   | Sub
--   | Mult
--   | Equal
--   | NotEqual
--   | Greater
--   | GreaterEqual
--   | Lesser
--   | LesserEqual
--   deriving (Eq,Ord,Show,Read)
-- 
-- data Term
--   = Op Term OpCode Term
--   | Num Int
--   | Ident Name
--   | App Term Term
--   | Let Name Term Term
--   | Letrec Name Term Term
--   | If Term Term Term
--   | Fn Name Term
--   deriving (Eq,Ord,Show,Read)


run :: Term -> Term
run t = let t' = removeLet t
        in _run t' t'

_run :: Term -> Term -> Term
_run t t' = let t'' = trace (ppShow t) $ traceShow "---" $ reduce t
           in if t'' == t' then t'' else _run t'' t''


reduce :: Term -> Term
-- reductions on math operations
reduce (Param _) = error "Reduce do not implement param"
reduce (Op (Num n1) Add (Num n2)) = Num (n1 + n2)
reduce (Op (Num n1) Sub (Num n2)) = Num (n1 - n2)
reduce (Op (Num n1) Mult (Num n2)) = Num (n1 * n2)
-- reductions on if
reduce (If (Op (Num n1) Equal (Num n2)) t1 t2) = if n1 == n2 then t1 else t2
reduce (If (Op (Num n1) NotEqual (Num n2)) t1 t2) = if n1 /= n2 then t1 else t2
reduce (If (Op (Num n1) Greater (Num n2)) t1 t2) = if n1 > n2 then t1 else t2
reduce (If (Op (Num n1) Lesser (Num n2)) t1 t2) = if n1 < n2 then t1 else t2
reduce (If (Op (Num n1) GreaterEqual (Num n2)) t1 t2) = if n1 >= n2 then t1 else t2
reduce (If (Op (Num n1) LesserEqual (Num n2)) t1 t2) = if n1 <= n2 then t1 else t2
-- reductions on App
reduce (App (Fn x t1) t2) = substitute x t2 t1
-- errors
reduce (Let _ _ _) = error "should reduce only after remove let"
reduce (Letrec _ _ _) = error "should reduce only after remove let recs"
-- recursive calls
reduce (Op t1 opc t2) = Op (reduce t1) opc (reduce t2)
reduce (App t1 t2) = App (reduce t1) (reduce t2)
reduce (If t1 t2 t3) = If (reduce t1) (reduce t2) (reduce t3)
reduce (Fn n t1) = Fn n (reduce t1)
reduce (Num i) = Num i
reduce (Ident n) = Ident n

-- change all occurences of one name by a term, in a given term
substitute :: L1.Name -> Term -> Term -> Term
substitute n newBlock (Op t1 opc t2) = Op (substitute n newBlock t1) opc (substitute n newBlock t2)
substitute n newBlock (App t1 t2) = App (substitute n newBlock t1) (substitute n newBlock t2)
substitute n newBlock (If t1 t2 t3) = If (substitute n newBlock t1) (substitute n newBlock t2) (substitute n newBlock t3)
substitute _ _ (Num i) = Num i
substitute n newBlock (Ident name) = if name == n then newBlock else Ident name
substitute n newBlock (Fn name t) = if name /= n then Fn name (substitute n newBlock t) else Fn name t
substitute _ _ t2 = error $ "Can't substitute on " ++ (show t2)


-- Removes the let and letrec from a L1 program, transforming it on applications.
removeLet :: Term -> Term
--terms that change
removeLet (Param _) = error "removeLet do not implement param"
removeLet (Let n t1 t2) = App (Fn n t2) t1
removeLet (Letrec n1 (Fn n2 t1) t2) =  App (Fn n1 t2) (App (Fn n1 fnBody) fnBody)
                                       where fnBody = (Fn n2 t1)
removeLet (Letrec _ _ _) = error "Letrec first term is not a function"

-- call recursivelly for other terms
removeLet (Op t1 opc t2) = Op (removeLet t1) opc (removeLet t2)
removeLet (App t1 t2) = App (removeLet t1) (removeLet t2)
removeLet (If t1 t2 t3) = If (removeLet t1) (removeLet t2) (removeLet t3)
removeLet (Fn n t1) = Fn n (removeLet t1)
removeLet (Num i) = Num i
removeLet (Ident n) = Ident n
