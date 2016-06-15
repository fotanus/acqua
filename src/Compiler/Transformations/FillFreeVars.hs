module Compiler.Transformations.FillFreeVars where

import Data.List
import L1.Language

fillFreeVars :: Term -> Term
fillFreeVars (Fn n t _) = Fn n (fillFreeVars t) (nub (findFreeVars t [n]))
fillFreeVars (If t1 t2 t3) = If (fillFreeVars t1) (fillFreeVars t2) (fillFreeVars t3)
fillFreeVars (Op t1 op t2) = Op (fillFreeVars t1) op (fillFreeVars t2)
fillFreeVars (App t1 t2) = App (fillFreeVars t1) (fillFreeVars t2)
fillFreeVars (Let n t1 t2) = Let n (fillFreeVars t1) (fillFreeVars t2)
fillFreeVars (Letrec n t1 t2) = Letrec n (fillFreeVars t1) (fillFreeVars t2)
fillFreeVars t = t


-- Given a term from L1, look for all variable names that occur inside it, except the ones in the black list.
-- The blacklist is there to avoid adding variables that are actually parameters for the inside functions.
findFreeVars :: Term -> [Name] -> [Name]
findFreeVars (Num _) _ = []
findFreeVars (Ident n) bl = if n `elem` bl then [] else [n]
findFreeVars (If t1 t2 t3) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl) ++ (findFreeVars t3 bl)
findFreeVars (Op t1 _ t2) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (App t1 t2) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (Let _ t1 t2) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (Letrec _ t1 t2) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (Fn n t _) bl = (findFreeVars t (n:bl))
