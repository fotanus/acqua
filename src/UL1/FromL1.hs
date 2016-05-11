module UL1.FromL1 where

import L1.Language as L1
import UL1.Language as UL1

-- For a given L1 Term, convert this term to UL1. That is, transform
-- the free variables in function parameters.
fromL1 :: L1.Term -> UL1.Term
fromL1 (L1.Num n) = UL1.Num n
fromL1 (L1.Ident n) = UL1.Ident n
fromL1 (L1.If t1 t2 t3) = UL1.If (fromL1 t1) (fromL1 t2) (fromL1 t3)
fromL1 (L1.Op t1 op t2) = UL1.Op (fromL1 t1) (opFromL1 op) (fromL1 t2)
fromL1 (L1.App t1 t2) = UL1.App (fromL1 t1) (fromL1 t2)
fromL1 (L1.Let n t1 t2) = UL1.Let n (fromL1 t1) (fromL1 t2)
fromL1 (L1.Letrec n t1 t2) = UL1.Letrec n (fromL1 t1) (fromL1 t2)
fromL1 (L1.Fn _ t) = UL1.Fn (freeVariables t) (fromL1 t)

-- Given a term from L1, look for all variable names that occur inside it
freeVariables :: L1.Term -> [L1.Name]
freeVariables (L1.Num _) = []
freeVariables (L1.Ident n) = [n]
freeVariables (L1.If t1 t2 t3) = (freeVariables t1) ++ (freeVariables t2) ++ (freeVariables t3)
freeVariables (L1.Op t1 _ t2) = (freeVariables t1) ++ (freeVariables t2)
freeVariables (L1.App t1 t2) = (freeVariables t1) ++ (freeVariables t2)
freeVariables (L1.Let _ t1 t2) = (freeVariables t1) ++ (freeVariables t2)
freeVariables (L1.Letrec _ t1 t2) = (freeVariables t1) ++ (freeVariables t2)
freeVariables (L1.Fn _ t) = (freeVariables t)


-- Simple function that transforms L1 operators to UL1 operators.
-- Essentially the same, only to complain the typecheck
opFromL1 :: L1.OpCode -> UL1.OpCode
opFromL1 L1.And = UL1.And
opFromL1 L1.Or = UL1.Or
opFromL1 L1.Add = UL1.Add
opFromL1 L1.Sub = UL1.Sub
opFromL1 L1.Mult = UL1.Mult
opFromL1 L1.Equal = UL1.Equal
opFromL1 L1.NotEqual = UL1.NotEqual
opFromL1 L1.Greater = UL1.Greater
opFromL1 L1.GreaterEqual = UL1.GreaterEqual
opFromL1 L1.Lesser = UL1.Lesser
opFromL1 L1.LesserEqual = UL1.LesserEqual
