module Compiler.Transformations.FillFreeVars where

import Data.List
import L1.Language

fillFreeVars :: Term -> Term
fillFreeVars t = _fillFreeVars [] t


-- [Name] is a black list of variable names that are considered free variables on the function body. We
-- only consider free variables the ones defined as function parameters, as oposed as defined by let or letrec.
-- let/letrec definitions are macros instead of free variables, having their code inserted on the compilation.
_fillFreeVars :: [Name] -> Term -> Term
_fillFreeVars bl (Fn ns t (typ,_)) = Fn ns (_fillFreeVars bl t) (typ, (nub (findFreeVars t (ns++bl))))
_fillFreeVars bl (If t1 t2 t3 anno) = If (_fillFreeVars bl t1) (_fillFreeVars bl t2) (_fillFreeVars bl t3) anno
_fillFreeVars bl (Op t1 op t2 anno) = Op (_fillFreeVars bl t1) op (_fillFreeVars bl t2) anno
_fillFreeVars bl (App t1 t2 anno) = App (_fillFreeVars bl t1) (_fillFreeVars bl t2) anno
_fillFreeVars bl (MultiApp t1 t2 anno) = MultiApp (_fillFreeVars bl t1) (map (_fillFreeVars bl) t2) anno
_fillFreeVars bl (Let n (Fn ns t1 fv) t2 anno) = Let n (_fillFreeVars (bl++[n]) (Fn ns t1 fv)) (_fillFreeVars (bl++[n]) t2) anno
_fillFreeVars bl (Let n (Num i anno2) t2 anno) = Let n (_fillFreeVars (bl++[n]) (Num i anno2)) (_fillFreeVars (bl++[n]) t2) anno
_fillFreeVars bl (Let n t1 t2 anno) = Let n (_fillFreeVars bl t1) (_fillFreeVars bl t2) anno
_fillFreeVars bl (Letrec n t1 t2 anno) = Letrec n (_fillFreeVars (bl++[n]) t1) (_fillFreeVars (bl++[n]) t2) anno
_fillFreeVars bl (Head t1 anno) = Head (_fillFreeVars bl t1) anno
_fillFreeVars bl (Tail t1 anno) = Tail (_fillFreeVars bl t1) anno
_fillFreeVars bl (Last t1 anno) = Last (_fillFreeVars bl t1) anno
_fillFreeVars bl (Length t1 anno) = Length (_fillFreeVars bl t1) anno
_fillFreeVars bl (Concat t1 t2 anno) = Concat (_fillFreeVars bl t1) (_fillFreeVars bl t2) anno
_fillFreeVars bl (Concat3 t1 t2 t3 anno) = Concat3 (_fillFreeVars bl t1) (_fillFreeVars bl t2) (_fillFreeVars bl t3) anno
_fillFreeVars bl (Map t1 t2 anno) = Map (_fillFreeVars bl t1) (_fillFreeVars bl t2) anno
_fillFreeVars bl (Slice t1 t2 t3 anno) = Slice (_fillFreeVars bl t1) (_fillFreeVars bl t2) (_fillFreeVars bl t3) anno
_fillFreeVars bl (Filter t1 t2 anno) = Filter (_fillFreeVars bl t1) (_fillFreeVars bl t2) anno
_fillFreeVars _ t = t


-- Given a term from L1, look for all variable names that occur inside it, except the ones in the black list.
-- The blacklist is there to avoid adding variables that are actually parameters for the inside functions.
findFreeVars :: Term -> [Name] -> [Name]
findFreeVars (Num _ _) _ = []
findFreeVars (Ident n _) bl = if n `elem` bl then [] else [n]
findFreeVars (If t1 t2 t3 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl) ++ (findFreeVars t3 bl)
findFreeVars (Op t1 _ t2 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (App t1 t2 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (MultiApp t1 t2 _) bl = (findFreeVars t1 bl) ++ (concat (map (\t-> findFreeVars t bl) t2))
findFreeVars (Let n t1 t2 _) bl = (findFreeVars t1 (bl++[n])) ++ (findFreeVars t2 (bl++[n]))
findFreeVars (Letrec n t1 t2 _) bl = (findFreeVars t1 (bl++[n])) ++ (findFreeVars t2 (bl++[n]))
findFreeVars (Fn ns t _) bl = (findFreeVars t (ns++bl))
findFreeVars (Head t1 _) bl = (findFreeVars t1 bl)
findFreeVars (Tail t1 _) bl = (findFreeVars t1 bl)
findFreeVars (Last t1 _) bl = (findFreeVars t1 bl)
findFreeVars (Length t1 _) bl = (findFreeVars t1 bl)
findFreeVars (Concat t1 t2 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (Concat3 t1 t2 t3 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl) ++ (findFreeVars t3 bl)
findFreeVars (Slice t1 t2 t3 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl) ++ (findFreeVars t3 bl)
findFreeVars (Map t1 t2 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (Filter t1 t2 _) bl = (findFreeVars t1 bl) ++ (findFreeVars t2 bl)
findFreeVars (List items _) bl = foldr (\e prev-> case e of ListIdent n -> if n `elem` bl then prev else prev++[n]; _ -> prev) [] items
