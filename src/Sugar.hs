
module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar x = makeIndexedVar (getVar x) 

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar x = Var (ivName x ++ (if ivCount x == 0 then "" else "_" ++ show (ivCount x))) 

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.


-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

desugarExp :: ComplexExp -> Exp  
desugarExp (CX x) = X (desugarVar x) 
desugarExp (Nat n) = foldr (const (App succExp)) zeroExp [1..n]
desugarExp (CLam x e) = Lam (desugarVar x) (desugarExp e)
desugarExp (CApp e1 e2) = App (desugarExp e1) (desugarExp e2)
desugarExp (Let x e1 e2) = App (Lam (desugarVar x) (desugarExp e2)) (desugarExp e1)
desugarExp (LetRec x e1 e2) = App (Lam (desugarVar x) (desugarExp e2)) (App fixExp (Lam (desugarVar x) (desugarExp e1)))
desugarExp (List es) = foldr (\e acc -> App (App consExp (desugarExp e)) acc) (X (makeIndexedVar "Nil")) es





-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 

sugarExp :: Exp -> ComplexExp
sugarExp (X x) = CX (sugarVar x)
sugarExp (Lam x e) = CLam (sugarVar x) (sugarExp e)
sugarExp (App e1 e2) = CApp (sugarExp e1) (sugarExp e2)
sugarExp (App (App (X (IndexedVar ":" _)) e1) e2) = List (sugarExp e1 : listExp e2)
    where listExp (App (App (X (IndexedVar ":" _)) e1) e2) = sugarExp e1 : listExp e2
          listExp (X (IndexedVar "Nil" _)) = []
          listExp e = error $ "sugarExp: not a list: " ++ show e
sugarExp (App (X (IndexedVar "fix" _)) (Lam x e)) = LetRec (sugarVar x) (sugarExp e) (CX (sugarVar x))
sugarExp (App (Lam x e) e2) = Let (sugarVar x) (sugarExp e2) (sugarExp e)
sugarExp (App e1 e2) = CApp (sugarExp e1) (sugarExp e2)
sugarExp e = error $ "sugarExp: not a complex expression: " ++ show e

