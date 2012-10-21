{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- � ������ ������� ��������� ����������� ������������� ���
-- ���������������� ������
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- �����-�� �������. ��������, ��� � ���� ������� ����
-- ������������ ������� Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- ����������� ��������� ��� ���������������� ������
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

----------------------------------------------------------
-- ������ �� �� ���� ����������
                                                                                                                                               
-- ���� ������ ������ ������������ ����������� �������������, ��
-- � ��� ����� ������� ��� ������
-- (����� ������, ��� �� ����� ������� ����� ������ ��������,
-- ���� �������)

free (Var v)    = [ v ]
free (Lam v t)  = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term
subst t@(Var v)   var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t')  var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

betaRed :: Term -> Variable -> Term -> Term
betaRed t@(Var v) var what = if v == var then what else t                          
betaRed t@(Lam v b) var what   | v == var                 = t
                               | not (elem v (free what))   = Lam v (betaRed b var what)
                               | otherwise                = Lam v' (betaRed (subst b v (Var v')) var what)
								where v' = newname (free b ++ free what) var  									                                                        
betaRed (App t t')  var what = App (betaRed t var what) (betaRed t' var what)
--- ...

------------------------------------------------------------
-- �� ����������� ����, ��� ��������� ����������� ���������
-- ��������� ������������ (��� ��� ��������� ������������
-- ����� ����� �������������� � �������� ������� 
-- ��������� (n); ���� �� n ����� ������������� �� ������,
-- �� ������� ������� error, ������ ��� �������):

wh, no, wa, sa :: Integer -> Term -> Term

-- �������� ������������� ��������
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if fst next then sa (n - 1) (snd next)
		     else snd next
		     	where next = sa' t


sa' :: Term -> (Bool, Term)
sa' (Lam v t) = (fst next, Lam v (snd next))
			where next = sa' t
sa' (App (Lam v t) t') = if fst next then (True, App (Lam v (snd next)) t') 
				     else (True, betaRed t v t')
				     	where next = sa' t
sa' (App t t')	= let next = sa' t
		      next' = sa' t'
		      in if fst next then (True, App (snd next) t')
				     else (fst next', App t (snd next'))	
sa' term = (False, term)

-- ������������ ���������� ��������
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if fst next then no (n - 1) (snd next)
		     else snd next
		     	where next = no' t

no' :: Term -> (Bool, Term)
no' (Lam v t) = (fst next, Lam v (snd next))
			where next = no' t
no' (App (Lam v t) t') = (True, betaRed t v t')
no' (App t t')	= let next = no' t
		      next' = no' t'
		      in if fst next then (True, App (snd next) t')
				     else (fst next', App t (snd next'))	
no' term = (False, term)


-- �������� � ������ �������� ���������� �����
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if fst next then wh (n - 1) (snd next)
		     else snd next
		     	where next = wh' t

wh' :: Term -> (Bool, Term)
wh' (App (Lam v t) t') = (True, betaRed t v t')
wh' (App t t')	= let next = wh' t
		      next' = wh' t'
		      in if fst next then (True, App (snd next) t')
				     else (fst next', App t (snd next'))	
wh' term = (False, term)

-- (*) (�� �����������) �������� "������" ������������� ��������.
-- ���������� �� �������� �������������� ���, ��� �� ����� ������
-- ����� � ������ ����� ����������, ����� ��� ��������.

wa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wa n t = if fst next then wa (n - 1) (snd next)
		     else snd next
		     	where next = wa' t


wa' :: Term -> (Bool, Term)
wa' (App (Lam v t) t') = (True, betaRed t v t')
wa' (App t t')	= let next = sa' t
		      next' = sa' t'
		      in if fst next then (True, App (snd next) t')
				     else (fst next', App t (snd next'))	
wa' term = (False, term)


-- ���������: c������� ������ ������ �������������� ���������� �� �����������,
-- ������ ����� ������������ ���� ���������� (� ��������� �� �����-���������)
-- ��� ��� ������������� ������ � ��������������� Term � ���� � �������.

-- ������������ ���� ���� �������� (� ������� �������� ��
-- �����������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
    , ("wa", wa) -- ����� ����������������, ��
    , ("sa", sa) ]

------------------------------------------------------------
-- ����������� ���, ���� �������� ���������
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- ���� ����� ��������� �����
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- ������� ������������� ���������, ���� ��� ��� �������
--
-- ������� ���������� ��������, ��� ��������� � ����� �����
-- ��������� ���������� ��������, �� ��������� Haskell ��
-- ������ �� ��������� ����������������� ����������.
--
-- ����� ��� �������� ����������� � ������� ���� � �������
-- seq � ���������� ����� (���� ��������� ��� ��� ������ ��
-- �����������, �� �����-�� ����).