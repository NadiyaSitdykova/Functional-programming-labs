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

------------------------------------------------------------
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
sa n t = undefined

-- ������������ ���������� ��������
no = undefined

-- �������� � ������ �������� ���������� �����
wh = undefined

-- (*) (�� �����������) �������� "������" ������������� ��������.
-- ���������� �� �������� �������������� ���, ��� �� ����� ������
-- ����� � ������ ����� ����������, ����� ��� ��������.
wa = undefined

-- ���������: c������� ������ ������ �������������� ���������� �� �����������,
-- ������ ����� ������������ ���� ���������� (� ��������� �� �����-���������)
-- ��� ��� ������������� ������ � ��������������� Term � ���� � �������.

-- ������������ ���� ���� �������� (� ������� �������� ��
-- �����������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
--    , ("wa", wa) -- ����� ����������������, ��
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