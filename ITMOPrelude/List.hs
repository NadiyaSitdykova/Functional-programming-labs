{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- ��� ���� ������?
--
-- ��� undefined ���������� � ��������� �����.
-- ��������� (*) �������� �����, � ������� ����� ������������� ������.

---------------------------------------------
-- �����������

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- ��������

-- ����� ������
length :: List a -> Nat
length Nil = Zero
length (Cons x xs) = Succ (length xs)

-- ������� ��� ������ �� O(length a)
(++) :: List a -> List a -> List a
Nil ++ l = l
(Cons x xs) ++ l = Cons x (xs ++ l)

-- ������ ��� ������� ��������
tail :: List a -> List a
tail Nil = Nil
tail (Cons x xs) = xs

-- ������ ��� ���������� ��������
init :: List a -> List a
init Nil = Nil
init (Cons x Nil) = Nil
init (Cons x xs) = Cons x (init xs)

-- ������ �������
head :: List a -> a
head (Cons x xs) = x

-- ��������� �������
last :: List a -> a
last (Cons x Nil) = x
last (Cons x xs) = last xs

-- n ������ ��������� ������
take :: Nat -> List a -> List a
take Zero l = Nil
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- ������ ��� n ������ ���������
drop :: Nat -> List a -> List a
drop Zero l = l
drop (Succ n) (Cons x xs) = drop n xs

-- �������� � ������ ������ �������� ��������������� p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons x xs) = if' (p x)
			(Cons x (filter p xs))
			(filter p xs)

-- ���������� ������. ������ "���������/��������" p
-- ������� "���������/�������� b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons x xs) = case (p x) of
			(Just b) -> Cons b (gfilter p xs)
			Nothing -> gfilter p xs

-- ���������� �� ������ � ��������� �� ������� ��������� ���������
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons x xs) = if' (p x) 
			(Cons x (takeWhile p xs))
			(Cons x Nil)

-- �� ���������� �� ������ � ��������� �� ������� ��������� ���������,
-- ����� ���� ����������� ��� ��������, ������� ������ ����������
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons x xs) = if' (p x) 
			(dropWhile p xs)
			(Cons x xs)

-- ������� ������ �� ��������� �� (takeWhile p xs, dropWhile p xs),
-- �� �����������
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons x xs) = if' (p x) 
			(Pair (Cons x (fst $ span p xs))(snd $ span p xs))
			(Pair Nil (Cons x xs))

-- ������� ������ �� ��������� �� (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- �� �����������
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p l = span (not . p) l

-- n-�� ������� ������ (������ � ����)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons x xs)  !! Zero = x
(Cons x xs) !! (Succ n) = xs !! n

-- ������ ����� �� ����
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs) = (reverse xs) ++ (Cons x Nil)

-- (*) ��� ��������� ������� ������
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = map (Cons x) (subsequences xs) ++ subsequences xs

-- (*) ��� ������������ ��������� ������� ������
permutations :: List a -> List (List a)
permutations = undefined

-- (*) ���� ������. ��� ������������ ��������� ������� ������
-- ������ ��������
permutations' :: List a -> List (List a)
permutations' = undefined

-- ��������� ������� ����������� ����� ���
repeat :: a -> List a
repeat l = Cons l (repeat l)

-- ����� ������
-- ��������� ����� ������ ����������:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons x xs) = foldl f (f z x) xs

-- ��� �� foldl, �� � ������ ����������� ��� ������������� ����������
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons x xs) = Cons (f z x) (scanl f (f z x) xs) 

-- ������ ������
-- ��������� ����� ������ ����������:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons x xs) = f x (foldr f z xs)

-- ����������
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons x xs) = Cons (f x y) ys where (Cons y ys) = scanr f z xs

-- ������ ����������� �� �������� �����
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- ��������� f � ������� �������� ������
map :: (a -> b) -> List a -> List b
map f (Cons x Nil) = Cons (f x) Nil
map f (Cons x xs) = Cons (f x) (map f xs)

-- ��������� ������ ������� � ������
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons x xs) = x ++ (concat xs)

-- ���������� (concat . map), �� �����������
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons x xs) = (f x) ++ (concatMap f xs)

-- �������� ��� ������ � ������ ��� ������ min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil b = Nil
zip a Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys)

-- ����������, �� ������� ��� ������ �������, � �� ������������� Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil b = Nil
zipWith f a Nil = Nil  
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)