{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
--import ITMOPrelude.Primitive
--import ITMOPrelude.List

data Tree a = Nill | Node a (Tree a) (Tree a) deriving (Show,Read)

-- создание пустого дерева
newTree :: Tree a
newTree = Nill

-- добавления элемента в вершину дерева
addHead :: Tree a -> a -> Tree a
addHead Nill x = Node x Nill Nill
addHead (Node h l r) x = Node x l r

-- добавление элемента в качестве самого левого
addLeft :: Tree a -> a -> Tree a
addLeft Nill x = Node x Nill Nill
addLeft (Node h l r) x = addLeft l x
                                              
--добавление элемента в качестве самого правого
addRight :: Tree a -> x -> Tree a
addRigth Nill x = Node x Nill Nill
addRight (Node h l r) x = addRight r x

-- поворот влево (малый)
rotL :: Tree a -> Tree a
rotL Nill = Nill
rotL (Node h l Nill) = Node h l Nill
rotL (Node h l (Node r rl rr)) = Node r (Node h l rl) rr

-- поворот вправо (малый)
rotR :: Tree a -> Tree a
rotR Nill = Nill
rotR (Node h Nill r) = Node h Nill r
rotR (Node h (Node l ll lr) r) = Node l ll (Node h lr r)

-- аналог map
tmap :: (a -> b) -> Tree a -> Tree b
tmap f Nill = Nill
tmap f (Node h l r) = Node (f h) (tmap f l) (tmap f r)

-- аналог foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f b Nill = b
foldr f b (Node h l r) = foldr f (f h (foldr f b r)) l