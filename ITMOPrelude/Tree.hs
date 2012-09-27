{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error)
--import ITMOPrelude.Primitive
--import ITMOPrelude.List

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Read)

-- создание пустого дерева
newTree :: Tree a
newTree = Nil

-- добавление элемента в вершину дерева
addHead :: Tree a -> a -> Tree a
addHead Nil x = Node x Nil Nil
addHead (Node h l r) x = Node x l r

-- добавление элемента в качестве самого левого
addLeft :: Tree a -> a -> Tree a
addLeft Nil x = Node x Nil Nil
addLeft (Node h l r) x = addLeft l x
                                              
-- добавление элемента в качестве самого правого
addRight :: Tree a -> x -> Tree a
addRigth Nil x = Node x Nil Nil
addRight (Node h l r) x = addRight r x

-- поворот влево (малый)
rotL :: Tree a -> Tree a
rotL Nil = Nil
rotL (Node h l Nil) = Node h l Nil
rotL (Node h l (Node r rl rr)) = Node r (Node h l rl) rr

-- поворот вправо (малый)
rotR :: Tree a -> Tree a
rotR Nil = Nil
rotR (Node h Nil r) = Node h Nil r
rotR (Node h (Node l ll lr) r) = Node l ll (Node h lr r)

-- map
map :: (a -> b) -> Tree a -> Tree b
map f Nil = Nil
map f (Node h l r) = Node (f h) (map f l) (map f r)

-- foldr
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f b Nil = b
foldr f b (Node h l r) = foldr f (f h (foldr f b r)) l