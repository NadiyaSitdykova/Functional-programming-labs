{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Tree

class Category f where
        id :: f a a
        (.) :: f b c -> f a b -> f a c



class Functor f where
        fmap :: (a -> b) -> f a -> f b


instance Functor List where
        fmap f = map f

instance Functor Tree where
        fmap f = tmap f  



class Monad m where
	return :: a -> m a
  	(>>=) :: m a -> (a -> m b) -> m b
  	(>>) :: m a -> m b -> m b
	a >> b = a >>= (\_ -> b)


instance Monad List where
        return a = Cons a Nil
        a >>= f = concat $ map f a
