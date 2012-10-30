{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем нас
instance Monad m => MonadFish m where
	returnFish = return
	f >=> g = \a -> f a >>= g

instance Monad m => MonadJoin m where
	returnJoin = return
	join m = m >>= id

instance Monad m => Functor m where
	fmap f m = m >>= return . f