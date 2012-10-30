{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас
instance MonadJoin m => MonadFish m where
	returnFish = returnJoin
	f >=> g = \a -> join (fmap g (f a))

instance MonadJoin m => Monad m where
	return = returnJoin
	m >>= f = join (fmap f m)

