{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive
                                
class Monoid m where
	mempty :: m      
	mappend :: m -> m -> m

data SumNat = Sum Nat
data SumInt = Sum Int
data SumRat = sum Rat

instance Monoid SumNat where
	mempty = natZero
 	(Sum a) `mappend` (Sum b) = Sum $ a +. b
                                                        
instance Monoid SumInt where
	mempty = intZero
	(Sum a) `mappend` (Sum b) = Sum $ a .+. b

instance Monoid SumRat where
	mempty = Rat intZero natOne
	(Sum a) `mappend` (Sum b) = Sum $ a %+ b

data MulNat = Mul Nat
data MulInt = Mul Int
data MulRat = Mul Rat

instance Monoid MulNat where
	mempty = Mul natOne
	(Mul a) `mappend` (Mul b) = Mul $ a *. b 

instance Monoid MulInt where
	mempty = Mul intOne
	(Mul a) `mappend` (Mul b) = Mul $ a .*. b 

instance Monoid MulRat where
	mempty = Mul $ Rat intOne natOne
	(RMult a) `mappend` (RMult b) = Mul $ a %* b

class Monoid m => Group a where
       ginv :: a -> a

instance Group IntSum where
        ginv (Sum a) = Sum $ intNeg x
 
instance Group SumRat where
        ginv (Sum a) = Sum $ ratNeg x

instance Group MulRat where
        ginv (Mul a) = Mul $ ratInv a