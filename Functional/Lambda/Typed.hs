{-# LANGUAGE RankNTypes #-}

module Functional.Lambda.Typed
	( TypedLambda(..)
	, TypedCombinator
	, TypedInput1
	, TypedInput2
	, TypedInput3
	, TypedInput4
	, free
	, abstract
	, ($$$)
	, reType
	) where

import qualified Functional.Lambda as L
import Functional.Reducible (($$))

newtype TypedLambda t v = TypedLambda { fromTyped :: L.Lambda v }
	deriving (Eq, Ord)

type TypedCombinator t = forall v. TypedLambda t v

-- TypedInputn is for inputs of arity n functions.
type TypedInput1 t = forall v. TypedLambda t (Maybe v)
type TypedInput2 t = forall v. TypedLambda t (Maybe (Maybe v))
type TypedInput3 t = forall v. TypedLambda t (Maybe (Maybe (Maybe v)))
type TypedInput4 t = forall v. TypedLambda t (Maybe (Maybe (Maybe (Maybe v))))

instance Functor (TypedLambda t) where
	fmap f (TypedLambda l) = TypedLambda $ f <$> l

instance Foldable (TypedLambda t) where
	foldMap f (TypedLambda l) = foldMap f l

instance Traversable (TypedLambda t) where
	sequenceA (TypedLambda l) = TypedLambda <$> sequenceA l

instance Applicative (TypedLambda t) where
	pure = TypedLambda . pure
	TypedLambda f <*> TypedLambda x = TypedLambda $ f <*> x

instance Monad (TypedLambda t) where
	TypedLambda x >>= f = TypedLambda $ x >>= fromTyped . f

instance Show v => Show (TypedLambda t v) where
	showsPrec d (TypedLambda l) = showParen (d>10) $
		showString "TypedLambda " . showsPrec 11 l

free :: v -> TypedLambda t v
free = TypedLambda . L.free

{-
It is recommended to specify the type of the variables being abstracted as well
as the result of the abstraction to make sure the type checking has worked
correctly, as haskell may assign these different types if a small error is
made, such as mixing up two inputs.
Example:
```
isZero :: TypedCombinator (Nat -> Bool)
isZero = ...
predNat :: TypedCombinator (Nat -> Nat)
predNat = ...
isOne :: TypedLambda (Nat -> Bool) v
isOne = abstract $ free Nothing $$$ predNat $$$ isZero
```
Here, Haskell has inferred that `free Nothing' has the type
`TypedInput1 ((Nat -> Nat) -> (Nat -> Bool) -> Bool)`
where it should have type `TypedLambda Nat (Maybe v)` since it is the input to
a function of type (Nat -> Bool). Giving it an explicit type will make Haskell
pick up on this error:
`isOne = abstract $ (free Nothing :: TypedInput1 Nat) $$$ predNat $$$ isZero`.
Also note that using ScopedTypeVariables for generic functions is recommended
to allow them to be type checked.
-}
abstract :: TypedLambda b (Maybe v) -> TypedLambda (a -> b) v
abstract (TypedLambda l) = TypedLambda $ L.abstract l

infixl 3 $$$

($$$) :: TypedLambda (a -> b) v -> TypedLambda a v -> TypedLambda b v
TypedLambda f $$$ TypedLambda x = TypedLambda $ f $$ x

{-
Obviously this is unsafe, and you should make the type of the input and output
explicit to avoid errors.
-}
reType :: TypedLambda a v -> TypedLambda b v
reType (TypedLambda l) = TypedLambda l
