{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveLift #-}

module Functional.Lambda.Typed
	( TypedLambda(..)
	, TypedCombinator
	, TypedInput
	, free
	, input
	, lift
	, abstract
	, toCombinator
	, ($$$)
	, reType
	, Representable(..)
	) where

import GHC.TypeNats (Nat)
import Nat (Peano, Positive(..), type (<=)(..))
import Data.Void (absurd)
import ValidLiterals (Lift)

import qualified Functional.Lambda as L
import Functional.Reducible (($$))

newtype TypedLambda t v = TypedLambda { fromTyped :: L.Lambda v }
	deriving (Eq, Ord, Lift)

-- Used for inputs of functions with arity n, and contains free terms
-- whose DeBruijn index will be <=n after abstract has been called n times
type TypedInput (n :: Nat) t = TypedLambda t (Peano n)

type TypedCombinator t = forall v. TypedLambda t v

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

-- A free term whose DeBruijn index will be n after abstract has been called
-- n times
input :: Positive n => TypedLambda t n
input = free maxmem

-- Lift a term with inputs of index <=n to a type that can be abstracted m
-- times
lift :: n <= m => TypedLambda t n -> TypedLambda t m
lift = fmap generalise

{-
It is recommended to specify the type of the variables being abstracted as well
as the result of the abstraction to make sure the type checking has worked
correctly, as haskell may assign these different types if a small error is
made, such as mixing up two inputs.
Example:
```
isZero :: TypedCombinator (Natural -> Bool)
isZero = ...
predNat :: TypedCombinator (Natural -> Natural)
predNat = ...
isOne :: TypedCombinator (Natural -> Bool)
isOne = abstract $ input $$$ predNat $$$ isZero
```
Here, Haskell has inferred that `input` has the type
`TypedInput 1 ((Natural -> Natural) -> (Natural -> Bool) -> Bool)`
where it should have type `TypedInput 1 Natural` since it is the input to
a function of type `Natural -> Bool`. Giving it an explicit type will make
Haskell pick up on this error:
```
isOne = abstract $ (input :: TypedInput 1 Natural) $$$ predNat $$$ isZero
```
Also note that using ScopedTypeVariables for generic functions is recommended
to allow them to be type checked.
-}
abstract :: TypedLambda b (Maybe t) -> TypedLambda (a -> b) t
abstract (TypedLambda l) = TypedLambda $ L.abstract l

toCombinator :: TypedInput 0 t -> TypedCombinator t
toCombinator = fmap absurd

infixl 3 $$$

($$$) :: TypedLambda (a -> b) v -> TypedLambda a v -> TypedLambda b v
TypedLambda f $$$ TypedLambda x = TypedLambda $ f $$ x

{-
Obviously this is unsafe, and you should make the type of the input and output
explicit to avoid errors.
-}
reType :: TypedLambda a v -> TypedLambda b v
reType (TypedLambda l) = TypedLambda l

{-
Class of types that can be converted to their lambda calculus encoding.
It is expected that instances of Representable and Decode satisfy
`decodeLambda (fromTyped $ toLambda x :: Lambda Void) = Just x`
for all x. It is also expected that converting the `fromTyped $ toLambda x` to
some other functional type and then doing `decodeBT` will also yield `Just x`.
-}
class Representable a where
	toLambda :: a -> TypedCombinator a
