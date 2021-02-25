{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Bool
	( FBool
	, toFBool
	, fromFBool
	, and
	, or
	, not
	, bool
	, magicIf
	, magicElseIf
	, magicElse
	) where

import Prelude hiding (and, or, not, id)

import Functional.Decode (Decode(..))
import Functional.Lambda.Typed
	( TypedLambda, TypedCombinator, TypedInput, Representable(..)
	, reType, toCombinator, input, lift, abstract, ($$$)
	)
import Functional.Lambda.Typed.Function (compose, id)
import Functional.Reducible (($$), Var(Var))
import Functional.BinaryTree (BinaryTree(Leaf))
import Functional.Lambda (Lambda(Lambda), LambdaTerm(Free))
import qualified Functional.Lambda as L

-- Functional equivalent of a Bool
type FBool a = a -> a -> a

toFBool :: TypedLambda Bool v -> TypedLambda (FBool a) v
toFBool = reType

fromFBool :: TypedLambda (FBool a) v -> TypedLambda Bool v
fromFBool = reType

instance Representable Bool where
	toLambda True = fromFBool true where
		true :: forall a b. TypedCombinator (a -> b -> a)
		true = toCombinator $ abstract $ abstract (input :: TypedInput 2 a)
	toLambda False = fromFBool false where
		false :: forall a b. TypedCombinator (a -> b -> b)
		false =
			toCombinator $ abstract $ abstract $ lift (input :: TypedInput 1 b)

instance Decode Bool where
	decodeLambda lambda = case
		L.leftmostReduce $
			(Right <$> lambda) $$
			L.free (Left $ Var "True") $$
			L.free (Left $ Var "False")
		of
			Lambda (Leaf (Free (Left (Var "True")))) -> Just True
			Lambda (Leaf (Free (Left (Var "False")))) -> Just False
			_ -> Nothing

and :: TypedCombinator (Bool -> Bool -> Bool)
and = toCombinator $ abstract $ abstract $
	toFBool (input :: TypedInput 2 Bool) $$$
	lift (input :: TypedInput 1 Bool) $$$
	(input :: TypedInput 2 Bool)

or :: TypedCombinator (Bool -> Bool -> Bool)
or = toCombinator $ abstract $ abstract $
	toFBool (input :: TypedInput 2 Bool) $$$
	(input :: TypedInput 2 Bool) $$$
	lift (input :: TypedInput 1 Bool)

not :: TypedCombinator (Bool -> Bool)
not = toCombinator $ abstract $ fromFBool notF where
	notF :: forall a. TypedInput 1 (FBool a)
	notF = abstract $ abstract $
		(input :: TypedInput 3 (FBool a)) $$$
		lift (input :: TypedInput 1 a) $$$
		lift (input :: TypedInput 2 a)

bool :: forall a. TypedCombinator (a -> a -> Bool -> a)
bool = toCombinator $ abstract $ abstract $ abstract $
	lift (toFBool (input :: TypedInput 1 Bool)) $$$
	lift (input :: TypedInput 2 a) $$$
	(input :: TypedInput 3 a)

-- Magic if, see
-- https://gist.github.com/olligobber/a2b48af361aa20d1751846924bb268c2
magicIf :: forall a t. TypedCombinator (Bool -> a -> ((a -> a) -> t) -> t)
magicIf = toCombinator $ abstract $ abstract $ abstract $
	lift (input :: TypedInput 1 ((a -> a) -> t)) $$$
	(
		toFBool (input :: TypedInput 3 Bool) $$$
		lift (input :: TypedInput 2 a)
	)

magicElseIf ::
	forall a t. TypedCombinator ((a -> a) -> Bool -> a -> ((a -> a) -> t) -> t)
magicElseIf = toCombinator $ abstract $ abstract $ abstract $ abstract $
	lift (input :: TypedInput 1 ((a -> a) -> t)) $$$
	(
		compose $$$
		(input :: TypedInput 4 (a -> a)) $$$
		(
			toFBool (lift (input :: TypedInput 3 Bool)) $$$
			lift (input :: TypedInput 2 a)
		)
	)

magicElse :: forall a. TypedCombinator ((a -> a) -> a -> a)
magicElse = id
