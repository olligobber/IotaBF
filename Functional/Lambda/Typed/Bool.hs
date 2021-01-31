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
	, eq
	) where

import Prelude hiding (and, or, not)

import Functional.Decode (Decode(..))
import Functional.Lambda.Typed
	( TypedLambda, TypedCombinator, TypedInput, Representable(..)
	, reType, toCombinator, input, lift, abstract, ($$$)
	)
import Functional.Reducible (($$), Var(Var))
import Functional.BinaryTree (BinaryTree(Leaf))
import Functional.Lambda (Lambda(Lambda), LambdaTerm(Free))
import qualified Functional.BinaryTree as BT
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
	decodeBT tree = case
		BT.leftmostReduce $
			(Right <$> tree) $$
			Leaf (Left $ Var True) $$
			Leaf (Left $ Var False)
		of
			Leaf (Left (Var b)) -> Just b
			_ -> Nothing
	decodeLambda lambda = case
		L.leftmostReduce $
			(Right <$> lambda) $$
			L.free (Left $ Var True) $$
			L.free (Left $ Var False)
		of
			Lambda (Leaf (Free (Left (Var b)))) -> Just b
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

-- AKA (==)
eq :: TypedCombinator (Bool -> Bool -> Bool)
eq = toCombinator $ abstract $ abstract $
	toFBool (input :: TypedInput 2 Bool) $$$
	lift (input :: TypedInput 1 Bool) $$$
	(
		toFBool (lift (input :: TypedInput 1 Bool)) $$$
		(input :: TypedInput 2 Bool) $$$
		toLambda True
	)
