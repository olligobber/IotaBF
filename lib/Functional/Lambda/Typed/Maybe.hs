{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Maybe
	( FMaybe
	, toFMaybe
	, fromFMaybe
	, nothing
	, just
	, maybe
	, isJust
	, isNothing
	, fromMaybe
	) where

import Prelude hiding (show, maybe, map)
import ValidLiterals (valid)

import Functional.Lambda.Typed
	( TypedLambda, TypedInput, TypedCombinator, Representable(..)
	, reType, abstract, input, toCombinator, liftInput, liftFree, ($$$)
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Decode (Decode(..))
import Functional.Lambda (Lambda(Lambda), LambdaTerm(LambdaFree))
import qualified Functional.Lambda as L
import Functional.BinaryTree (BinaryTree(..))
import Functional.Reducible (($$), Var(Var))
import Functional.Lambda.Typed.Render (LambdaShow, show, TypedRenderS)
import qualified Functional.Lambda.Typed.Function as LF
import Functional.Lambda.Typed.Functor (LambdaFunctor, map)
import Functional.Lambda.Typed.Semigroup (LambdaSemigroup, cat)

-- Functional equivalent of maybe type,
-- Nothing returns its first input, Just applies its second input to its value
type FMaybe a b = b -> (a -> b) -> b

toFMaybe :: TypedLambda (Maybe a) v -> TypedLambda (FMaybe a b) v
toFMaybe = reType

fromFMaybe :: TypedLambda (FMaybe a b) v -> TypedLambda (Maybe a) v
fromFMaybe = reType

instance LambdaEq a => LambdaEq (Maybe a) where
	eq = toCombinator $ abstract $ abstract $
		toFMaybe (input :: TypedInput 2 (Maybe a)) $$$
		(isNothing $$$ liftInput (input :: TypedInput 1 (Maybe a))) $$$
		abstract (
			toFMaybe (liftInput (input :: TypedInput 2 (Maybe a))) $$$
			toLambda False $$$
			abstract (
				eq $$$
				liftInput (input :: TypedInput 2 a) $$$
				liftInput (input :: TypedInput 1 a)
			)
		)

instance Representable a => Representable (Maybe a) where
	toLambda Nothing = nothing
	toLambda (Just x) = fromFMaybe justx where
		justx :: forall b c. TypedCombinator (b -> (a -> c) -> c)
		justx = toCombinator $ abstract $ abstract $
			liftInput (input :: TypedInput 1 (a -> c)) $$$
			toLambda x

instance Decode a => Decode (Maybe a) where
	decodeLambda lambda = case
		L.leftmostReduce $
			(Right <$> lambda) $$
			pure (Left $ Var "Nothing") $$
			pure (Left $ Var "Just")
		of
			Lambda (Leaf (LambdaFree (Left (Var "Nothing")))) -> Just Nothing
			Lambda (Leaf (LambdaFree (Left (Var "Just"))) :^: x) ->
				-- Remove any "Nothing" or "Just" that ended up in x
				case traverse (either (const Nothing) Just) (Lambda x) of
					Nothing -> Nothing
					Just lx -> Just <$> decodeLambda lx
			_ -> Nothing

instance LambdaShow a => LambdaShow (Maybe a) where
	show = abstract $
		liftInput (toFMaybe (input :: TypedInput 1 (Maybe a))) $$$
		liftFree ($$(valid "Nothing") :: TypedRenderS) $$$
		abstract (
			cat $$$ (
				cat $$$
				liftFree ($$(valid "Maybe[") :: TypedRenderS) $$$
				(liftFree show $$$ liftInput (input :: TypedInput 1 a))
			) $$$
			liftFree ($$(valid "]") :: TypedRenderS)
		)

instance LambdaFunctor Maybe where
	map :: forall a b. TypedCombinator ((a -> b) -> Maybe a -> Maybe b)
	map = toCombinator $ abstract $ abstract $
		toFMaybe (liftInput (input :: TypedInput 1 (Maybe a))) $$$
		nothing $$$
		abstract (
			just $$$
			(
				(input :: TypedInput 3 (a -> b)) $$$
				liftInput (input :: TypedInput 1 a)
			)
		)

instance LambdaSemigroup a => LambdaSemigroup (Maybe a) where
	cat = maybe $$$
		LF.id $$$ (
			LF.compose $$$
			map $$$
			cat
		)

nothing :: forall a. TypedCombinator (Maybe a)
nothing = fromFMaybe nothingF where
	nothingF :: forall b c. TypedCombinator (b -> c -> b)
	nothingF = toCombinator $ abstract $ abstract (input :: TypedInput 2 b)

just :: forall a. TypedCombinator (a -> Maybe a)
just = reType justF where
	justF :: forall b c. TypedCombinator (a -> b -> (a -> c) -> c)
	justF = toCombinator $ abstract $ abstract $ abstract $
		liftInput (input :: TypedInput 1 (a -> c)) $$$
		(input :: TypedInput 3 a)

maybe :: forall a b. TypedCombinator (b -> (a -> b) -> Maybe a -> b)
maybe = toCombinator $ abstract $ abstract $ abstract $
	toFMaybe (liftInput (input :: TypedInput 1 (Maybe a))) $$$
	(input :: TypedInput 3 b) $$$
	liftInput (input :: TypedInput 2 (a -> b))

isJust :: forall a. TypedCombinator (Maybe a -> Bool)
isJust = toCombinator $ abstract $
	toFMaybe (input :: TypedInput 1 (Maybe a)) $$$
	toLambda False $$$
	(LF.const $$$ toLambda True)

isNothing :: forall a. TypedCombinator (Maybe a -> Bool)
isNothing = toCombinator $ abstract $
	toFMaybe (input :: TypedInput 1 (Maybe a)) $$$
	toLambda True $$$
	(LF.const $$$ toLambda False)

fromMaybe :: forall a. TypedCombinator (a -> Maybe a -> a)
fromMaybe = toCombinator $ abstract $ abstract $
	toFMaybe (liftInput (input :: TypedInput 1 (Maybe a))) $$$
	(input :: TypedInput 2 a) $$$
	LF.id
