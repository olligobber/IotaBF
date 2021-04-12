{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Natural
	( FNatural
	, toFNatural
	, fromFNatural
	, succ
	, isZero
	, pred
	, add
	, mult
	, pow
	, subtract
	, divmod
	, div
	, mod
	) where

import Prelude hiding (succ, maybe, pred, subtract, div, mod, concat)
import Numeric.Natural (Natural)
import Data.List (genericReplicate)
import ValidLiterals (valid)

import Functional.Lambda.Typed
	( TypedLambda, TypedCombinator, Representable(..), TypedInput
	, reType, toCombinator, abstract, liftInput, input, fromTyped, liftFree
	, ($$$)
	)
import Functional.Lambda.Typed.Bool (toFBool, magicIf, magicElseIf, magicElse)
import Functional.Lambda.Typed.Function (compose, fix)
import Functional.Lambda.Typed.Maybe (just, nothing, maybe)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Decode (Decode(..))
import Functional.Reducible (($$))
import Functional.Lambda.Typed.Tuple (toFTuple2, mkTuple2, get1of2, get2of2)
import Functional.Lambda.Typed.Render
	(LambdaShow(..), RenderS, TypedRenderS, concat)
import Functional.Iota.Free (IFree)

-- Functional equivalent of natural numbers, composes its first input n times
type FNatural a = (a -> a) -> a -> a

toFNatural :: TypedLambda Natural v -> TypedLambda (FNatural a) v
toFNatural = reType

fromFNatural :: TypedLambda (FNatural a) v -> TypedLambda Natural v
fromFNatural = reType

instance LambdaEq Natural where
	-- Subtract and see if you get zero
	eq = toCombinator $ abstract $ abstract $
		maybe $$$
		toLambda False $$$
		isZero $$$
		(
			subtract $$$
			(input :: TypedInput 2 Natural) $$$
			liftInput (input :: TypedInput 1 Natural)
		)

instance Representable Natural where
	toLambda n = fromFNatural num where
		num :: forall a. TypedCombinator (FNatural a)
		num = toCombinator $ abstract $ abstract $
			foldl
				(\r _ -> (input :: TypedInput 2 (a -> a)) $$$ r)
				(liftInput (input :: TypedInput 1 a))
				(genericReplicate n ())

instance Decode Natural where
	decodeLambda lambda = case decodeLambda (fromTyped isZero $$ lambda) of
		Just True -> Just 0
		Just False -> case decodeLambda (fromTyped pred $$ lambda) of
			Just (Just n) -> Just (n+1)
			Just Nothing -> Nothing
			Nothing -> Nothing
		Nothing -> Nothing

instance LambdaShow Natural where
	-- If the number is less than 10, render its digit, otherwise use divmod
	-- to remove the last digit and recurse on the leading digits
	show = fix $$$ showF where
		showF :: TypedLambda ((Natural -> RenderS) -> Natural -> RenderS) IFree
		showF = abstract $ abstract $
			magicIf $$$
				(isZero $$$ liftInput (input :: TypedInput 1 Natural)) $$$
				liftFree ($$(valid "0") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 1 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "1") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 2 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "2") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 3 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "3") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 4 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "4") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 5 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "5") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 6 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "6") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 7 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "7") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 8 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "8") :: TypedRenderS) $$$
			magicElseIf $$$
				(eq $$$
					toLambda 9 $$$
					liftInput (input :: TypedInput 1 Natural)
				) $$$
				liftFree ($$(valid "9") :: TypedRenderS) $$$
			magicElse $$$
			(
				concat $$$
				(liftInput (input :: TypedInput 2 (Natural -> RenderS)) $$$
					(div $$$
						liftInput (input :: TypedInput 1 Natural) $$$
						toLambda 10
					)
				) $$$
				(liftInput (input :: TypedInput 2 (Natural -> RenderS)) $$$
					(mod $$$
						liftInput (input :: TypedInput 1 Natural) $$$
						toLambda 10
					)
				)
			)

succ :: TypedCombinator (Natural -> Natural)
succ = reType succF where
	succF :: forall a. TypedCombinator (Natural -> FNatural a)
	succF = toCombinator $ abstract $ abstract $ abstract $
		liftInput (input :: TypedInput 2 (a -> a)) $$$
		(
			(toFNatural input :: TypedInput 3 (FNatural a)) $$$
			liftInput (input :: TypedInput 2 (a -> a)) $$$
			liftInput (input :: TypedInput 1 a)
		)

isZero :: TypedCombinator (Natural -> Bool)
isZero = toCombinator $ abstract $
	toFNatural (input :: TypedInput 1 Natural) $$$
	abstract (toLambda False) $$$
	toLambda True

pred :: TypedCombinator (Natural -> Maybe Natural)
pred = toCombinator $ abstract $
	toFNatural (input :: TypedInput 1 Natural) $$$
	abstract (
		maybe $$$
		toLambda (Just 0) $$$
		(compose $$$ just $$$ succ) $$$
		liftInput (input :: TypedInput 1 (Maybe Natural))
	) $$$
	nothing

add :: TypedCombinator (Natural -> Natural -> Natural)
add = reType addF where
	addF :: forall a. TypedCombinator (FNatural a -> FNatural a -> FNatural a)
	addF = toCombinator $ abstract $ abstract $ abstract $ abstract $
		(input :: TypedInput 4 (FNatural a)) $$$
		liftInput (input :: TypedInput 2 (a -> a)) $$$
		(
			liftInput (input :: TypedInput 3 (FNatural a)) $$$
			liftInput (input :: TypedInput 2 (a -> a)) $$$
			liftInput (input :: TypedInput 1 a)
		)

mult :: TypedCombinator (Natural -> Natural -> Natural)
mult = reType multF where
	multF :: forall a. TypedCombinator (FNatural a -> FNatural a -> FNatural a)
	multF = toCombinator $ abstract $ abstract $ abstract $
		(input :: TypedInput 3 (FNatural a)) $$$
		(
			liftInput (input :: TypedInput 2 (FNatural a)) $$$
			liftInput (input :: TypedInput 1 (a -> a))
		)

pow :: TypedCombinator (Natural -> Natural -> Natural)
pow = toCombinator $ abstract $ abstract $ fromFNatural $
	toFNatural (liftInput (input :: TypedInput 1 Natural)) $$$
	toFNatural (input :: TypedInput 2 Natural)

subtract :: TypedCombinator (Natural -> Natural -> Maybe Natural)
subtract = toCombinator $ abstract $ abstract $
	toFNatural (liftInput (input :: TypedInput 1 Natural)) $$$
	abstract (
		maybe $$$
		nothing $$$
		pred $$$
		liftInput (input :: TypedInput 1 (Maybe Natural))
	) $$$
	(just $$$ (input :: TypedInput 2 Natural))

divmod :: TypedCombinator (Natural -> Natural -> (Natural, Natural))
divmod = toCombinator $ abstract $ abstract $
	toFNatural (input :: TypedInput 2 Natural) $$$
	abstract (
		toFTuple2 (liftInput (input :: TypedInput 1 (Natural, Natural))) $$$
		abstract (abstract $
			toFBool (
				eq $$$
				(succ $$$ liftInput (input :: TypedInput 1 Natural)) $$$
				liftInput (input :: TypedInput 4 Natural)
			) $$$
			(
				mkTuple2 $$$
				(succ $$$ liftInput (input :: TypedInput 2 Natural)) $$$
				toLambda 0
			) $$$
			(
				mkTuple2 $$$
				liftInput (input :: TypedInput 2 Natural) $$$
				(succ $$$ liftInput (input :: TypedInput 1 Natural))
			)
		)
	) $$$
	toLambda (0,0)

div :: TypedCombinator (Natural -> Natural -> Natural)
div = toCombinator $ abstract $ abstract $
	get1of2 $$$
	(
		divmod $$$
		(input :: TypedInput 2 Natural) $$$
		liftInput (input :: TypedInput 1 Natural)
	)

mod :: TypedCombinator (Natural -> Natural -> Natural)
mod = toCombinator $ abstract $ abstract $
	get2of2 $$$
	(
		divmod $$$
		(input :: TypedInput 2 Natural) $$$
		liftInput (input :: TypedInput 1 Natural)
	)

-- TODO lt, gt, lte, gte
