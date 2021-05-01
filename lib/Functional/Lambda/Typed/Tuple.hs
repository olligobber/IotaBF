{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Functional.Lambda.Typed.Tuple where

import Prelude hiding (curry, uncurry, const, id)
import ValidLiterals (valid)

import Functional.Lambda.Typed
	( TypedCombinator, TypedLambda, TypedInput, Representable(..)
	, input, abstract, toCombinator, reType, liftInput, ($$$)
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Lambda.Typed.Tuple.Template (generateTuples)
import Functional.Decode (Decode(..))
import qualified Functional.Lambda as L
import Functional.Reducible (Var(..), ($$))
import Functional.BinaryTree (BinaryTree(Leaf))
import Functional.Lambda.Typed.Render (LambdaShow, TypedRenderS)
import Functional.Lambda.Typed.Function (const, id)
import Functional.Lambda.Typed.Semigroup (LambdaSemigroup, cat)
import qualified Functional.Lambda.Typed.Render as TR

-- Functional equivalent of unit type: the identity
type FUnit x = x -> x

toFUnit :: TypedLambda () v -> TypedLambda (FUnit x) v
toFUnit = reType

fromFUnit :: TypedLambda (FUnit x) v -> TypedLambda () v
fromFUnit = reType

instance LambdaEq () where
	eq = toCombinator $ abstract $ abstract $ toLambda True

instance Representable () where
	toLambda () = fromFUnit id

instance Decode () where
	decodeLambda lambda = case
		L.leftmostReduce $
			(Right <$> lambda) $$
			L.free (Left $ Var "()")
		of
			L.Lambda (Leaf (L.Free (Left (Var "()")))) -> Just ()
			_ -> Nothing

instance LambdaShow () where
	show = const $$$ ($$(valid "Tuple[]") :: TypedRenderS)

instance LambdaSemigroup () where
	cat = const $$$ id

-- Generate the above but for tuples of length 2 to 8
generateTuples 8

-- Currying
curry :: forall a b c. TypedCombinator (((a, b) -> c) -> a -> b -> c)
curry = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 ((a,b) -> c)) $$$
	(
		mkTuple2 $$$
		liftInput (input :: TypedInput 2 a) $$$
		liftInput (input :: TypedInput 1 b)
	)

uncurry :: forall a b c. TypedCombinator ((a -> b -> c) -> (a, b) -> c)
uncurry = toCombinator $ abstract $ abstract $
	toFTuple2 (liftInput (input :: TypedInput 1 (a,b))) $$$
	(input :: TypedInput 2 (a -> b -> c))
