{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Functional.Lambda.Typed.Tuple where

import Prelude hiding (curry, uncurry)

import Functional.Lambda.Typed
	( TypedCombinator, TypedLambda, TypedInput, Representable(..)
	, input, abstract, toCombinator, reType, lift, ($$$)
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Lambda.Typed.Tuple.Template (generateTuples)
import Functional.Decode (Decode(..))
import qualified Functional.Lambda as L
import Functional.Reducible (Var(..), ($$))
import Functional.BinaryTree (BinaryTree(Leaf))

type FUnit x = x -> x

toFUnit :: TypedLambda () v -> TypedLambda (FUnit x) v
toFUnit = reType

fromFUnit :: TypedLambda (FUnit x) v -> TypedLambda () v
fromFUnit = reType

instance LambdaEq () where
	eq = toCombinator $ abstract $ abstract $ toLambda True

instance Representable () where
	toLambda () = fromFUnit unit where
		unit :: forall x. TypedCombinator (FUnit x)
		unit = toCombinator $ abstract (input :: TypedInput 1 x)

instance Decode () where
	decodeLambda lambda = case
		L.leftmostReduce $
			(Right <$> lambda) $$
			L.free (Left $ Var "()")
		of
			L.Lambda (Leaf (L.Free (Left (Var "()")))) -> Just ()
			_ -> Nothing

-- Generate the above but for tuples of length 2 to 8
generateTuples 8

-- Currying
curry :: forall a b c. TypedCombinator (((a, b) -> c) -> a -> b -> c)
curry = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 ((a,b) -> c)) $$$
	(
		mkTuple2 $$$
		lift (input :: TypedInput 2 a) $$$
		lift (input :: TypedInput 1 b)
	)
uncurry :: forall a b c. TypedCombinator ((a -> b -> c) -> (a, b) -> c)
uncurry = toCombinator $ abstract $ abstract $
	toFTuple2 (lift (input :: TypedInput 1 (a,b))) $$$
	(input :: TypedInput 2 (a -> b -> c))
