{-# LANGUAGE FlexibleContexts #-}

module Functional.Decode
	( Decode(..)
	, decodeBT
	) where

import Functional.Reducible (Reducible, Var)
import Functional.BinaryTree (BinaryTree, fromBinaryTree)
import Functional.Lambda (Lambda, LambdaTerm, free)

-- Class of types that can be converted from their lambda calculus encoding
class Decode t where
	decodeLambda ::
		Reducible (BinaryTree (LambdaTerm (Either (Var String) x))) x =>
		Lambda x -> Maybe t

-- Decode an arbitrary binary tree by turning it into lambda free variables
decodeBT ::
	(Decode t, Reducible (BinaryTree (LambdaTerm (Either (Var String) x))) x) =>
	BinaryTree x -> Maybe t
decodeBT = decodeLambda . fromBinaryTree . fmap free
