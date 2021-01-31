{-# LANGUAGE FlexibleContexts #-}

module Functional.Decode
	( Decode(..)
	) where

import Functional.Reducible (Reducible, Var)
import Functional.BinaryTree (BinaryTree)
import Functional.Lambda (Lambda, LambdaTerm)

-- Class of types that can be converted from their lambda calculus encoding
class Decode t where
	decodeBT :: Reducible (BinaryTree (Either (Var t) x)) x =>
		BinaryTree x -> Maybe t
	decodeLambda :: Reducible (BinaryTree (LambdaTerm (Either (Var t) x))) x =>
		Lambda x -> Maybe t
