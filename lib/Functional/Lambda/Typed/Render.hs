{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Functional.Lambda.Typed.Render
	( VerySafe
	, Rendering
	, RenderS
	, fromString
	, fromStringS
	, LambdaShow(..)
	, LambdaRender(..)
	, TypedRenderS
	, TypedRendering
	) where

import Functional.Lambda.Typed
	( TypedLambda(..), TypedInput, TypedCombinator
	, reType, abstract, input, ($$$), liftInput, liftFree
	)
import Functional.Lambda.Typed.Function (id, flip, compose)
import qualified Functional.Lambda as L
import Functional.Reducible (($$))
import Functional.VChar (VChar)
import Functional.Iota (IotaSafe)
import Functional.Lambda (LambdaSafe)
import Functional.Lambda.Typed.Semigroup (LambdaSemigroup, cat)
import NatTypes (S(S,Z))

import Prelude hiding (id, show, flip)
import ValidLiterals (Validate(..), Lift)
import Data.Type.Set (Union)

type VerySafe = Union IotaSafe LambdaSafe

-- Represents a string of free variables, or the identity
data Rendering

-- Allows for concatenation via function composition
type RenderS = Rendering -> Rendering

type TypedRenderS = TypedLambda RenderS (VChar VerySafe)
type TypedRendering = TypedLambda Rendering (VChar VerySafe)

fromString :: Validate Char c => String -> Maybe (TypedLambda Rendering c)
fromString "" = Just $ reType id
fromString s = TypedLambda . foldl1 ($$) <$>
	traverse (fmap pure . fromLiteral) s

fromStringS :: Validate Char c => String -> Maybe (TypedLambda RenderS c)
fromStringS s = TypedLambda . L.abstract . foldl ($$) (pure Z) <$>
	traverse (fmap (pure . S) . fromLiteral) s

instance LambdaSemigroup RenderS where
	cat = flip $$$ compose

-- Allow compile time construction of renderings using template haskell
instance (Lift c, Validate Char c) =>
	Validate String (TypedLambda Rendering c) where
		fromLiteral = fromString

instance (Lift c, Validate Char c) =>
	Validate String (TypedLambda RenderS c) where
		fromLiteral = fromStringS

class LambdaShow t where
	show :: TypedLambda (t -> RenderS) (VChar VerySafe)

class LambdaRender t where
	render :: TypedLambda (t -> Rendering) (VChar VerySafe)

instance LambdaShow t => LambdaRender t where
	render :: TypedLambda (t -> Rendering) (VChar VerySafe)
	render = abstract $
		liftFree show $$$
		liftInput (input :: TypedInput 1 t) $$$
		(reType id :: TypedCombinator Rendering)
