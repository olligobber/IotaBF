{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Functional.Lambda.Typed.Render
	( Rendering
	, RenderS
	, fromString
	, fromStringS
	, concat
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
import Functional.Iota.Free (IFree)

import Prelude hiding (id, show, concat, flip)
import ValidLiterals (Validate(..), Lift)

-- Represents a string of free variables, or the identity
data Rendering

-- Allows for concatenation via composition
type RenderS = Rendering -> Rendering

fromString :: Validate Char c => String -> Maybe (TypedLambda Rendering c)
fromString "" = Just $ reType id
fromString s = TypedLambda . foldl1 ($$) <$>
	traverse (fmap L.free . fromLiteral) s

fromStringS :: Validate Char c => String -> Maybe (TypedLambda RenderS c)
fromStringS s = TypedLambda . L.abstract . foldl ($$) (L.free Nothing) <$>
	traverse (fmap (L.free . Just) . fromLiteral) s

concat :: TypedCombinator (RenderS -> RenderS -> RenderS)
concat = flip $$$ compose

instance (Lift c, Validate Char c) =>
	Validate String (TypedLambda Rendering c) where
		fromLiteral = fromString

instance (Lift c, Validate Char c) =>
	Validate String (TypedLambda RenderS c) where
		fromLiteral = fromStringS

class LambdaShow t where
	show :: TypedLambda (t -> RenderS) IFree

class LambdaRender t where
	render :: TypedLambda (t -> Rendering) IFree

instance LambdaShow t => LambdaRender t where
	render :: TypedLambda (t -> Rendering) IFree
	render = abstract $
		liftFree show $$$
		liftInput (input :: TypedInput 1 t) $$$
		(reType id :: TypedCombinator Rendering)

type TypedRenderS = TypedLambda RenderS IFree

type TypedRendering = TypedLambda Rendering IFree
