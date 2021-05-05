{-# LANGUAGE ScopedTypeVariables #-}

module Functional.Lambda.SKI
	( toLambda
	, toSKI
	) where

import Data.Function (on)
import Data.Maybe (isNothing)

import Functional.Lambda
	(Lambda(..), LambdaTerm(..), abstract, substitute)
import Functional.Reducible (($$))
import Functional.SKI (SKI(..), HasSKI(..))
import Functional.BinaryTree (BinaryTree(..), fromBinaryTree)

toLambda :: SKI -> Lambda v
toLambda S = abstract $ abstract $ abstract $
	pure (Just (Just Nothing)) $$ pure Nothing $$
	(pure (Just Nothing) $$ pure Nothing)
toLambda K = abstract $ abstract $ pure $ Just Nothing
toLambda I = abstract $ pure Nothing

-- Checks if a variable occurs bound to an abstraction wrapping the given tree
isBound :: Lambda a -> Bool
isBound l = any isNothing $ substitute (Just <$> l) $ pure Nothing

toSKI :: forall a. HasSKI a => Lambda a -> BinaryTree a
toSKI (Lambda (Leaf (LambdaFree x))) = Leaf x
toSKI (Lambda (Leaf (Bound _))) = error "Bound variable with no abstraction"
toSKI (Lambda (l :^: r)) = ((:^:) `on` toSKI . Lambda) l r
toSKI (Lambda (Leaf (Abstraction l))) | not $ isBound l = k $$ toSKI l
toSKI (Lambda (Leaf (Abstraction (Lambda (Leaf (Bound 1)))))) = i
toSKI (Lambda (Leaf (Abstraction l@(Lambda (Leaf (Abstraction _)))))) =
	toSKI $ abstract $ fromBinaryTree $ pure <$>
	toSKI (substitute (Just <$> l) $ pure Nothing)
toSKI (Lambda (Leaf (Abstraction (Lambda (l :^: r))))) = s $$
	toSKI (Lambda $ Leaf $ Abstraction $ Lambda l) $$
	toSKI (Lambda $ Leaf $ Abstraction $ Lambda r)
toSKI _ = error "Unreachable option reached"
