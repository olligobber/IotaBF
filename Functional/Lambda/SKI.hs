{-# LANGUAGE ScopedTypeVariables #-}

module Functional.Lambda.SKI
	( toLambda
	, toSKI
	) where

import Data.Function (on)
import Data.Maybe (isNothing)

import Functional.Lambda
	(Lambda(..), LambdaTerm(..), free, abstract, substitute)
import Functional.Reducible (($$))
import Functional.SKI (SKI(..), HasSKI(..))
import Functional.BinaryTree (BinaryTree(..), fromBinaryTree)

toLambda :: SKI -> Lambda v
toLambda S = abstract $ abstract $ abstract $
	free (Just (Just Nothing)) $$ free Nothing $$
	(free (Just Nothing) $$ free Nothing)
toLambda K = abstract $ abstract $ free $ Just Nothing
toLambda I = abstract $ free Nothing

-- Checks if a variable occurs bound to an abstraction wrapping the given tree
isBound :: Lambda a -> Bool
isBound l = any isNothing $ substitute (Just <$> l) $ free Nothing

toSKI :: forall a. HasSKI a => Lambda a -> BinaryTree a
toSKI (Lambda (Leaf (Free x))) = Leaf x
toSKI (Lambda (Leaf (Bound _))) = error "Bound variable with no abstraction"
toSKI (Lambda (l :^: r)) = ((:^:) `on` toSKI . Lambda) l r
toSKI (Lambda (Leaf (Abstraction l))) | not $ isBound l = k $$ toSKI l
toSKI (Lambda (Leaf (Abstraction (Lambda (Leaf (Bound 1)))))) = i
toSKI (Lambda (Leaf (Abstraction l@(Lambda (Leaf (Abstraction _)))))) =
	toSKI $ abstract $ fromBinaryTree $ free <$>
	toSKI (substitute (Just <$> l) $ free Nothing)
toSKI (Lambda (Leaf (Abstraction (Lambda (l :^: r))))) = s $$
	toSKI (Lambda $ Leaf $ Abstraction $ Lambda l) $$
	toSKI (Lambda $ Leaf $ Abstraction $ Lambda r)
toSKI _ = error "Unreachable option reached"
