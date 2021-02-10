{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Functional.Lambda.Typed.Tuple.ProjectionsTemplate
	( generateProjections
	) where

import qualified Language.Haskell.TH as TH
import Control.Monad (replicateM)
import Data.List((!!))

import Functional.Lambda.Typed
	( TypedCombinator, TypedLambda, TypedInput, Representable(..)
	, ($$$), input, abstract, toCombinator, reType, lift
	)

getNofM :: Int -> Int -> TH.Q [TH.Dec]
getNofM n m = do
	typevars <- replicateM m (TH.newName "a")
	let
		tupletype = pure $ foldl TH.AppT (TH.TupleT m) $ TH.VarT <$> typevars
		returntype = pure $ TH.VarT $ typevars !! (n-1)
		functiontype = TH.appT (TH.AppT TH.ArrowT <$> tupletype) returntype
		functionname = TH.mkName $ "get" <> show n <> "of" <> show m
		typedeclaration = TH.sigD functionname $
			TH.forallT (TH.PlainTV <$> typevars) (pure [])
			[t| TypedCombinator $functiontype |]
		extractorReturn = [| lift (input :: TypedInput
			$(pure $ TH.LitT $ TH.NumTyLit $ toInteger $ m-n+1)
			$returntype) |]
		extractor = foldr TH.appE extractorReturn $ replicate m [| abstract |]
		function = [| toCombinator $ abstract $
			$(pure $ TH.VarE $ TH.mkName $ "toFTuple" <> show m)
			(input :: TypedInput 1 $tupletype) $$$ $extractor |]
		fundeclaration =
			TH.funD functionname [TH.clause [] (TH.normalB function) []]
	sequenceA [typedeclaration, fundeclaration]

generateProjections :: Int -> TH.Q [TH.Dec]
generateProjections maxtup = fmap concat $ sequenceA $ do
	m <- [2..maxtup]
	n <- [1..m]
	pure $ getNofM n m
