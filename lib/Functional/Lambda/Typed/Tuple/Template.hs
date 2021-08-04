{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Functional.Lambda.Typed.Tuple.Template
	( generateTuples
	) where

import qualified Language.Haskell.TH as TH
import Control.Monad (replicateM, join)
import Data.List (intersperse)
import ValidLiterals (valid)

import Functional.Lambda.Typed
	( TypedCombinator, TypedLambda, TypedInput
	, ($$$), input, abstract, toCombinator, reType, liftInput, fromTyped
	, liftFree
	)
import Functional.Lambda.Typed.Eq (neq)
import Functional.Lambda.Typed.Bool (magicIf, magicElseIf, magicElse)
import Functional.Reducible (($$))
import Functional.Lambda.Typed.Render (TypedRenderS)
import qualified Functional.Lambda.Typed.Render as TR

-- Turn an int into a type literal
typeLit :: Int -> TH.Q TH.Type
typeLit = TH.litT . TH.numTyLit . toInteger

-- Combine n types into an n-tuple type
tupleN :: [TH.Q TH.Type] -> TH.Q TH.Type
tupleN tupleTypes = foldl TH.appT (TH.tupleT $ length tupleTypes) tupleTypes

-- Apply the typed lambda abstract method n times to something
abstractN :: Int -> TH.Q TH.Exp -> TH.Q TH.Exp
abstractN n e = foldl (\a _-> [| abstract $a |]) e $ replicate n ()

-- Get the constructor for an n-tuple value
tupleConstructor :: Int -> TH.Q TH.Exp
tupleConstructor n = TH.conE $ TH.tupleDataName n

-- Turn n types and a return type into the functional equivalent of an n-tuple
fTupleN :: [TH.Q TH.Type] -> TH.Q TH.Type -> TH.Q TH.Type
fTupleN tupleTypes returnType = foldl
	TH.appT
	(TH.conT $ TH.mkName $ "FTuple" <> show (length tupleTypes))
	(tupleTypes <> [returnType])

-- Declare the functional equivalent of the n-tuple type
declFTupleN :: Int -> TH.Q TH.Dec
declFTupleN n = do
	tupleVars <- replicateM n (TH.newName "a")
	let
		returnVar = TH.mkName "x"
		tupleTypes = TH.varT <$> tupleVars
		returnType = TH.varT returnVar
		extractorType = foldr (\a x -> [t| $a -> $x |]) returnType tupleTypes
		finalType = [t| $extractorType -> $returnType |]
		name = TH.mkName $ "FTuple" <> show n
	TH.tySynD name (TH.plainTV <$> (tupleVars <> [returnVar])) finalType

-- Function to convert to functional tuple type
toFTupleN :: Int -> TH.Q TH.Exp
toFTupleN n = TH.varE $ TH.mkName $ "toFTuple" <> show n

-- Function to convert from functional tuple type
fromFTupleN :: Int -> TH.Q TH.Exp
fromFTupleN n = TH.varE $ TH.mkName $ "fromFTuple" <> show n

-- Declare conversion functions for functional tuple types
declConvertFTupleN :: Int -> TH.Q [TH.Dec]
declConvertFTupleN n = do
	tupleVars <- replicateM n (TH.newName "a")
	let
		tupleTypes = TH.varT <$> tupleVars
		returnType = TH.varT $ TH.mkName "x"
		lambdaType = TH.varT $ TH.mkName "v"
		tupleType = [t| TypedLambda $(tupleN tupleTypes) $lambdaType |]
		fTupleType =
			[t| TypedLambda $(fTupleN tupleTypes returnType) $lambdaType |]
		toName = TH.mkName $ "toFTuple" <> show n
		fromName = TH.mkName $ "fromFTuple" <> show n
	totypedec <- TH.sigD toName [t| $tupleType -> $fTupleType |]
	tofundec <- TH.funD toName [TH.clause [] (TH.normalB [| reType |]) []]
	fromtypedec <- TH.sigD fromName [t| $fTupleType -> $tupleType |]
	fromfundec <- TH.funD fromName [TH.clause [] (TH.normalB [| reType |]) []]
	pure [totypedec, tofundec, fromtypedec, fromfundec]

-- Eq instance for lambda tuples
instanceLambdaEq :: Int -> TH.Q TH.Dec
instanceLambdaEq n = do
	tupleVars <- replicateM n (TH.newName "a")
	let
		tupleTypes = TH.varT <$> tupleVars
		tupleType = tupleN tupleTypes
		lambdaEq = TH.conT $ TH.mkName "LambdaEq"
		constraints = TH.appT lambdaEq <$> tupleTypes
		result = TH.appT lambdaEq tupleType
		neqcheck m = [| neq $$$
			liftInput (input :: TypedInput $(typeLit $ 2*n-m)
				$(tupleTypes !! m)) $$$
			liftInput (input :: TypedInput $(typeLit $ n-m) $(tupleTypes !! m))
			|]
		ifs = foldl (\a b -> [| $a $$$ $b |]) [| magicIf |] $
			join (intersperse [ [| toLambda False |], [| magicElseIf |] ] $
				pure . neqcheck <$> [0..n-1]) <>
			[ [| toLambda False |], [| magicElse |], [| toLambda True |] ]
		eqdef = [| toCombinator $ abstract $ abstract $
			$(toFTupleN n) (input :: TypedInput 2 $tupleType) $$$
			$(abstractN n [|
				$(toFTupleN n) (liftInput (input :: TypedInput $(typeLit $ n+1)
					$tupleType)) $$$
				$(abstractN n ifs)
				|] )
			|]
	TH.instanceD (sequenceA constraints) result
		[TH.funD (TH.mkName "eq") [TH.clause [] (TH.normalB eqdef) []]]

-- Representable instance for lambda tuples
instanceRepresentable :: Int -> TH.Q TH.Dec
instanceRepresentable n = do
	tupleVars <- replicateM n $ TH.newName "a"
	inputVars <- replicateM n $ TH.newName "b"
	let
		tupleTypes = TH.varT <$> tupleVars
		tupleType = tupleN tupleTypes
		returnType = TH.varT $ TH.mkName "x"
		representable = TH.conT $ TH.mkName "Representable"
		constraints = TH.appT representable <$> tupleTypes
		result = TH.appT representable tupleType
		extractorType = foldr (\a b -> [t| $a -> $b |]) returnType tupleTypes
		extraction = foldl (\a b -> [| $a $$$ $b |])
			[| input :: TypedInput 1 $extractorType |] $
			(\b -> [| toLambda $(TH.varE b) |]) <$> inputVars
		convertdef =
			[| $(fromFTupleN n) $ toCombinator $ abstract $extraction |]
	TH.instanceD (sequenceA constraints) result
		[TH.funD (TH.mkName "toLambda") [TH.clause
			[TH.tupP $ TH.varP <$> inputVars]
			(TH.normalB convertdef)
			[]
		]]

-- Constructor for n-tuples
mkTupleN :: Int -> TH.Q TH.Exp
mkTupleN n = TH.varE $ TH.mkName $ "mkTuple" <> show n

-- Declares the constructor for n-tuples
declMkTupleN :: Int -> TH.Q [TH.Dec]
declMkTupleN n = do
	tupleVars <- replicateM n $ TH.newName "a"
	let
		tupleTypes = TH.varT <$> tupleVars
		returnType = TH.varT $ TH.mkName "x"
		extractorType =
			foldr (\a b -> [t| $a -> $b |]) returnType tupleTypes
		functionType =
			foldr (\a b -> [t| $a -> $b |]) (tupleN tupleTypes) tupleTypes
		name = TH.mkName $ "mkTuple" <> show n
		inputs = (\k -> [| liftInput
			(input :: TypedInput $(typeLit $ n+1-k) $(tupleTypes !! k)) |])
			<$> [0..n-1]
		extraction = foldl (\a b -> [| $a $$$ $b |])
			[| liftInput (input :: TypedInput 1 $extractorType) |]
			inputs
		function = [| toCombinator $(abstractN n
			[| $(fromFTupleN n) $ abstract $extraction |]
			) |]
	typeDecl <- TH.sigD name $ TH.forallT (TH.plainTV <$> tupleVars) (pure [])
		[t| TypedCombinator $functionType |]
	funDecl <- TH.funD name [TH.clause [] (TH.normalB function) []]
	pure [typeDecl, funDecl]

-- Extractor for the mth value of an n-tuple
getNofM :: Int -> Int -> TH.Q TH.Exp
getNofM n m = TH.varE $ TH.mkName $ "get" <> show n <> "of" <> show m

-- Declares the extractor for n-tuples
declGetNOfM :: Int -> Int -> TH.Q [TH.Dec]
declGetNOfM n m = do
	typeVars <- replicateM m $ TH.newName "a"
	let
		tupleType = tupleN $ TH.varT <$> typeVars
		returnType = TH.varT $ typeVars !! (n-1)
		functionType = [t| $tupleType -> $returnType |]
		functionName = TH.mkName $ "get" <> show n <> "of" <> show m
		extractorReturn = [| liftInput
			(input :: TypedInput $(typeLit $ m-n+1) $returnType) |]
		extractor = abstractN m extractorReturn
		function = [| toCombinator $ abstract $
			$(toFTupleN m) (input :: TypedInput 1 $tupleType) $$$ $extractor |]
	typeDeclaration <- TH.sigD functionName $
		TH.forallT (TH.plainTV <$> typeVars) (pure [])
		[t| TypedCombinator $functionType |]
	funDeclaration <-
		TH.funD functionName [TH.clause [] (TH.normalB function) []]
	pure [typeDeclaration, funDeclaration]

-- Declares all n extractors for an n-tuple
declGetAllOfN :: Int -> TH.Q [TH.Dec]
declGetAllOfN n = join <$> traverse (`declGetNOfM` n) [1..n]

-- Decode instance for n-tuples
instanceDecode :: Int -> TH.Q TH.Dec
instanceDecode n = do
	typeVars <- replicateM n $ TH.newName "a"
	let
		tupleTypes = TH.varT <$> typeVars
		decode = TH.conT $ TH.mkName "Decode"
		constraints = TH.appT decode <$> tupleTypes
		result = TH.appT decode $ tupleN tupleTypes
		decodeReturn = foldl
			(\a m ->
				[| $a <*> decodeLambda (fromTyped $(getNofM m n) $$ l) |])
			[| pure $(tupleConstructor n) |]
			[1..n]
	TH.instanceD (sequenceA constraints) result
		[ TH.funD (TH.mkName "decodeLambda") [ TH.clause
			[ TH.varP $ TH.mkName "l" ]
			(TH.normalB decodeReturn)
			[]
		]]

-- Show instance for n-tuples
instanceShow :: Int -> TH.Q TH.Dec
instanceShow n = do
	typeVars <- replicateM n $ TH.newName "a"
	let
		tupleTypes = TH.varT <$> typeVars
		lambdaShow = TH.conT $ TH.mkName "LambdaShow"
		constraints = TH.appT lambdaShow <$> tupleTypes
		result = TH.appT lambdaShow $ tupleN tupleTypes
		showEach = zipWith
			(\m t -> [|
				liftFree TR.show $$$
				liftInput (input :: TypedInput $(typeLit m) $t)
			|])
			[n,n-1..]
			tupleTypes
		validFree :: String -> TH.Q TH.Exp
		validFree s =
			[| $(TH.unType <$> (valid s :: TH.Q (TH.TExp TypedRenderS)))
				:: TypedRenderS |]
		showVals = foldl1
			(\a b -> [|
				cat $$$
				$a $$$ (
					cat $$$
					liftFree $(validFree ",") $$$
					$b
				)
			|])
			(
				[| liftFree $(validFree "Tuple[") |] :
				showEach <>
				[ [| liftFree $(validFree "]") |] ]
			)
		showDef = [| abstract $
			$(toFTupleN n)
				(liftInput (input :: TypedInput 1 $(tupleN tupleTypes))) $$$
			$(abstractN n showVals)
			|]
	TH.instanceD (sequenceA constraints) result
		[ TH.funD (TH.mkName "show")
			[ TH.clause [] (TH.normalB showDef) [] ]
		]

-- Semigroup instance for n-tuples
instanceSemigroup :: Int -> TH.Q TH.Dec
instanceSemigroup n = do
	typeVars <- replicateM n $ TH.newName "a"
	let
		tupleTypes = TH.varT <$> typeVars
		tupleType = tupleN tupleTypes
		lambdaSemigroup = TH.conT $ TH.mkName "LambdaSemigroup"
		constraints = TH.appT lambdaSemigroup <$> tupleTypes
		result = TH.appT lambdaSemigroup tupleType
		catResult = foldl
			(\f m -> [|
				$f $$$ (
					cat $$$
					liftInput (input :: TypedInput $(typeLit $ 2*n-m)
						$(tupleTypes !! m)) $$$
					liftInput (input :: TypedInput $(typeLit $ n-m)
						$(tupleTypes !! m))
				)
			|])
			(mkTupleN n)
			[0..n-1]
		catdef = [| toCombinator $ abstract $ abstract $
			$(toFTupleN n) (input :: TypedInput 2 $tupleType) $$$
			$(abstractN n [|
				$(toFTupleN n) (liftInput (input :: TypedInput $(typeLit $ n+1)
					$tupleType)) $$$
				$(abstractN n catResult)
				|] )
			|]
	TH.instanceD (sequenceA constraints) result
		[TH.funD (TH.mkName "cat") [TH.clause [] (TH.normalB catdef) []]]

-- Functor instance for n-tuples
instanceFunctor :: Int -> TH.Q TH.Dec
instanceFunctor n = do
	typeVars <- replicateM (n-1) $ TH.newName "a"
	inputVar <- TH.newName "b"
	outputVar <- TH.newName "c"
	let
		tupleTypes = TH.varT <$> typeVars
		inputType = TH.varT inputVar
		outputType = TH.varT outputVar
		inputTupleType = tupleN $ tupleTypes <> [inputType]
		-- outputTupleType = tupleN $ tupleTypes <> [outputType]
		lambdaFunctor = TH.conT $ TH.mkName "LambdaFunctor"
		result = TH.appT lambdaFunctor $
			foldl TH.appT (TH.tupleT n) tupleTypes
		remakeTuple = foldl
			(\f m -> [|
				$f $$$
				liftInput (input :: TypedInput $(typeLit $ n-m)
					$(tupleTypes !! m))
			|])
			(mkTupleN n)
			[0..n-2]
		mapDef = [| toCombinator $ abstract $ abstract $
			$(toFTupleN n)
				(liftInput (input :: TypedInput 1 $inputTupleType)) $$$
			$(abstractN n [|
				$remakeTuple $$$ (
					(input :: TypedInput $(typeLit $ n+2)
						($inputType -> $outputType)) $$$
					liftInput (input :: TypedInput 1 $inputType)
					)
				|])
			|]
	TH.instanceD (pure []) result
		[TH.funD (TH.mkName "map") [TH.clause [] (TH.normalB mapDef) []]]

-- -- Convert tuple to a list
-- declToListN :: Int -> TH.Q [TH.Dec]
-- declToListN n = do
-- 	typeVar <- TH.newName "a"
-- 	let
-- 		paramType = TH.varT typeVar
-- 		inputType = tupleN $ replicate n paramType
-- 		outputType = [t| [ $paramType ] |]
-- 		makeList = foldl
-- 			(\l m -> [|
-- 				cons $$$
-- 				liftInput (input :: TypedInput $(typeLit m) $paramType) $$$
-- 				l
-- 			|])
-- 			empty
-- 			[1..n]
-- 		toListDef = [| toCombinator $ abstract $
-- 			$(toFTupleN n) (input :: TypedInput 1 $inputType) $$$
-- 			$(abstractN n makeList)
-- 			|]
-- 		functionName = "toList" <> show n
-- 	typeDeclaration <- TH.sigD functionName $
-- 		TH.forallT [TH.plainTV typeVar] (pure [])
-- 		[t| TypedCombinator ($inputType -> $outputType) |]
-- 	funDeclaration <-
-- 		TH.funD functionName [TH.clause [] (TH.normalB toListDef) []]
-- 	pure [typeDeclaration, funDeclaration]

-- Generates all declarations needed for an n-tuple
generateTuple :: Int -> TH.Q [TH.Dec]
generateTuple n = join <$> sequenceA
	[ pure <$> declFTupleN n
	, declConvertFTupleN n
	, pure <$> instanceLambdaEq n
	, pure <$> instanceRepresentable n
	, declMkTupleN n
	, declGetAllOfN n
	, pure <$> instanceDecode n
	, pure <$> instanceShow n
	, pure <$> instanceSemigroup n
	, pure <$> instanceFunctor n
	-- , declToListN n
	]

-- Generate all declarations for tuples of size 2 to n
generateTuples :: Int -> TH.Q [TH.Dec]
generateTuples n = join <$> traverse generateTuple [2..n]
