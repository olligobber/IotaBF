{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module PrintLambdasTemplate
	( FunctionEntry(..)
	, makeEntry
	) where

import qualified Language.Haskell.TH as TH

import Functional.Lambda (Lambda, LambdaSafe)
import Functional.Lambda.Typed (fromTyped, TypedLambda)
import THPrintType (printType)
import Functional.VChar (VChar, release)
import Functional.Lambda.Typed.Render (VerySafe)
-- import Functional.Iota (IotaSafe)

data FunctionEntry = FunctionEntry
	{ name :: String
	, description :: String
	, ltype :: String
	, lambda :: Lambda (VChar LambdaSafe)
	-- , iota :: TODO
	}

-- Test if a type constructor is for typed lambda calculus
tl :: TH.Type -> Bool
tl (TH.ConT t) = TH.nameBase t == "TypedLambda" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tl _ = False

-- Test if a type constructor is for a typed lambda combinator
tc :: TH.Type -> Bool
tc (TH.ConT t) = TH.nameBase t == "TypedCombinator" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tc _ = False

-- Given a function's name and description, make an entry using its type and
-- definition
makeEntry :: String -> String -> TH.Q TH.Exp
makeEntry function desc = do
	Just fname <- TH.lookupValueName function
	actualType <- TH.reifyType fname
	let
		lambdaType = case actualType of
			TH.ForallT _ [] (TH.AppT (TH.AppT c t) _) | tl c -> t
			TH.AppT (TH.AppT c t) _ | tl c -> t
			TH.AppT c t | tc c -> t
			t -> t
		showType = case printType lambdaType of
			Just s -> s
			Nothing -> TH.pprint lambdaType
	[| FunctionEntry
		function
		desc
		showType
		((release @LambdaSafe @VerySafe) <$> fromTyped $(TH.varE fname))
		|]
