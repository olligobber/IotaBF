{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module PrintLambdasTemplate
	( FunctionEntry(..)
	, makeEntry
	) where

import qualified Language.Haskell.TH as TH

import Functional.Lambda (Lambda)
import Functional.Lambda.Typed (fromTyped)
import THPrintType (printType)
import Functional.Iota.Free (IFree)

data FunctionEntry = FunctionEntry
	{ name :: String
	, description :: String
	, ltype :: String
	, lambda :: Lambda IFree
	}

tl :: TH.Type -> Bool
tl (TH.ConT t) = TH.nameBase t == "TypedLambda" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tl _ = False

tc :: TH.Type -> Bool
tc (TH.ConT t) = TH.nameBase t == "TypedCombinator" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tc _ = False

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
	[| FunctionEntry function desc showType (fromTyped $(TH.varE fname)) |]
