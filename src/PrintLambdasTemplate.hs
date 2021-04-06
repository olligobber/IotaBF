{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module PrintLambdasTemplate
	( FunctionEntry(..)
	, makeEntry
	) where

import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
import Data.Map (Map, (!?))
import Control.Monad.State (StateT, gets, put, evalStateT)

import Functional.Lambda (LambdaCombinator)
import Functional.Lambda.Typed (fromTyped)

data FunctionEntry = FunctionEntry
	{ name :: String
	, description :: String
	, ltype :: String
	, lambda :: LambdaCombinator
	}

tl :: TH.Type -> Bool
tl (TH.ConT t) = TH.nameBase t == "TypedLambda" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tl _ = False

tc :: TH.Type -> Bool
tc (TH.ConT t) = TH.nameBase t == "TypedCombinator" &&
	TH.nameModule t == Just "Functional.Lambda.Typed"
tc _ = False

-- Prints a type, renaming variables and using base names of constructors
-- Fails for types I haven't written
printType :: TH.Type -> Maybe String
printType = flip evalStateT (variables, M.empty) . printTypeS False where

	printTypeS ::
		Bool -> TH.Type -> StateT ([String], Map TH.Name String) Maybe String
	printTypeS False (TH.AppT (TH.AppT TH.ArrowT t) s) = do
		pt <- printTypeS True t
		ps <- printTypeS False s
		pure $ pt <> " -> " <> ps
	printTypeS False (TH.AppT t@(TH.AppT _ _) s) = do
		pt <- printTypeS False t
		ps <- printTypeS True s
		pure $ pt <> " " <> ps
	printTypeS False (TH.AppT t s) = do
		pt <- printTypeS True t
		ps <- printTypeS True s
		pure $ pt <> " " <> ps
	printTypeS _ (TH.VarT n) = do
		names <- gets snd
		case names !? n of
			Just s -> pure s
			Nothing -> do
				newname:unused <- gets fst
				put (unused, M.insert n newname names)
				pure newname
	printTypeS _ (TH.ConT n) = pure $ TH.nameBase n
	printTypeS False (TH.InfixT t n s) = do
		pt <- printTypeS True t
		ps <- printTypeS True s
		pure $ "(" <> pt <> ") " <> TH.nameBase n <> " (" <> ps <> ")"
	printTypeS b (TH.ParensT t) = printTypeS b t
	printTypeS _ (TH.TupleT n) = pure $ "(" <> replicate (n-1) ',' <> ")"
	printTypeS False TH.ArrowT = pure "->"
	printTypeS _ TH.ListT = pure "[]"
	printTypeS True x = do
		px <- printTypeS False x
		pure $ "(" <> px <> ")"
	printTypeS _ _ = fail "Not implemented"

	variables :: [String]
	variables =
		(pure <$> ['a'..'z']) <> do
			num <- [0..] :: [Integer]
			letter <- ['a'..'z']
			pure $ letter:'_':show num


makeEntry :: String -> String -> TH.Q TH.Exp
makeEntry function desc = do
	Just fname <- TH.lookupValueName function
	actualType <- TH.reifyType fname
	let
		lambdaType = case actualType of
			TH.ForallT _ _ (TH.AppT (TH.AppT c t) _) | tl c -> t
			TH.AppT c t | tc c -> t
			t -> t
		showType = case printType lambdaType of
			Just s -> s
			Nothing -> TH.pprint lambdaType
	[| FunctionEntry function desc showType (fromTyped $(TH.varE fname)) |]
