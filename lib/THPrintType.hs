module THPrintType
	( printType
	) where

import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
import Data.Map (Map, (!?))
import Control.Monad.State (State, gets, put, evalState)
import Data.List (intercalate)

-- TODO make more complete, steal ideas from template haskell pretty printer

-- A more useful representation of types for rendering purposes,
-- though incomplete
data BetterType
	= Arrow BetterType BetterType
	| Tuple [BetterType] Int -- List of input types and number of missing inputs
	| Application BetterType BetterType
	| List BetterType
	| EmptyArrow
	| EmptyList
	| Constructor String
	| Variable TH.Name

-- Convert from usual AST representation
toBetterType :: TH.Type -> Maybe BetterType
toBetterType (TH.AppT (TH.AppT TH.ArrowT t) s) =
	Arrow <$> toBetterType t <*> toBetterType s
toBetterType (TH.AppT t s) = case toBetterType t of
	Just (Tuple ts n) | n > 0 ->
		(\x -> Tuple (ts <> [x]) (n-1)) <$> toBetterType s
	Just EmptyList -> List <$> toBetterType s
	Just pt -> Application pt <$> toBetterType s
	Nothing -> Nothing
toBetterType (TH.VarT n) = Just $ Variable n
toBetterType (TH.ConT n)
	-- Check if the constructor should be a TupleT instead
	| TH.nameModule n == Just "GHC.Tuple" = case TH.nameBase n of
		"()" -> Just $ Tuple [] 0
		'(':t@(_:_) | all (== ',') (init t) && last t == ')' ->
			Just $ Tuple [] $ length t
		_ -> Just $ Constructor $ TH.nameBase n
	| otherwise = Just $ Constructor $ TH.nameBase n
toBetterType (TH.TupleT n) = Just $ Tuple [] n
toBetterType TH.ArrowT = Just EmptyArrow
toBetterType TH.ListT = Just EmptyList
toBetterType _ = Nothing

-- Stateful monad for tracking used variable names and next available
-- TODO could use reader instead?
type VarState = State ((Char, Maybe Integer), Map TH.Name String)

-- Iterate the variable tracker
nextVar :: (Char, Maybe Integer) -> (Char, Maybe Integer)
nextVar ('z', n) = ('a', maybe (Just 0) (Just . succ) n)
nextVar (c, n) = (succ c, n)

-- Turn the tracked variable into an actual name
renderVar :: (Char, Maybe Integer) -> String
renderVar (c, Nothing) = [c]
renderVar (c, Just n) = c : show n

-- Render a type in a context where no brackets are needed
printUnsafe :: BetterType -> VarState String
printUnsafe (Arrow t s) = do
	pt <- printLeftSafe t
	ps <- printUnsafe s
	pure $ pt <> " -> " <> ps
printUnsafe (Tuple ts n)
	-- Complete tuple
	| n == 0 = do
		pts <- traverse printUnsafe ts
		pure $ "(" <> intercalate "," pts <> ")"
	-- Incomplete tuple
	| otherwise = do
		pts <- traverse printSafe ts
		pure $ "(" <> replicate (length pts + n - 1) ',' <> ") " <> unwords pts
printUnsafe (Application t s) = do
	pt <- printLeftSafe t
	ps <- printSafe s
	pure $ pt <> " " <> ps
printUnsafe (List t) = do
	pt <- printUnsafe t
	pure $ "[" <> pt <> "]"
printUnsafe EmptyArrow = pure "(->)"
printUnsafe EmptyList = pure "[]"
printUnsafe (Constructor s) = pure s
printUnsafe (Variable n) = do
	names <- gets snd
	case names !? n of
		Just s -> pure s
		Nothing -> do
			var <- gets fst
			put (nextVar var, M.insert n (renderVar var) names)
			pure $ renderVar var

-- Render a type possibly with brackets
-- so it can be on the left of an arrow or application
printLeftSafe :: BetterType -> VarState String
printLeftSafe t@(Arrow _ _) = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printLeftSafe t = printUnsafe t

-- Render a type usually with brackets so it can appear anywhere
printSafe :: BetterType -> VarState String
printSafe t@(Tuple _ n) | n /= 0 = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printSafe t@(Application _ _) = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printSafe t = printLeftSafe t

-- Prints a type, renaming variables and using base names of constructors
-- Fails for types I haven't written
printType :: TH.Type -> Maybe String
printType = fmap (flip evalState (('a', Nothing), M.empty) . printUnsafe) .
	toBetterType
