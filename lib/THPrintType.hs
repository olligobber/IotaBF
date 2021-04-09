module THPrintType
	( printType
	) where

import qualified Language.Haskell.TH as TH
import qualified Data.Map as M
import Data.Map (Map, (!?))
import Control.Monad.State (State, gets, put, evalState)
import Data.List (intercalate)

data BetterType
	= Arrow BetterType BetterType
	| Tuple [BetterType] Int
	| Application BetterType BetterType
	| List BetterType
	| EmptyArrow
	| EmptyList
	| Constructor String
	| Variable TH.Name

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

type VarState = State ((Char, Maybe Integer), Map TH.Name String)

printUnsafe :: BetterType -> VarState String
printUnsafe (Arrow t s) = do
	pt <- printLeftSafe t
	ps <- printUnsafe s
	pure $ pt <> " -> " <> ps
printUnsafe (Tuple ts n)
	| n == 0 = do
		pts <- traverse printUnsafe ts
		pure $ "(" <> intercalate "," pts <> ")"
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

printLeftSafe :: BetterType -> VarState String
printLeftSafe t@(Arrow _ _) = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printLeftSafe t = printUnsafe t

printSafe :: BetterType -> VarState String
printSafe t@(Tuple _ n) | n /= 0 = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printSafe t@(Application _ _) = do
	pt <- printUnsafe t
	pure $ "(" <> pt <> ")"
printSafe t = printLeftSafe t

nextVar :: (Char, Maybe Integer) -> (Char, Maybe Integer)
nextVar ('z', n) = ('a', maybe (Just 0) (Just . succ) n)
nextVar (c, n) = (succ c, n)

renderVar :: (Char, Maybe Integer) -> String
renderVar (c, Nothing) = [c]
renderVar (c, Just n) = c : show n

-- Prints a type, renaming variables and using base names of constructors
-- Fails for types I haven't written
printType :: TH.Type -> Maybe String
printType = fmap (flip evalState (('a', Nothing), M.empty) . printUnsafe) .
	toBetterType
