{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Tuple
	( toFUnit
	, toFTuple2
	, toFTuple3
	, toFTuple4
	, toFTuple5
	, toFTuple6
	, toFTuple7
	, toFTuple8
	, fromFUnit
	, fromFTuple2
	, fromFTuple3
	, fromFTuple4
	, fromFTuple5
	, fromFTuple6
	, fromFTuple7
	, fromFTuple8
	, mkTuple2
	, mkTuple3
	, mkTuple4
	, mkTuple5
	, mkTuple6
	, mkTuple7
	, mkTuple8
	, curry
	, uncurry
	) where

import Prelude hiding (curry, uncurry)

import Functional.Lambda.Typed
	( TypedCombinator, TypedLambda, TypedInput, Representable(..)
	, ($$$), input, abstract, toCombinator, reType, lift
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..), neq)
import Functional.Lambda.Typed.Bool (magicIf, magicElseIf, magicElse)

-- Functional equivalent of tuple types
type FUnit x = x -> x
type FTuple2 a b x = (a -> b -> x) -> x
type FTuple3 a b c x = (a -> b -> c -> x) -> x
type FTuple4 a b c d x = (a -> b -> c -> d -> x) -> x
type FTuple5 a b c d e x = (a -> b -> c -> d -> e -> x) -> x
type FTuple6 a b c d e f x = (a -> b -> c -> d -> e -> f -> x) -> x
type FTuple7 a b c d e f g x =
	(a -> b -> c -> d -> e -> f -> g -> x) -> x
type FTuple8 a b c d e f g h x =
	(a -> b -> c -> d -> e -> f -> g -> h -> x) -> x

toFUnit :: TypedLambda () v -> TypedLambda (FUnit x) v
toFUnit = reType
toFTuple2 :: TypedLambda (a,b) v -> TypedLambda (FTuple2 a b x) v
toFTuple2 = reType
toFTuple3 :: TypedLambda (a,b,c) v -> TypedLambda (FTuple3 a b c x) v
toFTuple3 = reType
toFTuple4 :: TypedLambda (a,b,c,d) v -> TypedLambda (FTuple4 a b c d x) v
toFTuple4 = reType
toFTuple5 :: TypedLambda (a,b,c,d,e) v -> TypedLambda (FTuple5 a b c d e x) v
toFTuple5 = reType
toFTuple6 :: TypedLambda (a,b,c,d,e,f) v ->
	TypedLambda (FTuple6 a b c d e f x) v
toFTuple6 = reType
toFTuple7 :: TypedLambda (a,b,c,d,e,f,g) v ->
	TypedLambda (FTuple7 a b c d e f g x) v
toFTuple7 = reType
toFTuple8 :: TypedLambda (a,b,c,d,e,f,g,h) v ->
	TypedLambda (FTuple8 a b c d e f g h x) v
toFTuple8 = reType

fromFUnit :: TypedLambda (FUnit x) v -> TypedLambda () v
fromFUnit = reType
fromFTuple2 :: TypedLambda (FTuple2 a b x) v -> TypedLambda (a,b) v
fromFTuple2 = reType
fromFTuple3 :: TypedLambda (FTuple3 a b c x) v -> TypedLambda (a,b,c) v
fromFTuple3 = reType
fromFTuple4 :: TypedLambda (FTuple4 a b c d x) v -> TypedLambda (a,b,c,d) v
fromFTuple4 = reType
fromFTuple5 :: TypedLambda (FTuple5 a b c d e x) v -> TypedLambda (a,b,c,d,e) v
fromFTuple5 = reType
fromFTuple6 :: TypedLambda (FTuple6 a b c d e f x) v ->
	TypedLambda (a,b,c,d,e,f) v
fromFTuple6 = reType
fromFTuple7 :: TypedLambda (FTuple7 a b c d e f g x) v ->
	TypedLambda (a,b,c,d,e,f,g) v
fromFTuple7 = reType
fromFTuple8 :: TypedLambda (FTuple8 a b c d e f g h x) v ->
	TypedLambda (a,b,c,d,e,f,g,h) v
fromFTuple8 = reType

instance LambdaEq () where
	eq = toCombinator $ abstract $ abstract $ toLambda True
instance (LambdaEq a, LambdaEq b) => LambdaEq (a,b) where
	eq = toCombinator $ abstract $ abstract $
		toFTuple2 (input :: TypedInput 2 (a,b)) $$$
		abstract (abstract $
			toFTuple2 (lift (input :: TypedInput 3 (a,b))) $$$
			abstract (abstract $
				magicIf $$$
					( neq $$$
						lift (input :: TypedInput 4 a) $$$
						lift (input :: TypedInput 2 a)
					) $$$
					toLambda False $$$
				magicElseIf $$$
					( neq $$$
						lift (input :: TypedInput 3 b) $$$
						lift (input :: TypedInput 1 b)
					) $$$
					toLambda False $$$
				magicElse $$$
					toLambda True
			)
		)
instance (LambdaEq a, LambdaEq b, LambdaEq c) => LambdaEq (a,b,c) where
	eq = toCombinator $ abstract $ abstract $
		toFTuple3 (input :: TypedInput 2 (a,b,c)) $$$
		abstract (abstract $ abstract $
			toFTuple3 (lift (input :: TypedInput 4 (a,b,c))) $$$
			abstract (abstract $ abstract $
				magicIf $$$
					( neq $$$
						lift (input :: TypedInput 6 a) $$$
						lift (input :: TypedInput 3 a)
					) $$$
					toLambda False $$$
				magicElseIf $$$
					( neq $$$
						lift (input :: TypedInput 5 b) $$$
						lift (input :: TypedInput 2 b)
					) $$$
					toLambda False $$$
				magicElseIf $$$
					( neq $$$
						lift (input :: TypedInput 4 c) $$$
						lift (input :: TypedInput 1 c)
					) $$$
					toLambda False $$$
				magicElse $$$
					toLambda True
			)
		)
instance (LambdaEq a, LambdaEq b, LambdaEq c, LambdaEq d) =>
	LambdaEq (a,b,c,d) where
		eq = toCombinator $ abstract $ abstract $
			toFTuple4 (input :: TypedInput 2 (a,b,c,d)) $$$
			abstract (abstract $ abstract $ abstract $
				toFTuple4 (lift (input :: TypedInput 5 (a,b,c,d))) $$$
				abstract (abstract $ abstract $ abstract $
					magicIf $$$
						( neq $$$
							lift (input :: TypedInput 8 a) $$$
							lift (input :: TypedInput 4 a)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 7 b) $$$
							lift (input :: TypedInput 3 b)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 6 c) $$$
							lift (input :: TypedInput 2 c)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 5 d) $$$
							lift (input :: TypedInput 1 d)
						) $$$
						toLambda False $$$
					magicElse $$$
						toLambda True
				)
			)
instance (LambdaEq a, LambdaEq b, LambdaEq c, LambdaEq d, LambdaEq e) =>
	LambdaEq (a,b,c,d,e) where
		eq = toCombinator $ abstract $ abstract $
			toFTuple5 (input :: TypedInput 2 (a,b,c,d,e)) $$$
			abstract (abstract $ abstract $ abstract $ abstract $
				toFTuple5 (lift (input :: TypedInput 6 (a,b,c,d,e))) $$$
				abstract (abstract $ abstract $ abstract $ abstract $
					magicIf $$$
						( neq $$$
							lift (input :: TypedInput 10 a) $$$
							lift (input :: TypedInput 5 a)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 9 b) $$$
							lift (input :: TypedInput 4 b)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 8 c) $$$
							lift (input :: TypedInput 3 c)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 7 d) $$$
							lift (input :: TypedInput 2 d)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 6 e) $$$
							lift (input :: TypedInput 1 e)
						) $$$
						toLambda False $$$
					magicElse $$$
						toLambda True
				)
			)
instance (LambdaEq a, LambdaEq b, LambdaEq c, LambdaEq d, LambdaEq e
	, LambdaEq f) => LambdaEq (a,b,c,d,e,f) where
		eq = toCombinator $ abstract $ abstract $
			toFTuple6 (input :: TypedInput 2 (a,b,c,d,e,f)) $$$
			abstract (abstract $ abstract $ abstract $ abstract $ abstract $
				toFTuple6 (lift (input :: TypedInput 7 (a,b,c,d,e,f))) $$$
				abstract (abstract $ abstract $ abstract $ abstract $
				abstract $
					magicIf $$$
						( neq $$$
							lift (input :: TypedInput 12 a) $$$
							lift (input :: TypedInput 6 a)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 11 b) $$$
							lift (input :: TypedInput 5 b)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 10 c) $$$
							lift (input :: TypedInput 4 c)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 9 d) $$$
							lift (input :: TypedInput 3 d)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 8 e) $$$
							lift (input :: TypedInput 2 e)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 7 f) $$$
							lift (input :: TypedInput 1 f)
						) $$$
						toLambda False $$$
					magicElse $$$
						toLambda True
				)
			)
instance (LambdaEq a, LambdaEq b, LambdaEq c, LambdaEq d, LambdaEq e
	, LambdaEq f, LambdaEq g) => LambdaEq (a,b,c,d,e,f,g) where
		eq = toCombinator $ abstract $ abstract $
			toFTuple7 (input :: TypedInput 2 (a,b,c,d,e,f,g)) $$$
			abstract (abstract $ abstract $ abstract $ abstract $ abstract $
			abstract $
				toFTuple7 (lift (input :: TypedInput 8 (a,b,c,d,e,f,g))) $$$
				abstract (abstract $ abstract $ abstract $ abstract $
				abstract $ abstract $
					magicIf $$$
						( neq $$$
							lift (input :: TypedInput 14 a) $$$
							lift (input :: TypedInput 7 a)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 13 b) $$$
							lift (input :: TypedInput 6 b)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 12 c) $$$
							lift (input :: TypedInput 5 c)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 11 d) $$$
							lift (input :: TypedInput 4 d)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 10 e) $$$
							lift (input :: TypedInput 3 e)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 9 f) $$$
							lift (input :: TypedInput 2 f)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 8 g) $$$
							lift (input :: TypedInput 1 g)
						) $$$
						toLambda False $$$
					magicElse $$$
						toLambda True
				)
			)
instance (LambdaEq a, LambdaEq b, LambdaEq c, LambdaEq d, LambdaEq e
	, LambdaEq f, LambdaEq g, LambdaEq h) => LambdaEq (a,b,c,d,e,f,g,h) where
		eq = toCombinator $ abstract $ abstract $
			toFTuple8 (input :: TypedInput 2 (a,b,c,d,e,f,g,h)) $$$
			abstract (abstract $ abstract $ abstract $ abstract $ abstract $
			abstract $ abstract $
				toFTuple8 (lift (input :: TypedInput 9 (a,b,c,d,e,f,g,h))) $$$
				abstract (abstract $ abstract $ abstract $ abstract $
				abstract $ abstract $ abstract $
					magicIf $$$
						( neq $$$
							lift (input :: TypedInput 16 a) $$$
							lift (input :: TypedInput 8 a)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 15 b) $$$
							lift (input :: TypedInput 7 b)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 14 c) $$$
							lift (input :: TypedInput 6 c)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 13 d) $$$
							lift (input :: TypedInput 5 d)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 12 e) $$$
							lift (input :: TypedInput 4 e)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 11 f) $$$
							lift (input :: TypedInput 3 f)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 10 g) $$$
							lift (input :: TypedInput 2 g)
						) $$$
						toLambda False $$$
					magicElseIf $$$
						( neq $$$
							lift (input :: TypedInput 9 h) $$$
							lift (input :: TypedInput 1 h)
						) $$$
						toLambda False $$$
					magicElse $$$
						toLambda True
				)
			)

instance Representable () where
	toLambda () = fromFUnit unit where
		unit :: forall x. TypedCombinator (FUnit x)
		unit = toCombinator $ abstract (input :: TypedInput 1 x)
instance (Representable a, Representable b) => Representable (a,b) where
	toLambda (a,b) = fromFTuple2 tuple where
		tuple :: forall x. TypedCombinator (FTuple2 a b x)
		tuple = toCombinator $ abstract $
			(input :: TypedInput 1 (a -> b -> x)) $$$
			toLambda a $$$
			toLambda b
instance (Representable a, Representable b, Representable c) =>
	Representable (a,b,c) where
		toLambda (a,b,c) = fromFTuple3 tuple where
			tuple :: forall x. TypedCombinator (FTuple3 a b c x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1 (a -> b -> c -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c
instance (Representable a, Representable b, Representable c, Representable d)
	=> Representable (a,b,c,d) where
		toLambda (a,b,c,d) = fromFTuple4 tuple where
			tuple :: forall x. TypedCombinator (FTuple4 a b c d x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1 (a -> b -> c -> d -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c $$$
				toLambda d
instance (Representable a, Representable b, Representable c, Representable d
	, Representable e) => Representable (a,b,c,d,e) where
		toLambda (a,b,c,d,e) = fromFTuple5 tuple where
			tuple :: forall x. TypedCombinator (FTuple5 a b c d e x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1 (a -> b -> c -> d -> e -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c $$$
				toLambda d $$$
				toLambda e
instance (Representable a, Representable b, Representable c, Representable d
	, Representable e, Representable f) => Representable (a,b,c,d,e,f) where
		toLambda (a,b,c,d,e,f) = fromFTuple6 tuple where
			tuple :: forall x. TypedCombinator (FTuple6 a b c d e f x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1 (a -> b -> c -> d -> e -> f -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c $$$
				toLambda d $$$
				toLambda e $$$
				toLambda f
instance (Representable a, Representable b, Representable c, Representable d
	, Representable e, Representable f, Representable g) =>
	Representable (a,b,c,d,e,f,g) where
		toLambda (a,b,c,d,e,f,g) = fromFTuple7 tuple where
			tuple :: forall x. TypedCombinator (FTuple7 a b c d e f g x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1
					(a -> b -> c -> d -> e -> f -> g -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c $$$
				toLambda d $$$
				toLambda e $$$
				toLambda f $$$
				toLambda g
instance (Representable a, Representable b, Representable c, Representable d
	, Representable e, Representable f, Representable g, Representable h) =>
	Representable (a,b,c,d,e,f,g,h) where
		toLambda (a,b,c,d,e,f,g,h) = fromFTuple8 tuple where
			tuple :: forall x. TypedCombinator (FTuple8 a b c d e f g h x)
			tuple = toCombinator $ abstract $
				(input :: TypedInput 1
					(a -> b -> c -> d -> e -> f -> g -> h -> x)) $$$
				toLambda a $$$
				toLambda b $$$
				toLambda c $$$
				toLambda d $$$
				toLambda e $$$
				toLambda f $$$
				toLambda g $$$
				toLambda h

-- Constructors
mkTuple2 :: forall a b. TypedCombinator (a -> b -> (a,b))
mkTuple2 = toCombinator $ abstract $ abstract $ fromFTuple2 tuple where
	tuple :: forall x. TypedInput 2 (FTuple2 a b x)
	tuple = abstract $
		lift (input :: TypedInput 1 (a -> b -> x)) $$$
		(input :: TypedInput 3 a) $$$
		lift (input :: TypedInput 2 b)
mkTuple3 :: forall a b c. TypedCombinator (a -> b -> c -> (a,b,c))
mkTuple3 = toCombinator $ abstract $ abstract $ abstract $ fromFTuple3 tuple
	where
		tuple :: forall x. TypedInput 3 (FTuple3 a b c x)
		tuple = abstract $
			lift (input :: TypedInput 1 (a -> b -> c -> x)) $$$
			(input :: TypedInput 4 a) $$$
			lift (input :: TypedInput 3 b) $$$
			lift (input :: TypedInput 2 c)
mkTuple4 :: forall a b c d. TypedCombinator (a -> b -> c -> d -> (a,b,c,d))
mkTuple4 = toCombinator $ abstract $ abstract $ abstract $ abstract $
	fromFTuple4 tuple where
		tuple :: forall x. TypedInput 4 (FTuple4 a b c d x)
		tuple = abstract $
			lift (input :: TypedInput 1 (a -> b -> c -> d -> x)) $$$
			(input :: TypedInput 5 a) $$$
			lift (input :: TypedInput 4 b) $$$
			lift (input :: TypedInput 3 c) $$$
			lift (input :: TypedInput 2 d)
mkTuple5 :: forall a b c d e. TypedCombinator
	(a -> b -> c -> d -> e -> (a,b,c,d,e))
mkTuple5 = toCombinator $ abstract $ abstract $ abstract $ abstract $
	abstract $ fromFTuple5 tuple where
		tuple :: forall x. TypedInput 5 (FTuple5 a b c d e x)
		tuple = abstract $
			lift (input :: TypedInput 1 (a -> b -> c -> d -> e -> x)) $$$
			(input :: TypedInput 6 a) $$$
			lift (input :: TypedInput 5 b) $$$
			lift (input :: TypedInput 4 c) $$$
			lift (input :: TypedInput 3 d) $$$
			lift (input :: TypedInput 2 e)
mkTuple6 :: forall a b c d e f. TypedCombinator
	(a -> b -> c -> d -> e -> f -> (a,b,c,d,e,f))
mkTuple6 = toCombinator $ abstract $ abstract $ abstract $ abstract $
	abstract $ abstract $ fromFTuple6 tuple where
		tuple :: forall x. TypedInput 6 (FTuple6 a b c d e f x)
		tuple = abstract $
			lift (input :: TypedInput 1 (a -> b -> c -> d -> e -> f -> x)) $$$
			(input :: TypedInput 7 a) $$$
			lift (input :: TypedInput 6 b) $$$
			lift (input :: TypedInput 5 c) $$$
			lift (input :: TypedInput 4 d) $$$
			lift (input :: TypedInput 3 e) $$$
			lift (input :: TypedInput 2 f)
mkTuple7 :: forall a b c d e f g. TypedCombinator
	(a -> b -> c -> d -> e -> f -> g -> (a,b,c,d,e,f,g))
mkTuple7 = toCombinator $ abstract $ abstract $ abstract $ abstract $
	abstract $ abstract $ abstract $ fromFTuple7 tuple where
		tuple :: forall x. TypedInput 7 (FTuple7 a b c d e f g x)
		tuple = abstract $
			lift (input :: TypedInput 1
				(a -> b -> c -> d -> e -> f -> g -> x)) $$$
			(input :: TypedInput 8 a) $$$
			lift (input :: TypedInput 7 b) $$$
			lift (input :: TypedInput 6 c) $$$
			lift (input :: TypedInput 5 d) $$$
			lift (input :: TypedInput 4 e) $$$
			lift (input :: TypedInput 3 f) $$$
			lift (input :: TypedInput 2 g)
mkTuple8 :: forall a b c d e f g h. TypedCombinator
	(a -> b -> c -> d -> e -> f -> g -> h -> (a,b,c,d,e,f,g,h))
mkTuple8 = toCombinator $ abstract $ abstract $ abstract $ abstract $
	abstract $ abstract $ abstract $ abstract $ fromFTuple8 tuple where
		tuple :: forall x. TypedInput 8 (FTuple8 a b c d e f g h x)
		tuple = abstract $
			lift (input :: TypedInput 1
				(a -> b -> c -> d -> e -> f -> g -> h -> x)) $$$
			(input :: TypedInput 9 a) $$$
			lift (input :: TypedInput 8 b) $$$
			lift (input :: TypedInput 7 c) $$$
			lift (input :: TypedInput 6 d) $$$
			lift (input :: TypedInput 5 e) $$$
			lift (input :: TypedInput 4 f) $$$
			lift (input :: TypedInput 3 g) $$$
			lift (input :: TypedInput 2 h)

-- Currying
curry :: forall a b c. TypedCombinator (((a,b) -> c) -> a -> b -> c)
curry = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 ((a,b) -> c)) $$$
	(
		mkTuple2 $$$
		lift (input :: TypedInput 2 a) $$$
		lift (input :: TypedInput 1 b)
	)
uncurry :: forall a b c. TypedCombinator ((a -> b -> c) -> (a,b) -> c)
uncurry = toCombinator $ abstract $ abstract $
	toFTuple2 (lift (input :: TypedInput 1 (a,b))) $$$
	(input :: TypedInput 2 (a -> b -> c))
