{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Word
	( Nibble
	, TByte
	, toTByte
	, fromTByte
	, getNibbles
	) where

import Data.Word (Word8)
import Data.Bits (testBit, bit)
import Control.Monad (guard)

import Functional.Decode (Decode(..))
import Functional.Lambda.Typed
	( TypedLambda, TypedCombinator, TypedInput, Representable(..)
	, reType, input, liftInput, ($$$), abstract, toCombinator
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Lambda.Typed.Tuple (mkTuple2, mkTuple4, toFTuple8)

type Nibble = (Bool,Bool,Bool,Bool)
type TByte = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)

toTByte :: TypedLambda Word8 v -> TypedLambda TByte v
toTByte = reType

fromTByte :: TypedLambda TByte v -> TypedLambda Word8 v
fromTByte = reType

instance LambdaEq Word8 where
	eq = reType (eq :: TypedCombinator (TByte -> TByte -> Bool))

instance Representable Word8 where
	toLambda w = fromTByte $ toLambda
		( testBit w 7
		, testBit w 6
		, testBit w 5
		, testBit w 4
		, testBit w 3
		, testBit w 2
		, testBit w 1
		, testBit w 0
		)

instance Decode Word8 where
	decodeLambda lambda = case decodeLambda lambda of
		Nothing -> Nothing
		Just (a,b,c,d,e,f,g,h) -> Just $ sum $ do
			(bool,index) <- zip [a,b,c,d,e,f,g,h] [7,6..]
			guard bool
			pure $ bit index

getNibbles :: TypedCombinator (Word8 -> (Nibble, Nibble))
getNibbles = toCombinator $ abstract $
	toFTuple8 (toTByte (input :: TypedInput 1 Word8)) $$$
	abstract ( abstract $ abstract $ abstract $ abstract $ abstract $ abstract
		$ abstract $ mkTuple2 $$$
		(mkTuple4 $$$
			liftInput (input :: TypedInput 8 Bool) $$$
			liftInput (input :: TypedInput 7 Bool) $$$
			liftInput (input :: TypedInput 6 Bool) $$$
			liftInput (input :: TypedInput 5 Bool)
		) $$$
		(mkTuple4 $$$
			liftInput (input :: TypedInput 4 Bool) $$$
			liftInput (input :: TypedInput 3 Bool) $$$
			liftInput (input :: TypedInput 2 Bool) $$$
			liftInput (input :: TypedInput 1 Bool)
		)
	)
