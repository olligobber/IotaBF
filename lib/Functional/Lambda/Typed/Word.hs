{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.Word
	( Nibble
	, TByte
	, toTByte
	, fromTByte
	, showNibble
	, getNibbles
	, showByte
	) where

import Data.Word (Word8)
import Data.Bits (testBit, bit)
import Control.Monad (guard)
import ValidLiterals (valid)
import Prelude hiding (show, concat)

import Functional.Decode (Decode(..))
import Functional.Lambda.Typed
	( TypedLambda, TypedCombinator, TypedInput, Representable(..)
	, reType, input, liftInput, liftFree, ($$$), abstract, toCombinator
	)
import Functional.Lambda.Typed.Eq (LambdaEq(..))
import Functional.Lambda.Typed.Tuple
	(mkTuple2, mkTuple4, toFTuple2, toFTuple4, toFTuple8)
import Functional.Lambda.Typed.Render
	(LambdaShow(..), RenderS, TypedRenderS, concat)
import Functional.Iota.Free (IFree)
import Functional.Lambda.Typed.Bool (toFBool)

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

showNibble :: TypedLambda (Nibble -> RenderS) IFree
showNibble = abstract $
	toFTuple4 (liftInput (input :: TypedInput 1 Nibble)) $$$
	abstract (abstract $ abstract $ abstract $
		toFBool (liftInput (input :: TypedInput 4 Bool)) $$$
		(
			toFBool (liftInput (input :: TypedInput 3 Bool)) $$$
			(
				toFBool (liftInput (input :: TypedInput 2 Bool)) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "F") :: TypedRenderS) $$$
					liftFree ($$(valid "E") :: TypedRenderS)
				) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "D") :: TypedRenderS) $$$
					liftFree ($$(valid "C") :: TypedRenderS)
				)
			) $$$
				(
				toFBool (liftInput (input :: TypedInput 2 Bool)) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "B") :: TypedRenderS) $$$
					liftFree ($$(valid "A") :: TypedRenderS)
				) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "9") :: TypedRenderS) $$$
					liftFree ($$(valid "8") :: TypedRenderS)
				)
			)
		) $$$
		(
			toFBool (liftInput (input :: TypedInput 3 Bool)) $$$
			(
				toFBool (liftInput (input :: TypedInput 2 Bool)) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "7") :: TypedRenderS) $$$
					liftFree ($$(valid "6") :: TypedRenderS)
				) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "5") :: TypedRenderS) $$$
					liftFree ($$(valid "4") :: TypedRenderS)
				)
			) $$$
				(
				toFBool (liftInput (input :: TypedInput 2 Bool)) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "3") :: TypedRenderS) $$$
					liftFree ($$(valid "2") :: TypedRenderS)
				) $$$
				(
					toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
					liftFree ($$(valid "1") :: TypedRenderS) $$$
					liftFree ($$(valid "0") :: TypedRenderS)
				)
			)
		)
	)

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

-- Shows a byte as two hex digits
showByte :: TypedLambda (Word8 -> RenderS) IFree
showByte = abstract $
	toFTuple2 ( getNibbles $$$ liftInput (input :: TypedInput 1 Word8) ) $$$
	abstract (abstract $
		concat $$$
		(liftFree showNibble $$$ liftInput (input :: TypedInput 2 Nibble)) $$$
		(liftFree showNibble $$$ liftInput (input :: TypedInput 1 Nibble))
	)

instance LambdaShow Word8 where
	show = abstract $
		concat $$$
		liftFree ($$(valid "0x") :: TypedRenderS) $$$
		(liftFree showByte $$$ liftInput (input :: TypedInput 1 Word8))
