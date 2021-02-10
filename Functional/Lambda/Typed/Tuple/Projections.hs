{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Functional.Lambda.Typed.Tuple.Projections where

import Functional.Lambda.Typed.Tuple.ProjectionsTemplate (generateProjections)
import Functional.Lambda.Typed.Tuple
	( toFTuple2, toFTuple3, toFTuple4, toFTuple5, toFTuple6, toFTuple7
	, toFTuple8)

generateProjections 8
