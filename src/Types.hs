{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Array
import Linear.V3
import Data.Sequence (Seq, (|>), (<|), (><))
import Lens.Micro.Platform

type Dir = V3 Int
type Loc = V3 Int

data Block = Dirt | Stone | Bedrock | Air
    deriving (Eq, Show, Ord)

data Game = Game {
    _loc :: Loc,
    _inventory :: Seq (Block,Int),
    _area :: Array Loc Block,
    _areaChanges :: [(Loc, Block)]
}

makeLenses ''Game

data Action =
    Move Dir |
    PlaceBlock Dir Int |
    RemoveBlock Dir
