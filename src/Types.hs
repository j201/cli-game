{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Array
import Linear.V3
import Data.Sequence (Seq, (|>), (<|), (><))
import Lens.Micro.Platform

type Dir = V3 Int
type Loc = V3 Int
type Area = Array Loc Block

data Block = Grass | Dirt | Stone | Bedrock | Air
    deriving (Eq, Show, Ord)

data Game = Game {
    _loc :: Loc,
    _inventory :: Seq (Block,Int),
    _area :: Area,
    _areaChanges :: [(Loc, Block)],
    _creative :: Bool
}

makeLenses ''Game

data Action =
    Move Dir |
    PlaceBlock Dir Int |
    RemoveBlock Dir |
    ToggleCreative

data FixedParams = FixedParams
data RandParams = RandParams

data AreaParams = AreaParams FixedParams RandParams

-- TODO: move into a 'consts' or 'settings' file?
maxDim :: Int
maxDim = 30
