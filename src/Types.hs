{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear.V2
import Linear.V3
import Data.Sequence (Seq, (|>), (<|), (><))
import Lens.Micro.Platform

type Dir = V3 Int
type Loc = V3 Int

data Biome =
    HotDesert |
    DryShrubland |
    ArcticDesert |
    Tundra |
    TropicalForest |
    TropicalRainforest |
    TemperateForest |
    TemperateRainforest |
    BorealForest |
    TropicalGrassland |
    TemperateGrassland |
    TemperateWetland |
    TropicalWetland |
    Mountains

data TreePart = Trunk | Leaf | Root
    deriving (Eq, Show, Ord)

data TreeType =
    Birch |
    Elm |
    Maple |
    Oak
    deriving (Eq, Show, Ord)

data Block =
    Grass |
    Dirt |
    Stone |
    Bedrock |
    Air |
    Tree TreeType TreePart
    deriving (Eq, Show, Ord)

type Area = Array Loc Block

data AreaInfo = AreaInfo {
    _biome :: Biome,
    _areaLoc :: V2 Int
}

makeLenses ''AreaInfo

data Game = Game {
    _loc :: Loc,
    _inventory :: Seq (Block,Int),
    _area :: Area,
    _allAreas :: Map (V2 Int) AreaInfo,
    _currentArea :: AreaInfo,
    _areaChanges :: [(Loc, Block)],
    _creative :: Bool,
    _seed :: Int
}

makeLenses ''Game

data Action =
    Move Dir |
    PlaceBlock Dir Int |
    RemoveBlock Dir |
    ToggleCreative

-- TODO: move into a 'consts' or 'settings' file?
maxDim :: Int
maxDim = 30

lookMaxZDist :: Int
lookMaxZDist = 5
