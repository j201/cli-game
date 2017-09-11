module AreaGen where

import Types

import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear.V2
import Linear.V3
import Control.Monad.Random
import qualified Math.Noise as MN
import Linear.Vector ((^/))

genParams :: RandomGen g => Map (V2 Int) AreaInfo -> (V2 Int) -> Rand g AreaInfo
genParams ais l = return $ AreaInfo TemperateForest l

perlin :: RandomGen g => Rand g MN.Perlin
perlin = do r <- getRandom
            return $ MN.Perlin {
                       MN.perlinFrequency = 1.0,
                       MN.perlinLacunarity = 2.0,
                       MN.perlinOctaves = 6,
                       MN.perlinPersistence = 0.5,
                       MN.perlinSeed = r
                     }

perlinFull :: RandomGen g => MN.Perlin -> Rand g MN.Perlin
perlinFull p = do r <- getRandom
                  return $ p { MN.perlinSeed = r }

perlinAt :: Loc -> MN.Perlin -> Double
perlinAt l p = let (V3 x y z) = fmap fromIntegral l ^/ fromIntegral maxDim
               in case MN.getValue p (x,y,z)
                    of (Just d) -> d -- I'm a bad widdle boy

genArea :: RandomGen g => Map (V2 Int) AreaInfo -> (V2 Int) -> Rand g (AreaInfo, Area)
genArea ais l = do pCave <- perlin
                   pHeight <- perlin
                   ai <- genParams ais l
                   return $ (ai,
                             array (negate (V3 maxDim maxDim maxDim), V3 maxDim maxDim maxDim)
                                   [(V3 x y z, let topLevel = truncate (5 * perlinAt (V3 x y 0) pHeight)
                                                   nDirtBlocks = floor (2 * (perlinAt (V3 x y maxDim) pHeight + 1.0))
                                               in if z > topLevel then Air
                                                  else if z == topLevel then Grass
                                                  else if z >= topLevel - nDirtBlocks then Dirt
                                                  else if perlinAt (V3 x y z) pCave < (-0.4) then Air
                                                  else Stone) |
                                    x <- [-maxDim..maxDim],
                                    y <- [-maxDim..maxDim],
                                    z <- [-maxDim..maxDim]])

addArea :: RandomGen g => Game -> (V2 Int) -> Rand g Game
addArea = undefined
