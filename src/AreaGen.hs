module AreaGen where

import Types

import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear.Metric (norm)
import Linear.V2
import Linear.V3
import Control.Monad.Random
import qualified Math.Noise as MN
import Linear.Vector ((^/))
import Lens.Micro.Platform

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
                   let topLevel x y = truncate (5 * perlinAt (V3 x y 0) pHeight)
                   ai <- genParams ais l
                   tps <- treePlaces 0.01 2.0
                   ts <- fmap (cleanFeature . concat) $ mapM (\(V2 x y) -> fmap (shiftFeature (V3 x y (topLevel x y)))
                                                                                (genTree Birch))
                                                             tps
                   return $ (ai,
                             array (negate (V3 maxDim maxDim maxDim), V3 maxDim maxDim maxDim)
                                   [(V3 x y z, let nDirtBlocks = floor (2 * (perlinAt (V3 x y maxDim) pHeight + 1.0))
                                               in if z > topLevel x y then Air
                                                  else if z == topLevel x y then Grass
                                                  else if z >= topLevel x y - nDirtBlocks then Dirt
                                                  else if perlinAt (V3 x y z) pCave < (-0.4) then Air
                                                  else Stone) |
                                    x <- [-maxDim..maxDim],
                                    y <- [-maxDim..maxDim],
                                    z <- [-maxDim..maxDim]]
                             // ts)

addArea :: RandomGen g => Game -> (V2 Int) -> Rand g Game
addArea = undefined

-- TODO: naïve implementation - O(n²)
-- TODO: could run infinitely
-- density in trees/tile
treePlaces :: RandomGen g => Double -> Double -> Rand g [V2 Int]
treePlaces density minDist = treePlaces' (floor $ density * fromIntegral (maxDim*maxDim)) minDist (return [])
    where treePlaces' :: RandomGen g => Int -> Double -> Rand g [V2 Int] -> Rand g [V2 Int]
          treePlaces' 0 md ts = ts
          treePlaces' n md ts = do x <- getRandomR (-maxDim,maxDim)
                                   y <- getRandomR (-maxDim,maxDim)
                                   ts' <- ts
                                   if any (\t -> (norm $ fmap fromIntegral $ V2 x y - t) < minDist) ts'
                                   then treePlaces' n md ts
                                   else treePlaces' (n-1) md (return $ V2 x y : ts' )

shiftFeature :: Loc -> [(Loc, Block)] -> [(Loc, Block)]
shiftFeature l = map (\(l',b) -> (l'+l,b))

cleanFeature :: [(Loc, Block)] -> [(Loc, Block)]
cleanFeature = filter (\(l,_) -> (l^._x) >= (-maxDim) && (l^._x) <= maxDim &&
                                 (l^._y) >= (-maxDim) && (l^._y) <= maxDim &&
                                 (l^._z) >= (-maxDim) && (l^._z) <= maxDim)

genTree :: RandomGen g => TreeType -> Rand g [(Loc, Block)]
genTree Birch = do h <- getRandomR (2, 6)
                   return $ (V3 0 0 0, Tree Birch Trunk) :
                            (V3 0 0 h, Tree Birch Leaf) :
                            [(V3 x y z, Tree Birch Leaf) | x <- [-1..1], y <- [-1..1], z <- [1..(h-1)]] ++
                            map (\z -> (V3 0 0 z, Tree Birch Trunk)) [0..(h-1)]
