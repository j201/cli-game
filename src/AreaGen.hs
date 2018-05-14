module AreaGen (genArea) where

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

randFromSeed :: Int -> Rand StdGen a -> a
randFromSeed s r = evalRand r $ mkStdGen s

-- Maps each area location to a distinct seed
areaSeed :: Int -> V2 Int -> Int
areaSeed seed l = seed + pair (toNat (l^._x)) (toNat (l^._y))
    where toNat x = if x < 0 then 2*(-x)-1 else 2*x
          pair x y = (x+y) * (x+y+1) `div` 2 + y

randBiome :: RandomGen g => Map (V2 Int) (AreaInfo, Area) -> (V2 Int) -> Rand g Biome
randBiome _ _ = fromList [(TemperateForest, 0.5), (BorealForest, 0.5)]

-- Module entry function
-- Determines the high-level characteristics of an area (biome, special
-- features), then calls genLandscape to actually generate/place everything as
-- needed
genArea :: Map (V2 Int) (AreaInfo, Area) -> (V2 Int) -> Int -> (AreaInfo, Area)
genArea ais l seed = randFromSeed (areaSeed seed l) randArea
    where randArea = do b <- randBiome ais l
                        let ai = AreaInfo b l
                        a <- genLandscape ai
                        return (ai,a)

genLandscape :: RandomGen g => AreaInfo -> Rand g Area
genLandscape (AreaInfo BorealForest _) = genForest [(Spruce, 0.8), (Birch, 0.2)] 0.1
genLandscape (AreaInfo TemperateForest _) = genForest [(Birch, 1)] 0.05

genForest :: RandomGen g => [(TreeType, Rational)] -> Double -> Rand g Area
genForest treeProbs density =
    do pCave <- perlin
       pHeight <- perlin
       let topLevel x y = truncate (5 * perlinAt (V3 x y 0) pHeight)
       tps <- treePlaces density 2.0
       ttypes <- replicateM (length tps) $ fromList treeProbs
       ts <- fmap (cleanFeature . concat) $
                  zipWithM (\(V2 x y) ttype -> fmap (shiftFeature (V3 x y (topLevel x y)))
                                                    (genTree ttype))
                           tps ttypes
       return $ array (negate (V3 maxDim maxDim maxDim), V3 maxDim maxDim maxDim)
                       [(V3 x y z, let nDirtBlocks = floor (2 * (perlinAt (V3 x y maxDim) pHeight + 1.0))
                                   in if z > topLevel x y then Air
                                      else if z == topLevel x y then Grass
                                      else if z >= topLevel x y - nDirtBlocks then Dirt
                                      else if perlinAt (V3 x y z) pCave < (-0.4) then Air
                                      else Stone) |
                        x <- [-maxDim..maxDim],
                        y <- [-maxDim..maxDim],
                        z <- [-maxDim..maxDim]]
                 // ts

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
genTree Spruce = do h <- getRandomR (2, 6)
                    let leafLocs z = if z < (h `div` 2) then [-2..2] else [-1..1]
                    return $ (V3 0 0 0, Tree Spruce Trunk) :
                             (V3 0 0 h, Tree Spruce Leaf) :
                             [(V3 x y z, Tree Spruce Leaf) | z <- [1..(h-1)], x <- leafLocs z, y <- leafLocs z] ++
                             map (\z -> (V3 0 0 z, Tree Birch Trunk)) [0..(h-1)]
