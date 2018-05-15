{-# LANGUAGE TemplateHaskell #-}

module Game
where

import Types
import AreaGen

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Array
import Linear.V2
import Linear.V3
import Lens.Micro.Platform
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as DS
import Control.Monad.Random

-- Area/Block utility functions

nextLowerBlock :: Area -> Loc -> Block
nextLowerBlock a xyz = if (xyz^._z) == -maxDim
                  then Bedrock
                  else a ! (xyz + (V3 0 0 (-1)))

nextLowerNonAirBlock :: Area -> Loc -> Block
nextLowerNonAirBlock a xyz = let b = a ! xyz
                                 z = xyz^._z
                             in if b /= Air then b
                                else if z == -maxDim then Bedrock
                                else nextLowerNonAirBlock a (xyz + (V3 0 0 (-1)))

-- Goes to the location that the player would end up at if they moved to this x/y loc from the given z level
topNonAirLoc :: Area -> Loc -> Maybe Loc
topNonAirLoc a l = let z = l^._z
                       z' = if a ! l == Air
                            then Just $ fromMaybe (-maxDim) $ find (\x -> a ! (set _z (x-1) l) /= Air) [z,z-1..(-maxDim)]
                            else find (\x -> a ! (set _z x l) == Air) [z+1..maxDim]
                   in fmap (\z -> set _z z l) z'

initGame :: Int -> Game
initGame seed = let (ai, a) = genArea Map.empty (V2 0 0) seed
                in Game {
                       _loc = case topNonAirLoc a (V3 0 0 0) of
                                (Just a) -> a,
                       _inventory = DS.empty,
                       _area = a,
                       _areaChanges = [],
                       _areaInfo = ai,
                       _allAreas = Map.empty,
                       _creative = False,
                       _seed = seed
                   }

inBounds :: Array Loc a -> Dir -> Bool
inBounds a xyz = let (lb,ub) = bounds a
                 in all (>= 0) (xyz - lb) &&
                    all (<= 0) (xyz - ub)

selectedItem :: Game -> Maybe Int -> Maybe Block
selectedItem g m = fmap (\i -> fst $ DS.index (g^.inventory) i) m

addItem :: Block -> Game -> Game
addItem b = over inventory (\s -> case DS.findIndexL ((== b) . fst) s of
                                    Just i -> DS.adjust (\(x,y) -> (x,y+1)) i s
                                    Nothing -> s |> (b,1))

-- Not added until containers-0.5.8.0, which isn't in Stackage
deleteAt :: Int -> Seq a -> Seq a
deleteAt i s = DS.take i s >< DS.drop (i+1) s

removeItemAt :: Int -> Game -> Game
removeItemAt i g = over inventory (\s -> if snd (DS.index s i) == 1
                                         then deleteAt i s
                                         else DS.adjust (\(x,y) -> (x,y-1)) i s)
                        g

removeItem :: Block -> Game -> Game
removeItem b g = case DS.findIndexL ((== b) . fst) (g^.inventory) of
                   Just i -> removeItemAt i g
                   Nothing -> error "Tried to remove item not in inventory!"

removeBlock :: Dir -> Game -> Game
removeBlock dir g = let xyz = g^.loc + dir
                        b = (g^.area) ! xyz
                    in if b /= Air
                       then over areaChanges ((xyz,Air):) $
                            addItem b g
                       else g

placeBlock :: Dir -> Int -> Game -> Game
placeBlock dir i g = let xyz = g^.loc + dir
                     in if ((g^.area) ! xyz) == Air
                        then over areaChanges ((xyz, fst $ DS.index (g^.inventory) i):) $
                             removeItemAt i g
                        else g

moveArea :: V2 Int -> Game -> Game
moveArea dir g = let newAreaLoc = g^.areaInfo^.areaLoc + dir
                     (newAreaInfo,newArea) =
                        Map.findWithDefault (genArea (g^.allAreas) newAreaLoc (g^.seed))
                                            newAreaLoc
                                            (g^.allAreas)
                     newX = if dir^._x == 0 then g^.loc^._x else -(dir^._x)*maxDim
                     newY = if dir^._y == 0 then g^.loc^._y else -(dir^._y)*maxDim
                     newLoc = case topNonAirLoc newArea (V3 newX newY 0) of
                                Just l -> l -- TODO: deal with bug here: what if all tiles from z=-maxDim to maxDim are occupied?
                  in g & area .~ newArea
                       & areaInfo .~ newAreaInfo
                       & loc .~ newLoc
                       & allAreas %~ Map.insert (g^.areaInfo^.areaLoc) (g^.areaInfo,g^.area)

handleAction :: Action -> Game -> Game
handleAction (Move dir) g = let l = g^.loc + dir
                            in if not $ inBounds (g^.area) l
                               then if dir^._z == 0
                                    then moveArea (fmap signum $ V2 (dir^._x) (dir^._y))
                                                  g
                                    else g
                               else if g^.creative
                               then set loc l g
                               else case topNonAirLoc (g^.area) l of
                                      Just l' -> if (l'^._z - l^._z) > 1
                                                 then g
                                                 else set loc l' g
                                      Nothing -> g
handleAction (PlaceBlock dir i) g = placeBlock dir i g
handleAction (RemoveBlock dir) g = removeBlock dir g & commitChanges & handleAction (Move 0) -- TODO: shows issue with commitChanges approach
handleAction ToggleCreative g = over creative not g

commitChanges :: Game -> Game
commitChanges g = if g^.areaChanges == []
                  then g
                  else set areaChanges [] $
                       over area (// (g^.areaChanges)) g
