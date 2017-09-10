{-# LANGUAGE TemplateHaskell #-}

module Game
where

import Types
import AreaGen

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as DS
import Control.Monad.Random

initGame :: RandomGen g => Rand g Game
initGame = do a <- genArea FixedParams
              return $ Game {
                           _loc = (V3 0 0 0),
                           _inventory = DS.empty,
                           _area = a,
                           _areaChanges = []
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

handleAction :: Action -> Game -> Game
handleAction (Move dir) g = over loc (+ dir) g
handleAction (PlaceBlock dir i) g = placeBlock dir i g
handleAction (RemoveBlock dir) g = removeBlock dir g

commitChanges :: Game -> Game
commitChanges g = if g^.areaChanges == []
                  then g
                  else set areaChanges [] $
                       over area (// (g^.areaChanges)) g
