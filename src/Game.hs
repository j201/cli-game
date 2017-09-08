{-# LANGUAGE TemplateHaskell #-}

module Game
where

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as DS

type Dir = V3 Int
type Loc = V3 Int

data Block = Dirt | Stone | Air
    deriving (Eq, Show, Ord)

data Game = Game {
    _loc :: Loc,
    _inventory :: Seq (Block,Int),
    _area :: Array Loc Block,
    _areaChanges :: [(Loc, Block)]
}

makeLenses ''Game

maxDim = 10

initArea :: Array Loc Block
initArea = array (negate (V3 maxDim maxDim maxDim), V3 maxDim maxDim maxDim)
                  [(V3 x y z, if z >= 0 then Air
                              else if even (x + y) then Dirt
                              else Stone) |
                   x <- [-maxDim..maxDim],
                   y <- [-maxDim..maxDim],
                   z <- [-maxDim..maxDim]]

initGame :: Game
initGame = Game {
    _loc = (V3 0 0 0),
    _inventory = DS.empty,
    _area = initArea,
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

data Action =
    Move Dir |
    PlaceBlock Dir Int |
    RemoveBlock Dir

handleAction :: Action -> Game -> Game
handleAction (Move dir) g = over loc (+ dir) g
handleAction (PlaceBlock dir i) g = placeBlock dir i g
handleAction (RemoveBlock dir) g = removeBlock dir g

commitChanges :: Game -> Game
commitChanges g = if g^.areaChanges == []
                  then g
                  else set areaChanges [] $
                       over area (// (g^.areaChanges)) g
