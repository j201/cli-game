{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as DS

import Control.Monad
import Data.IORef

data Block = Dirt | Stone | Air
    deriving (Eq, Show, Ord)

data InputMode = Normal | Remove | Place
    deriving (Eq, Show)

data Game = Game {
    _loc :: V3 Int,
    _inventory :: Seq (Block,Int),
    _selected :: Maybe Int,
    _world :: Array (V3 Int) Block,
    _inputMode :: InputMode,
    _worldChanges :: [(V3 Int, Block)]
}

makeLenses ''Game

maxDim = 10

initWorld :: Array (V3 Int) Block
initWorld = array (negate (V3 maxDim maxDim maxDim), V3 maxDim maxDim maxDim)
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
    _selected = Nothing,
    _world = initWorld,
    _inputMode = Normal,
    _worldChanges = []
}

isDirKey :: Char -> Bool
isDirKey c = c == 'w' || c == 'a' || c == 's' || c == 'd' || c == 'q' || c == 'e'

keyDir :: Char -> V3 Int
keyDir 'w' = V3 (-1) 0 0
keyDir 'a' = V3 0 (-1) 0
keyDir 's' = V3 1 0 0
keyDir 'd' = V3 0 1 0
keyDir 'q' = V3 0 0 (-1)
keyDir 'e' = V3 0 0 1

inBounds :: Array (V3 Int) a -> V3 Int -> Bool
inBounds a xyz = let (lb,ub) = bounds a
                 in all (>= 0) (xyz - lb) &&
                    all (<= 0) (xyz - ub)

selectedItem :: Game -> Maybe Block
selectedItem g = fmap (\i -> fst $ DS.index (g^.inventory) i) (g^.selected)

addItem :: Block -> Game -> Game
addItem b = over inventory (\s -> case DS.findIndexL ((== b) . fst) s of
                                    Just i -> DS.adjust (\(x,y) -> (x,y+1)) i s
                                    Nothing -> s |> (b,1)) .
            over selected (\m -> if m == Nothing then Just 0 else m)

-- Not added until containers-0.5.8.0, which isn't in Stackage
deleteAt :: Int -> Seq a -> Seq a
deleteAt i s = DS.take i s >< DS.drop (i+1) s

removeItemAt :: Int -> Game -> Game
removeItemAt i g = let g' = over inventory (\s -> if snd (DS.index s i) == 1
                                                  then deleteAt i s
                                                  else DS.adjust (\(x,y) -> (x,y-1)) i s)
                                 g
                   in over selected (\m -> case m of
                                           Just i -> if DS.null (g'^.inventory)
                                                         then Nothing
                                                     else if i >= DS.length (g'^.inventory)
                                                         then Just (i-1)
                                                     else m
                                           _ -> m)
                           g'

removeItem :: Block -> Game -> Game
removeItem b g = case DS.findIndexL ((== b) . fst) (g^.inventory) of
                   Just i -> removeItemAt i g
                   Nothing -> error "Tried to remove item not in inventory!"

removeBlock :: V3 Int -> Game -> Game
removeBlock dir g = let xyz = g^.loc + dir
                        b = (g^.world) ! xyz
                    in if b /= Air
                       then over worldChanges ((xyz,Air):) $
                            addItem b g
                       else g

placeBlock :: V3 Int -> Game -> Game
placeBlock dir g = let xyz = g^.loc + dir
                   in case g^.selected of
                       (Just i) -> if ((g^.world) ! xyz) == Air
                                   then over worldChanges ((xyz, fst $ DS.index (g^.inventory) i):) $
                                        removeItemAt i g
                                   else g
                       _ -> g

isValidDir :: Game -> V3 Int -> Bool
isValidDir g dir = inBounds (g^.world) (dir + (g^.loc))

commitChanges :: Game -> Game
commitChanges g = set worldChanges [] $
                  over world (// (g^.worldChanges)) g

updateGame :: Event -> Game -> Game
updateGame (EvKey (KChar c) mods) g =
    commitChanges $ case (g^.inputMode, c, mods) of
        (_,';',[]) -> over selected (fmap (\x -> (x-1) `mod` (DS.length (g^.inventory)))) g
        (_,'\'',[]) -> over selected (fmap (\x -> (x+1) `mod` (DS.length (g^.inventory)))) g
        (Normal,'r',[]) -> set inputMode Remove g
        (Normal,'p',[]) -> set inputMode Place g
        (Normal,_,[]) -> if isDirKey c && isValidDir g (keyDir c)
                         then over loc (+ keyDir c) g
                         else g
        (Remove,_,[]) -> if mods == [] && isDirKey c && isValidDir g (keyDir c)
                         then removeBlock (keyDir c) (set inputMode Normal g)
                         else (set inputMode Normal g)
        (Place,_,[]) -> if mods == [] && isDirKey c && isValidDir g (keyDir c)
                        then placeBlock (keyDir c) (set inputMode Normal g)
                        else (set inputMode Normal g)
updateGame _ g = g

blockImage :: Block -> Image
blockImage Dirt = char (defAttr `withForeColor` red) '#'
blockImage Stone = char (defAttr `withForeColor` blue) '#'
blockImage Air = char defAttr ' '

playerImage = char (defAttr `withForeColor` white) '@'

gameImage :: Game -> V3 Int -> Image
gameImage g xyz = if xyz == g^.loc
                  then playerImage
                  else let z = xyz^._z
                           w = g^.world
                           block = w ! xyz
                       in if block == Air && z > (-maxDim)
                          then blockImage (w ! (xyz + (V3 0 0 (-1))))
                          else blockImage block

status :: Game -> Image
status g = string defAttr (show (g^.loc)) <->
           string defAttr (show (g^.inventory)) <->
           string defAttr (show (g^.selected))

ui :: Game -> Picture
ui g = let z = view (loc . _z) g
           w = g^.world
       in picForImage $ (<|> status g) $ vertCat $ map horizCat $ [[gameImage g (V3 x y z) | y <- [-maxDim..maxDim]] | x <- [-maxDim..maxDim]]

runGame :: Vty -> Game -> IO ()
runGame vty g = do
    update vty (ui g)
    e <- nextEvent vty
    if e == EvKey KEsc []
    then shutdown vty
    else runGame vty (updateGame e g)

run = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    runGame vty initGame
