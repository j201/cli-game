{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

import Control.Monad
import Data.IORef

data Tile = Dirt | Stone | Air
    deriving (Eq, Show, Ord)

data InputMode = Normal | Remove | Place
    deriving (Eq, Show)

data Game = Game {
    _loc :: V3 Int,
    _inventory :: MultiSet Tile,
    _selected :: Maybe Tile,
    _world :: Array (V3 Int) Tile,
    _inputMode :: InputMode,
    _worldChanges :: [(V3 Int, Tile)]
}

makeLenses ''Game

maxDim = 10

initWorld :: Array (V3 Int) Tile
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
    _inventory = MS.empty,
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

removeTile :: V3 Int -> Game -> Game
removeTile dir g = let xyz = g^.loc + dir
                       t = (g^.world) ! xyz
                   in if t /= Air
                      then over worldChanges ((xyz,Air):) $
                           over inventory (MS.insert t) g
                      else g

placeTile :: V3 Int -> Game -> Game
placeTile dir g = let xyz = g^.loc + dir
                  in case g^.selected of
                      (Just t) -> if ((g^.world) ! xyz) == Air && MS.member t (g^.inventory)
                                  then over worldChanges ((xyz,t):) $
                                      over inventory (MS.delete t) g
                                  else g
                      _ -> g

commitChanges :: Game -> Game
commitChanges g = set worldChanges [] $
                  over world (// (g^.worldChanges)) g

updateGame :: Event -> Game -> Game
updateGame (EvKey (KChar c) mods) g =
    commitChanges $ case g^.inputMode of
        Normal -> case (c,mods) of
            ('r',[]) -> set inputMode Remove g
            ('p',[]) -> set inputMode Place g
            (c, []) -> if isDirKey c
                       then over loc (+ keyDir c) g
                       else g
            _ -> g
        Remove -> if mods == [] && isDirKey c
                  then removeTile (keyDir c) (set inputMode Normal g)
                  else (set inputMode Normal g)
        Place -> if mods == [] && isDirKey c
                 then placeTile (keyDir c) (set inputMode Normal g)
                 else (set inputMode Normal g)
updateGame _ g = g

tileImage :: Tile -> Image
tileImage Dirt = char (defAttr `withForeColor` red) '#'
tileImage Stone = char (defAttr `withForeColor` blue) '#'
tileImage Air = char defAttr ' '

playerImage = char (defAttr `withForeColor` white) '@'

gameImage :: Game -> V3 Int -> Image
gameImage g xyz = if xyz == g^.loc
                  then playerImage
                  else let z = xyz^._z
                           w = g^.world
                           tile = w ! xyz
                       in if tile == Air && z > (-maxDim)
                          then tileImage (w ! (xyz + (V3 0 0 (-1))))
                          else tileImage tile

status :: Game -> Image
status g = string defAttr (show (g^.loc)) <-> string defAttr (show (MS.elems (g^.inventory)))

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
