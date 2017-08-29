{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty

import Control.Monad
import Data.IORef

data Tile = Dirt | Stone | Air
    deriving (Eq, Show)

data Game = Game {
    _player :: V3 Int,
    _world :: Array (V3 Int) Tile
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
initGame = Game (V3 0 0 0) initWorld

updateGame :: Event -> Game -> Game
updateGame (EvKey (KChar 'w') []) = over player (+ (V3 (-1) 0 0))
updateGame (EvKey (KChar 'a') []) = over player (+ (V3 0 (-1) 0))
updateGame (EvKey (KChar 's') []) = over player (+ (V3 1 0 0))
updateGame (EvKey (KChar 'd') []) = over player (+ (V3 0 1 0))
updateGame _ = id

tileImage :: Tile -> Image
tileImage Dirt = char (defAttr `withForeColor` red) '#'
tileImage Stone = char (defAttr `withForeColor` blue) '#'
tileImage Air = char defAttr ' '

playerImage = char (defAttr `withForeColor` white) '@'

gameImage :: Game -> V3 Int -> Image
gameImage g xyz = if xyz == g^.player
                  then playerImage
                  else let z = xyz^._z
                           w = g^.world
                           tile = w ! xyz
                       in if tile == Air && z > (-maxDim)
                          then tileImage (w ! (xyz + (V3 0 0 (-1))))
                          else tileImage tile

ui :: Game -> Picture
ui g = let z = view (player . _z) g
           w = g^.world
       in picForImage $ vertCat $ map horizCat $ [[gameImage g (V3 x y z) | y <- [-maxDim..maxDim]] | x <- [-maxDim..maxDim]]

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
