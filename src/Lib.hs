{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Types
import Game

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty
import qualified Data.Sequence as DS
import Control.Monad.Random (evalRand, getStdGen)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import System.Environment (setEnv)

data InputMode = Normal | Remove | Place
    deriving (Eq, Show)

data UIState = UIState {
    _game :: Game,
    _selected :: Maybe Int,
    _inputMode :: InputMode
}

makeLenses ''UIState

isDirKey :: Char -> Bool
isDirKey c = c == 'w' || c == 'a' || c == 's' || c == 'd' || c == 'q' || c == 'e'

keyDir :: Char -> Dir
keyDir 'w' = V3 (-1) 0 0
keyDir 'a' = V3 0 (-1) 0
keyDir 's' = V3 1 0 0
keyDir 'd' = V3 0 1 0
keyDir 'q' = V3 0 0 (-1)
keyDir 'e' = V3 0 0 1

isValidDir :: Game -> Dir -> Bool
isValidDir g dir = inBounds (g^.area) (dir + (g^.loc))

correctSelected :: UIState -> UIState
correctSelected ui = let s = ui^.selected
                         inv = ui^.game^.inventory
                     in case s of
                          Just i -> if DS.null inv
                                    then set selected Nothing ui
                                    else if i >= DS.length inv
                                    then set selected (Just $ DS.length inv - 1) ui
                                    else ui
                          Nothing -> if not $ DS.null inv
                                     then set selected (Just 0) ui
                                     else ui

handleEvent :: Event -> UIState -> UIState
handleEvent (EvKey (KChar c) mods) ui =
    let g = ui^.game
        act a = over game (handleAction a) ui
    in over game commitChanges $ case (ui^.inputMode, c, mods) of
        (_,';',[]) -> case ui^.selected of
                        Just x -> set selected (Just $ (x-1) `mod` (DS.length (g^.inventory))) ui
                        Nothing -> ui
        (_,'\'',[]) -> case ui^.selected of
                        Just x -> set selected (Just $ (x+1) `mod` (DS.length (g^.inventory))) ui
                        Nothing -> ui
        (Normal,'r',[]) -> set inputMode Remove ui
        (Normal,'p',[]) -> set inputMode Place ui
        (Normal,_,[]) -> if isDirKey c && isValidDir g (keyDir c)
                         then act $ Move (keyDir c)
                         else ui
        (Remove,_,[]) -> set inputMode Normal $
                         if isDirKey c && isValidDir g (keyDir c)
                         then correctSelected $ act $ RemoveBlock (keyDir c)
                         else ui
        (Place,_,[]) -> set inputMode Normal $
                        if isDirKey c && isValidDir g (keyDir c)
                        then case ui^.selected of
                           Just i -> correctSelected $ act $ PlaceBlock (keyDir c) i
                           Nothing -> ui
                        else ui
        _ -> ui
handleEvent _ ui = ui

toVtyColor :: (RealFrac a, Num a) => RGB a -> Color
toVtyColor rgb = let rgb' = fmap (truncate . (* 255)) rgb
                 in uncurryRGB rgbColor rgb'

_hue :: (RealFrac a, Num a) => Lens' (RGB a) a
_hue = lens hue (\rgb h -> hsl h (saturation rgb) (lightness rgb))

_saturation :: (RealFrac a, Num a) => Lens' (RGB a) a
_saturation = lens saturation (\rgb s -> hsl (hue rgb) s (lightness rgb))

_lightness :: (RealFrac a, Num a) => Lens' (RGB a) a
_lightness = lens lightness (\rgb l -> hsl (hue rgb) (saturation rgb) l)

data Tile = Tile {
    _foreColor :: (RGB Double),
    _tileChar :: Char -- leaving out background, style for now
}

makeLenses ''Tile

blockTile :: Block -> Tile
blockTile Dirt = Tile (hsl 40 0.8 0.4) '#'
blockTile Stone = Tile (hsl 220 0.0 0.4) '#'
blockTile Bedrock = Tile (hsl 200 0.4 0.4) 'X'
blockTile Grass = Tile (hsl 130 0.8 0.4) 'i'
blockTile Air = Tile (hsl 0 0.0 0.0) ' '

blockImageWith :: (Tile -> Tile) -> Block -> Image
blockImageWith f b = let Tile rgb c = f $ blockTile b
                     in char (defAttr `withForeColor` (toVtyColor rgb)) c

blockImage :: Block -> Image
blockImage = blockImageWith id

playerImage = char (defAttr `withForeColor` white) '@'

nextLower :: Area -> Loc -> Block
nextLower a xyz = if (xyz^._z) == -maxDim
                  then Bedrock
                  else a ! (xyz + (V3 0 0 (-1)))

nextLowerNonAir :: Area -> Loc -> Block
nextLowerNonAir a xyz = let b = a ! xyz
                            z = xyz^._z
                        in if b /= Air then b
                           else if z == -maxDim then Bedrock
                           else nextLowerNonAir a (xyz + (V3 0 0 (-1)))

gameImageAt :: Game -> Loc -> Image
gameImageAt g xyz = if xyz == g^.loc
                    then playerImage
                    else let a = g^.area
                             b = a ! xyz
                             b' = nextLower a xyz
                         in if b /= Air then blockImage b
                            else if b' /= Air then blockImage b'
                            else blockImageWith (over foreColor (over _lightness (/ 2))) $ nextLowerNonAir a xyz

status :: UIState -> Image
status ui = string defAttr (show (ui^.game^.loc)) <->
            string defAttr (show (ui^.game^.inventory)) <->
            string defAttr (show (ui^.selected))

render :: UIState -> Picture
render ui = let g = ui^.game
                z = view (loc . _z) g
            in picForImage $ (<|> status ui) $ vertCat $ map horizCat $ [[gameImageAt g (V3 x y z) | y <- [-maxDim..maxDim]] | x <- [-maxDim..maxDim]]

runGame :: Vty -> UIState -> IO ()
runGame vty ui = do
    update vty (render ui)
    e <- nextEvent vty
    if e == EvKey KEsc []
    then shutdown vty
    else runGame vty (handleEvent e ui)

initUIState :: IO UIState
initUIState = do g <- getStdGen
                 return $ UIState {
                              _selected = Nothing,
                              _game = evalRand initGame g,
                              _inputMode = Normal
                          }

run = do
    setEnv "TERM" "xterm-256color"
    cfg <- standardIOConfig
    vty <- mkVty cfg
    ui <- initUIState
    runGame vty ui
