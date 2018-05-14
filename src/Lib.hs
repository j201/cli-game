{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Types
import Game

import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Linear.V2
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty
import qualified Data.Sequence as DS
import Control.Monad.Random
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import System.Environment (setEnv)

data InputMode = Normal | Remove | Place | Look
    deriving (Eq, Show)

data UIState = UIState {
    _game :: Game,
    _selected :: Maybe Int,
    _inputMode :: InputMode,
    _lookLoc :: Loc,
    _dr :: DisplayRegion
}

makeLenses ''UIState

isDirKey :: Char -> Bool
isDirKey c = c == 'w' || c == 'a' || c == 's' || c == 'd' || c == 'q' || c == 'e'

keyDir :: Char -> Dir
keyDir 'w' = V3 0 1 0
keyDir 'a' = V3 (-1) 0 0
keyDir 's' = V3 0 (-1) 0
keyDir 'd' = V3 1 0 0
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

isValidLook :: UIState -> Dir -> Bool
isValidLook ui dir = let playerZ = ui^.game^.loc^._z
                         newLoc = (ui^.lookLoc) + dir
                         lookZ = view _z $ newLoc
                     in inBounds (ui^.game^.area) newLoc &&
                        abs (playerZ - lookZ) <= lookMaxZDist

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
        (_,'c',[]) -> act ToggleCreative
        (Normal,'l',[]) -> set lookLoc (ui^.game^.loc) $
                           set inputMode Look ui
        (Normal,'r',[]) -> set inputMode Remove ui
        (Normal,'p',[]) -> set inputMode Place ui
        (Normal,_,[]) -> if isDirKey c
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
        (Look,'l',[]) -> set inputMode Normal ui
        (Look,_,[]) -> if isDirKey c && isValidLook ui (keyDir c)
                       then over lookLoc (+ keyDir c) ui
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
blockTile (Tree Spruce Leaf) = Tile (hsl 120 0.8 0.2) 'l'
blockTile (Tree _ Leaf) = Tile (hsl 120 0.8 0.4) 'l'
blockTile (Tree _ Trunk) = Tile (hsl 40 0.8 0.4) 'O'

blockImageWith :: (Tile -> Tile) -> Block -> Image
blockImageWith f b = let Tile rgb c = f $ blockTile b
                     in char (defAttr `withForeColor` (toVtyColor rgb)) c

blockImage :: Block -> Image
blockImage = blockImageWith id

playerImage = char (defAttr `withForeColor` (toVtyColor $ hsl 0 0.0 1.0)) '@'

lookImage = char (defAttr `withForeColor` (toVtyColor $ hsl 90 0.8 0.7)) '?'

imageAt :: UIState -> Loc -> Image
imageAt ui xyz = let g = ui^.game
                 in if ui^.inputMode == Look && xyz == ui^.lookLoc
                       then lookImage
                    else if xyz == g^.loc
                       then playerImage
                    else let a = g^.area
                             b = a ! xyz
                             b' = nextLowerBlock a xyz
                         in if b /= Air then blockImage b
                            else if b' /= Air then blockImageWith (over foreColor (over _lightness (* 0.7))) b'
                            else blockImage Air

status :: UIState -> Image
status ui = foldl1 (<->) $
            map (string defAttr) [
                show $ ui^.game^.loc,
                show $ ui^.lookLoc,
                show $ ui^.game^.inventory,
                show $ ui^.selected,
                show $ ui^.dr,
                show $ drawRange (ui^.dr^._2) (ui^.game^.loc^._y),
                if ui^.game^.creative then "Creative" else "Non-creative",
                show $ ui^.game^.seed
            ]

-- The range of values to draw in one x or y dimension
drawRange :: Int -> Int -> [Int]
drawRange w x = let vision = (w-1) `div` 2
                in if x - vision < (-maxDim) then [-maxDim .. -maxDim + w - 1]
                   else if x - vision + w - 1 > maxDim then [maxDim - w + 1 .. maxDim]
                   else [x - vision .. x - vision + w - 1]

render :: UIState -> Picture
render ui = let g = ui^.game
                centre = if ui^.inputMode == Look
                         then ui^.lookLoc
                         else g^.loc
                (w,h) = ui^.dr
            in picForImage $
               (<|> status ui) $
               (vertCat . reverse) $
               map horizCat $
               [[imageAt ui (V3 x y (centre^._z)) |
                 x <- drawRange w (centre^._x)] |
                y <- drawRange h (centre^._y)]

runGame :: Vty -> UIState -> IO ()
runGame vty ui = do
    update vty $ render ui
    e <- nextEvent vty
    case e of
        EvKey KEsc [] -> shutdown vty
        EvResize w h -> runGame vty $ set dr (min (2*maxDim+1) w, min (2*maxDim+1) h) ui
        _ -> runGame vty (handleEvent e ui)

initUIState :: DisplayRegion -> IO UIState
initUIState dr = do seed <- getStdRandom random
                    let (w,h) = dr
                    return $ UIState {
                                 _selected = Nothing,
                                 _game = initGame seed,
                                 _inputMode = Normal,
                                 _lookLoc = V3 0 0 0,
                                 _dr = (min (2*maxDim+1) w, min (2*maxDim+1) h)
                             }

run = do
    setEnv "TERM" "xterm-256color"
    cfg <- standardIOConfig
    vty <- mkVty cfg
    dr <- outputForConfig cfg >>= displayBounds
    ui <- initUIState dr
    runGame vty ui
