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
import Linear.Vector ((^*), (*^), (^+^), (^-^), negated)
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
    _dr :: DisplayRegion,
    _lastMove :: Dir
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
                         then set lastMove (keyDir c) $ act (Move (keyDir c))
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
                if ui^.game^.creative then "Creative" else "Non-creative",
                show $ ui^.game^.seed
            ]

-- The range of values to draw in one x or y dimension
drawRange :: Int -> Int -> [Int]
drawRange w x = let vision = (w-1) `div` 2
                in if x - vision < (-maxDim) then [-maxDim .. -maxDim + w - 1]
                   else if x - vision + w - 1 > maxDim then [maxDim - w + 1 .. maxDim]
                   else [x - vision .. x - vision + w - 1]

rayBlocks :: Loc -> Double -> Double -> [Loc]
rayBlocks origin theta phi = rayBlocks_ origin (V3 0 0 0) (V3 0 0 0)
    where rayBlocks_ :: Loc -> V3 Double -> V3 Double -> [Loc]
          rayBlocks_ block rayPos lastFace = let (blockPos, face) = nextPos rayPos lastFace
                                             in (block ^+^ fmap round face) : rayBlocks_ (block ^+^ fmap round face)
                                                                                         blockPos
                                                                                         (negated face)
          nextPos :: V3 Double -> V3 Double -> (V3 Double, V3 Double)
          nextPos pos lastFace = let changes = map (nextChange pos) allFaces
                                     validResults = filter (\(c,f) -> f /= lastFace &&
                                                                    all ((<= 0.5) . abs) (c ^+^ pos) &&
                                                                    correctQuadrant c) $
                                                           zip changes allFaces
                                     (change,face) = head validResults
                                 in (change ^+^ pos ^-^ face, face)
          nextChange :: V3 Double -> V3 Double -> V3 Double
          nextChange pos face | face^._x /= 0 = let x = 0.5 * face^._x - pos^._x
                                                    y = x * tan phi
                                                    z = sqrt (x*x + y*y) / tan theta
                                                in V3 x y z
                              | face^._y /= 0 = let y = 0.5 * face^._y - pos^._y
                                                    x = y / tan phi
                                                    z = sqrt (x*x + y*y) / tan theta
                                               in V3 x y z
                              | otherwise    = let z = 0.5 * face^._z - pos^._z
                                                   rxy = z * tan theta
                                                   y = rxy * sin phi
                                                   x = rxy * cos phi
                                               in V3 x y z
          correctQuadrant :: V3 Double -> Bool
          correctQuadrant pos = ((pos^._x >= 0) == (cos phi >= 0)) && ((pos^._y >= 0) == (sin phi >= 0))
          allFaces :: [V3 Double]
          allFaces = [V3 (-1) 0 0, V3 1 0 0, V3 0 (-1) 0, V3 0 1 0, V3 0 0 (-1), V3 0 0 1]

rayImage :: Area -> [Loc] -> Image
rayImage area r = rayImage_ (zip r (map (0.98 **) [0..]))
    where rayImage_ ((l,d):rs) = if not (inBounds area l)
                                 then blockImage Air
                                 else let b = area ! l
                                      in if b /= Air
                                         then blockImageWith (over (foreColor._lightness) (* d)) b
                                         else rayImage_ rs

vertFov = pi/3
horizFov = pi/2
vertFovOffset = pi*5/12

firstPersonView :: UIState -> Image
firstPersonView ui = let a = ui^.game^.area
                         centre = ui^.game^.loc
                         phiOffset = atan2 (fromIntegral (ui^.lastMove^._y))
                                           (fromIntegral (ui^.lastMove^._x))
                     in vertCat $ map horizCat [[rayImage a (rayBlocks centre theta phi) |
                                                 phi <- map (phiOffset +) $ angleRange horizFov 0 51] |
                                                theta <- angleRange vertFov vertFovOffset 25]
    where angleRange fov offset n = [fov * (x/(n-1) - 0.5) + offset | x <- [0..n-1]] :: [Double]

render :: UIState -> Picture
render ui = let g = ui^.game
                centre = if ui^.inputMode == Look
                         then ui^.lookLoc
                         else g^.loc
                (w,h) = ui^.dr
            in picForImage $
               (<|> (status ui <-> firstPersonView ui)) $
               vertCat $ map horizCat [[imageAt ui (V3 x y (centre^._z)) |
                                        x <- drawRange w (centre^._x)] |
                                       y <- reverse $ drawRange h (centre^._y)]

runGame :: Vty -> UIState -> IO ()
runGame vty ui = do
    update vty $ render ui
    e <- nextEvent vty
    case e of
        EvKey KEsc [] -> shutdown vty
        EvResize w h -> runGame vty $ set dr (min (2*maxDim+1) w, min (2*maxDim+1) h) ui
        _ -> runGame vty (handleEvent e ui)

initUIState :: Maybe Int -> DisplayRegion -> IO UIState
initUIState s dr = do seed <- case s of
                                Just s -> return s
                                Nothing -> getStdRandom random
                      let (w,h) = dr
                      return $ UIState {
                                   _selected = Nothing,
                                   _game = initGame seed,
                                   _inputMode = Normal,
                                   _lookLoc = V3 0 0 0,
                                   _dr = (min (2*maxDim+1) w, min (2*maxDim+1) h),
                                   _lastMove = V3 1 0 0
                               }

run = do
    setEnv "TERM" "xterm-256color"
    cfg <- standardIOConfig
    vty <- mkVty cfg
    dr <- outputForConfig cfg >>= displayBounds
    ui <- initUIState Nothing dr
    runGame vty ui
