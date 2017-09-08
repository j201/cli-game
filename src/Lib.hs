{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Game

import Data.Array
import Linear.V3
import Lens.Micro.Platform
import Graphics.Vty
import qualified Data.Sequence as DS

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

blockImage :: Block -> Image
blockImage Dirt = char (defAttr `withForeColor` red) '#'
blockImage Stone = char (defAttr `withForeColor` blue) '#'
blockImage Air = char defAttr ' '

playerImage = char (defAttr `withForeColor` white) '@'

gameImageAt :: Game -> Loc -> Image
gameImageAt g xyz = if xyz == g^.loc
                    then playerImage
                    else let z = xyz^._z
                             w = g^.area
                             block = w ! xyz
                         in if block == Air && z > (-maxDim)
                            then blockImage (w ! (xyz + (V3 0 0 (-1))))
                            else blockImage block

status :: UIState -> Image
status ui = string defAttr (show (ui^.game^.loc)) <->
            string defAttr (show (ui^.game^.inventory)) <->
            string defAttr (show (ui^.selected))

render :: UIState -> Picture
render ui = let g = ui^.game
                z = view (loc . _z) g
                w = g^.area
            in picForImage $ (<|> status ui) $ vertCat $ map horizCat $ [[gameImageAt g (V3 x y z) | y <- [-maxDim..maxDim]] | x <- [-maxDim..maxDim]]

runGame :: Vty -> UIState -> IO ()
runGame vty ui = do
    update vty (render ui)
    e <- nextEvent vty
    if e == EvKey KEsc []
    then shutdown vty
    else runGame vty (handleEvent e ui)

initUIState = UIState {
    _selected = Nothing,
    _game = initGame,
    _inputMode = Normal
}

run = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    runGame vty initUIState
