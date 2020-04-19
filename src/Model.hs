
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Data.Map.Strict as M
import Carte (Carte)
import Carte (Coord)
import Carte (Case)
import qualified Carte as C

data Espece = 
    Orc

data Monstre = M {espece :: Espece, coor :: Coord}

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int 
                           , monstres :: [Monstre]
                           , carte :: Carte}

especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"

testeCoord :: Int -> Int -> M.Map Coord Case -> Bool
testeCoord x y map = case M.lookup (C.C x y) map of
    Just C.Vide -> True
    Just C.PorteEOO -> True
    Just C.PorteNSO -> True
    otherwise -> False

initMonstres ::Int -> [Monstre]
initMonstres 0 = []
initMonstres x = (M Orc (C.C 250 250)): (initMonstres (x - 1))

initGameState :: Carte -> GameState
initGameState carte = GameState 350 250 50 (initMonstres 1) carte

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px py sp _ (C.Carte l h contenue)) | px > 0 && (testeCoord (px - sp) py contenue) = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px py sp _ (C.Carte l h contenue)) | px < l && (testeCoord (px + sp) py contenue)= gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState px py sp _ (C.Carte l h contenue)) | py > 0 && (testeCoord px (py - sp) contenue)= gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState px py sp _ (C.Carte l h contenue)) | py < h && (testeCoord px (py + sp) contenue) = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = moveAux gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD]


moveAux :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
moveAux gstate keyb deltaTime (x : []) = if K.keypressed x keyb then moveTo gstate x deltaTime else gstate
moveAux gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then moveAux (moveTo gstate x deltaTime) keyb deltaTime xs
                                                                 else moveAux gstate keyb deltaTime xs 



moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime = moveUp gstate
moveTo gstate KeycodeQ deltaTime = moveLeft gstate
moveTo gstate KeycodeS deltaTime = moveDown gstate
moveTo gstate KeycodeD deltaTime = moveRight gstate
moveTo gstate _ _ = gstate


