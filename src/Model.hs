
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Data.Map.Strict as M

-- CARTE
import Carte (Carte)
import Carte (Coord)
import Carte (Case)
import qualified Carte as C

-- MONSTERS
import Monster (Monstre)
import qualified Monster as Mst

-- OUTILS
import Outils (Outil)
import Outils (Type)
import qualified Outils as O


data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , epee :: Bool
                           , clef :: Bool
                           , speed :: Int 
                           , monstres :: [Mst.Monstre]
                           , outils :: (M.Map Coord Outil)
                           , carte :: Carte}

-- Initialise l'état du jeu
initGameState :: Carte -> GameState
initGameState carte = GameState 350 250 False False 50 (Mst.initMonstres 1) (O.initOutils [("epee",(200,200))]) carte


-----------------------------
-- *** DELACEMENT HERO *** --
-----------------------------
-- Deplace le perso vers la gauche
moveLeft :: GameState -> GameState
moveLeft gs@(GameState px py _ _ sp _ _ (C.Carte l h contenue)) | px > 0 && (C.caseAccesible (px - sp) py contenue) = gs { persoX = px - sp }
                                | otherwise = gs

-- Deplace le perso vers la droite
moveRight :: GameState -> GameState
moveRight gs@(GameState px py _  _ sp _ _ (C.Carte l h contenue)) | px < l && (C.caseAccesible (px + sp) py contenue)= gs { persoX = px + sp }
                                 | otherwise = gs

-- Deplace le perso vers le haut                           
moveUp :: GameState -> GameState
moveUp gs@(GameState px py _ _ sp _ _ (C.Carte l h contenue)) | py > 0 && (C.caseAccesible px (py - sp) contenue)= gs { persoY = py - sp }
                              | otherwise = gs

-- Deplace le perso vers le bas
moveDown :: GameState -> GameState
moveDown gs@(GameState px py _ _ sp _ _ (C.Carte l h contenue)) | py < h && (C.caseAccesible px (py + sp) contenue) = gs { persoY = py + sp }
                                | otherwise = gs




------------------------------
-- *** AUTRE DELACEMENT *** --
------------------------------
changeOutils :: GameState -> GameState
changeOutils gs@(GameState px py e _ _ _ outils _) | (O.isSword px py outils || O.isKey px py outils) = let (O.Outil id aff) = M.findWithDefault (O.Outil O.ErrorOutil False) (C.C px py) outils in case id of
                                                                                                                                                    O.Epee -> let o = (O.changeOutils (C.C px py) O.Epee outils) in gs { epee = True, outils = o }
                                                                                                                                                    O.Clef -> let o = (O.changeOutils (C.C px py) O.Clef outils) in gs { clef = True, outils = o }
                                                                                                                                                    otherwise -> gs
                                            |otherwise = gs


changeMonstres :: GameState -> GameState
changeMonstres gs@(GameState px py True _ _ monstres _ _) | Mst.testeMonstres px py monstres = let m = (Mst.elimineMonstres px py monstres) in gs { monstres = m, epee = False }
                                                          | otherwise = gs
changeMonstres gs@(GameState px py False _ _ monstres _ c) |Mst.testeMonstres px py monstres =  initGameState c
                                                           |otherwise = gs

-- Deplace les monstres lorsque le compteurs est à 0
moveMonsters :: Int -> GameState -> GameState
moveMonsters 0 gs@(GameState _ _ _ _ _ liste _ _) = gs { monstres = Mst.moveAllMonster liste}
moveMonsters _ gs = gs




--------------------------------
-- *** DELACEMENT GENERAL *** --
--------------------------------
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = getEvent gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD]

-- Gere l'événement liée au clavier
getEvent :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
getEvent gstate keyb deltaTime (x : []) = if K.keypressed x keyb then changeMonstres (moveTo gstate x deltaTime) else (changeMonstres gstate)
getEvent gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then getEvent (changeMonstres (moveTo gstate x deltaTime)) keyb deltaTime xs
                                                                 else getEvent (changeMonstres gstate) keyb deltaTime xs 

-- Repercution de l'évement sur les elements de la gamestate
moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime =  changeOutils (moveUp gstate)
moveTo gstate KeycodeQ deltaTime =  changeOutils (moveLeft gstate)
moveTo gstate KeycodeS deltaTime =  changeOutils (moveDown gstate)
moveTo gstate KeycodeD deltaTime =  changeOutils (moveRight gstate)
moveTo gstate _ _ = gstate
