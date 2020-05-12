
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

-- ITEMS
import Items (Item)
import Items (Type)
import qualified Items as I
import Debug.Trace as T


data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , epee :: Bool
                           , clef :: Bool
                           , speed :: Int 
                           , monstres :: [Mst.Monstre] --Liste des monstres du gamestate
                           , items :: (M.Map Coord Item) --Emplacement des Items
                           , iniCarte :: Carte --CarteInitiale dans le cas ou on reset celle-ci sera récupérer
                           , carte :: Carte --Carte courante du gamestate
                           }

-- Initialise l'état du jeu
initGameState :: Carte -> GameState
initGameState (C.Carte larg haut contenu) = GameState 350 250 False False 50 (Mst.initMonstres [((250,250),"Orc"),((1150,500),"Skeleton"),((50,50),"Fantome")]) (I.initItems [("epee",(200,200)),("clef",(300,300))]) (C.Carte larg haut contenu) (C.Carte larg haut contenu)


-----------------------------
-- *** DELACEMENT HERO *** --
-----------------------------
-- Deplace le perso vers la gauche
moveLeft :: GameState -> GameState
moveLeft gs@(GameState px py _ _ sp _ _ _ carte) | px > 0 && (C.caseAccesible (px - sp) py carte) = gs { persoX = px - sp }
                                | otherwise = gs

-- Deplace le perso vers la droite
moveRight :: GameState -> GameState
moveRight gs@(GameState px py _  _ sp _ _ _ (C.Carte l h map)) | px < l && (C.caseAccesible (px + sp) py (C.Carte l h map))= gs { persoX = px + sp }
                                 | otherwise = gs

-- Deplace le perso vers le haut                           
moveUp :: GameState -> GameState
moveUp gs@(GameState px py _ _ sp _ _ _ carte) | py > 0 && (C.caseAccesible px (py - sp) carte)= gs { persoY = py - sp }
                              | otherwise = gs

-- Deplace le perso vers le bas
moveDown :: GameState -> GameState
moveDown gs@(GameState px py _ _ sp _ _ _ (C.Carte l h map)) | py < h && (C.caseAccesible px (py + sp) (C.Carte l h map)) = gs { persoY = py + sp }
                                | otherwise = gs


move_post :: GameState -> Bool
move_post (GameState px py _ _ _ _ _ _ (C.Carte l h _)) | px >= 0 && py>=0 && px <= l && py <= h = True
                                                        | otherwise = False

------------------------------
-- *** AUTRE DELACEMENT *** --
------------------------------


-- Deplace les monstres lorsque le compteurs est à 0
moveMonsters :: Int -> GameState -> GameState
moveMonsters 0 gs@(GameState _ _ _ _ _ liste _ _ _) = gs { monstres = (fmap Mst.moveMonster liste)}
moveMonsters _ gs = gs

------------------------------
-- *** MODIF ETAT *** --
------------------------------
changeItems :: GameState -> GameState
changeItems gs@(GameState px py e c _ _ items _ _) | (I.isSword px py e items || I.isKey px py c items) = let (I.Item id aff) = M.findWithDefault (I.Item I.ErrorItem False) (C.C px py) items in case id of
                                                                                                                                                    I.Epee -> let o = (I.changeItems (C.C px py) I.Epee items) in gs { epee = True, items = o }
                                                                                                                                                    I.Clef -> let o = (I.changeItems (C.C px py) I.Clef items) in gs { clef = True, items = o }
                                                                                                                                                    otherwise -> gs
                                            |otherwise = gs


changeMonstres :: GameState -> GameState
changeMonstres gs@(GameState px py True _ _ monstres _ _ _) | Mst.collisionMonstres px py monstres = let m = (Mst.elimineMonstres px py monstres) in gs { monstres = m, epee = False }
                                                            | otherwise = gs
changeMonstres gs@(GameState px py False _ _ monstres _ ini _) | Mst.collisionMonstres px py monstres =  initGameState ini
                                                               |otherwise = gs

-- Permet d'ouvrir une porte si le perso a une clef
activePorte :: GameState -> GameState
activePorte gs@(GameState px py _ True sp _ _ _ carte) =let (carte2, b)=(C.openDoor px py carte) in if b then gs{carte = carte2, clef = False} else gs
activePorte gs = gs

--------------------------------
-- *** DELACEMENT GENERAL *** --
--------------------------------
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = getEvent gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD,KeycodeK]

-- Gere l'événement liée au clavier
getEvent :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
getEvent gstate keyb deltaTime (x : []) = if K.keypressed x keyb then changeMonstres (moveTo gstate x deltaTime) else (changeMonstres gstate)
getEvent gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then getEvent (changeMonstres (moveTo gstate x deltaTime)) keyb deltaTime xs
                                                                 else getEvent (changeMonstres gstate) keyb deltaTime xs 

-- Repercution de l'évenement sur les elements de la gamestate
moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime =  changeItems (moveUp gstate)
moveTo gstate KeycodeQ deltaTime =  changeItems (moveLeft gstate)
moveTo gstate KeycodeS deltaTime =  changeItems (moveDown gstate)
moveTo gstate KeycodeD deltaTime =  changeItems (moveRight gstate)
moveTo gstate KeycodeK deltaTime =  activePorte gstate
moveTo gstate _ _ = gstate
