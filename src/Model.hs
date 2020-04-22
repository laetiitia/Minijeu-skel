
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
    | Skeleton

data Monstre = Monster {espece :: Espece, coor :: Coord, direct :: Int, cpt :: Int}

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int 
                           , monstres :: [Monstre]
                           , carte :: Carte}


-----------------------------
----- MONSTER FUNCTIONS -----
-----------------------------
especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"
    Skeleton -> "Skeleton"

getMonsterPattern :: Espece -> [String]
getMonsterPattern esp =
    case esp of
        Orc -> ["Haut", "Droite", "Bas", "Gauche"]
        Skeleton -> ["Droite", "Droite", "Haut","Gauche", "Haut", "Haut", "Gauche","Bas","Bas"]

moveToDir :: String -> C.Coord -> C.Coord
moveToDir str (C.C x y) = 
    case str of 
        "Haut" -> (C.C x (y - 50))
        "Droite" -> (C.C (x + 50) y)
        "Gauche" -> (C.C (x - 50) y)
        "Bas" -> (C.C x (y + 50))

initMonstres ::Int -> [Monstre]
initMonstres 0 = []
initMonstres x = (Monster Orc (C.C 250 250) 0 3): (initMonstres (x - 1))

mooveCreature :: Monstre -> Monstre
mooveCreature (Monster Orc (C.C x y) index cpt) | cpt == 0 = (Monster Orc (C.C x y) ((index + 1) `mod` (length (getMonsterPattern Orc))) 3) 
                                                | cpt > 0  = (Monster Orc (moveToDir ((getMonsterPattern Orc)!!index) (C.C x y) ) index (cpt-1) ) 


-----------------------------
----- GENERAL FUNCTIONS -----
-----------------------------

testeCoord :: Int -> Int -> M.Map Coord Case -> Bool
testeCoord x y map = case M.lookup (C.C x y) map of
    Just C.Vide -> True
    Just C.PorteEOOD -> True
    Just C.PorteNSO -> True
    otherwise -> False


mooveEspece :: [Monstre] -> [Monstre]
mooveEspece (m:[]) = [(mooveCreature m)]
mooveEspece (m:x) = (mooveCreature m) : (mooveEspece x)

mooveMonstre :: Int -> GameState -> GameState
mooveMonstre 0 gs@(GameState _ _ _ liste _) = gs { monstres = mooveEspece liste}
mooveMonstre _ gs = gs

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


