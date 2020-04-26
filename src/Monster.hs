module Monster where

import SDL

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Carte (Coord)
import qualified Carte as C

data Espece = 
    Orc
-- | Skeleton

data Monstre = Monster {espece :: Espece, coor :: Coord, direct :: Int, cpt :: Int}


----- MONSTER FUNCTIONS -----

especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"
-- Skeleton -> "Skeleton"


-- Represente le chemin du monstre (decrit dans une liste de direction)
getMonsterPattern :: Espece -> [String]
getMonsterPattern esp =
    case esp of
        Orc -> ["Haut", "Droite", "Bas", "Gauche"]
-- Skeleton -> ["Droite", "Droite", "Haut","Gauche", "Haut", "Haut", "Gauche","Bas","Bas"]


-- Modifie la coordonée en fonction de la direction
moveToDir :: String -> C.Coord -> C.Coord
moveToDir str (C.C x y) = 
    case str of 
        "Haut" -> (C.C x (y - 50))
        "Droite" -> (C.C (x + 50) y)
        "Gauche" -> (C.C (x - 50) y)
        "Bas" -> (C.C x (y + 50))



-- Initialise le monstre
initMonstres ::Int -> [Monstre]
initMonstres 0 = []
initMonstres x = (Monster Orc (C.C 250 250) 0 3): (initMonstres (x - 1))


-- Modifie les coordonnées du monstre selon son pattern
moveMonster :: Monstre -> Monstre
moveMonster (Monster m (C.C x y) index cpt) | cpt == 0 = (Monster m (C.C x y) ((index + 1) `mod` (length (getMonsterPattern Orc))) 3) 
                                            | cpt > 0  = (Monster m (moveToDir ((getMonsterPattern Orc)!!index) (C.C x y) ) index (cpt-1) ) 


-- Modifie les coordonnées d'une liste des monstres
moveAllMonster :: [Monstre] -> [Monstre]
moveAllMonster [] = []
moveAllMonster (x:xs) = (moveMonster x):(moveAllMonster xs)