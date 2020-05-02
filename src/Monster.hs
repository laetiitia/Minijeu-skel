module Monster where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Carte (Coord)
import qualified Carte as C

data Espece = 
    Orc
    | Skeleton
    | Fantome

data Monstre = Monster {espece :: Espece, coor :: Coord, direct :: Int, cpt :: Int, affichage :: Bool}


----- MONSTER FUNCTIONS -----

especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"
    Skeleton -> "Skeleton"
    Fantome -> "Fantome"

-- Permet de recuperer selon l'espece le nombre deplacement par mouvement
getCptInit :: Espece -> Int
getCptInit e = case e of
    Orc -> 3
    Skeleton -> 2
    Fantome -> 5


-- Represente le chemin du monstre (decrit dans une liste de direction)
getMonsterPattern :: Espece -> [String]
getMonsterPattern esp =
    case esp of
        Orc -> ["Haut", "Droite", "Bas", "Gauche"]
        Skeleton -> ["Haut", "Bas"]
        Fantome -> ["Droite", "Bas", "Droite","Haut","Gauche", "Bas", "Gauche","Haut" ]


-- Modifie la coordonée en fonction de la direction
moveToDir :: String -> C.Coord -> C.Coord
moveToDir str (C.C x y) = 
    case str of 
        "Haut" -> (C.C x (y - 50))
        "Droite" -> (C.C (x + 50) y)
        "Gauche" -> (C.C (x - 50) y)
        "Bas" -> (C.C x (y + 50))



-- Initialise le monstre
initMonstres ::Int -> Int -> Int -> [Monstre]
initMonstres 0 _ _ = []
initMonstres x l h = (Monster Orc (C.C 250 250) 0 (getCptInit Orc) True):(Monster Skeleton (C.C 1150 500) 0 (getCptInit Skeleton) True):(Monster Fantome (C.C 50 50) 0 (getCptInit Fantome) True): (initMonstres (x - 1) l h)


-- Modifie les coordonnées du monstre selon son pattern
moveMonster :: Monstre -> Monstre
moveMonster mo@(Monster m (C.C x y) index cpt a) | cpt == 0 && a = (Monster m (C.C x y) ((index + 1) `mod` (length (getMonsterPattern m))) (getCptInit m)  a) 
                                            | cpt > 0 && a = (Monster m (moveToDir ((getMonsterPattern m)!!index) (C.C x y) ) index (cpt-1) a)
                                            | otherwise = mo


-- Modifie les coordonnées d'une liste des monstres
moveAllMonster :: [Monstre] -> [Monstre]
moveAllMonster [] = []
moveAllMonster (x:xs) = (moveMonster x):(moveAllMonster xs)


elimineMonstres :: Int -> Int -> [Monstre] -> [Monstre]
elimineMonstres px py ((Monster m (C.C x y) index cpt a):[])| px == x && py ==y = [(Monster m (C.C x y) index cpt False)]
                                                           | otherwise = [(Monster m (C.C x y) index cpt a)]
elimineMonstres px py ((Monster m (C.C x y) index cpt a):xs)| px == x && py ==y = ((Monster m (C.C x y) index cpt False):(elimineMonstres px py xs))
                                                            | otherwise = ((Monster m (C.C x y) index cpt a):(elimineMonstres px py xs))
elimineMonstres px py [] = []

 
-- Verifie si un des monstres est en collision selon les coordonnées x et y donnée
collisionMonstres :: Int -> Int -> [Monstre] -> Bool
collisionMonstres px py ((Monster m (C.C x y) index cpt a):[])| px == x && py ==y && a= True
                                                           | otherwise = False
collisionMonstres px py ((Monster m (C.C x y) index cpt a):xs)| px == x && py ==y && a = True
                                                           | otherwise = (collisionMonstres px py xs)