module Monster where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Carte (Coord)
import qualified Carte as C
import Control.Monad.Trans.State
import Control.Monad
import qualified System.Random as R

data Espece = 
    Orc
    | Skeleton
    | Fantome
    | Demon
    | Undead
    deriving Eq

data Monstre = Monster {espece :: Espece, coor :: Coord, direct :: Int, cpt :: Int, affichage :: Bool}


instance Show Monstre where
    show(Monster e (C.C x y) d cpt a ) =  ("espéce: " ++(especeToString e) ++" coord x: " ++ (show x) ++" coord y: " ++ (show y) ++"  direction: " ++ (show d) ++" compteur: " ++ (show cpt) ++" affichage: " ++ (show a))

instance Show Espece where
    show e = especeToString e


---------- INVARIANTS MONSTRE ----------

prop_inv_Monstre :: Monstre -> Bool
prop_inv_Monstre m = (prop_inv_coord_monstre m) && (prop_inv_cpt_monstre m) && (prop_inv_direction_monstre m)


-- Verifie que les coordonnées du monstre soit correcte 
prop_inv_coord_monstre :: Monstre -> Bool
prop_inv_coord_monstre (Monster _ (C.C x y) _ _ _) = y>=0 && x>=0 && ((mod x 50) == 0) && ((mod y 50) == 0)

-- Verifie que l'index de direction soit bien compris dans la longueur du tableau de son pattern
prop_inv_direction_monstre :: Monstre -> Bool
prop_inv_direction_monstre (Monster m _ direction _ _) = (direction < (length (getMonsterPattern m))) && (0 <= direction)

-- Verifie que le nombre de mouvement pour une direction soit bien correcte
prop_inv_cpt_monstre :: Monstre -> Bool
prop_inv_cpt_monstre (Monster m _ _ cpt _) = (0 <= cpt) && (cpt <= (getCptInit m))



----- MONSTER FONCTIONS SECONDAIRES -----

-- Retourne le string liée au type du monstre
especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"
    Skeleton -> "Skeleton"
    Fantome -> "Fantome"
    Demon -> "Demon"
    Undead -> "Undead"

-- Retourne l'Espece associé au string
-- Remarque: verifié le string passé en argument avec la fonction 
-- stringIsEspece sinon par defaut cela retourne un Orc
stringToEspece :: String -> Espece
stringToEspece str = case str of
    "Orc" -> Orc
    "Skeleton" -> Skeleton
    "Fantome" -> Fantome
    "Demon" -> Demon
    "Undead" -> Undead
    otherwise -> Undead -- Espece retourné par defaut


-- Verifie que le string soit bien assimilé à une Espece
stringIsEspece :: String -> Bool
stringIsEspece str = case str of
    "Orc" -> True
    "Skeleton" -> True
    "Fantome" -> True
    "Demon" -> True
    "Undead" -> True
    otherwise -> False 

-- Permet de recuperer selon l'espece le nombre de deplacement
-- par défaut pour un mouvement (direction)
getCptInit :: Espece -> Int
getCptInit e = case e of
    Orc -> 3
    Skeleton -> 2
    Fantome -> 5
    Demon -> 3
    otherwise -> 1 -- par defaut



-- Represente le chemin du monstre (decrit dans une liste de direction)
getMonsterPattern :: Espece -> [String]
getMonsterPattern esp =
    case esp of
        Orc -> ["Haut", "Droite", "Bas", "Gauche"]
        Skeleton -> ["Haut", "Bas"]
        Fantome -> ["Droite", "Bas", "Droite","Haut","Gauche", "Bas", "Gauche","Haut" ]
        Demon -> ["Gauche","Droite"]
        otherwise -> ["Haut", "Droite", "Bas", "Gauche"] -- par defaut

-- Verifie que la liste des directions soit correcte 
prop_post_getMonsterPattern :: Espece -> Bool
prop_post_getMonsterPattern e = C.listAnd (fmap prop_pre_getMonsterPattern (getMonsterPattern e))

prop_pre_getMonsterPattern ::String -> Bool
prop_pre_getMonsterPattern str =
    case str of 
        "Haut" -> True
        "Droite" -> True
        "Gauche" -> True
        "Bas" -> True
        otherwise -> False



-- Modifie la coordonée en fonction de la direction
moveToDir :: String -> C.Coord -> C.Coord
moveToDir str (C.C x y) = 
    case str of 
        "Haut" -> (C.C x (y - 50))
        "Droite" -> (C.C (x + 50) y)
        "Gauche" -> (C.C (x - 50) y)
        "Bas" -> (C.C x (y + 50))
        otherwise -> (C.C x y)

-- Verifie que le string envoyer soit correcte
prop_pre_MoveToDir ::String -> C.Coord -> Bool
prop_pre_MoveToDir str (C.C cx cy) =
    case str of 
        "Haut" -> 0<=cx && 0 <= cy && ((mod cx 50) == 0) && ((mod cy 50) == 0)
        "Droite" -> 0<=cx && 0 <= cy && ((mod cx 50) == 0) && ((mod cy 50) == 0)
        "Gauche" -> 0<=cx && 0 <= cy && ((mod cx 50) == 0) && ((mod cy 50) == 0)
        "Bas" -> 0<=cx && 0 <= cy && ((mod cx 50) == 0) && ((mod cy 50) == 0)
        otherwise -> False


prop_post_MoveToDir :: C.Coord -> Bool 
prop_post_MoveToDir (C.C cx cy) = 0<=cx && 0 <= cy && ((mod cx 50) == 0) && ((mod cy 50) == 0)

----- MONSTER FONCTIONS GENERAUX -----

-- Initialise une liste de monstre selon les informations données ( Coordonnées du monstre, Type du Monstre  )
initMonstres :: [(C.Coord, String)] -> [Monstre]
initMonstres [] = []
initMonstres ((c, id):xs) | (stringIsEspece id) = let e = (stringToEspece id) in (Monster e c 0 (getCptInit e) True): (initMonstres xs)
                              | otherwise = (initMonstres xs)

-- Verifie que les coordonnées soient des multiples de 50 et que le string corresponde à une Espece  
prop_pre_initMonstres :: [(C.Coord,String)] -> Bool
prop_pre_initMonstres [] = True
prop_pre_initMonstres (((C.C x y),id):xs)   | (stringIsEspece id) && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_initMonstres xs
                                            | otherwise = False 

-- Verifie que tout les monstres en sorties soient correctes 
prop_post_initMonstres :: [(C.Coord,String)] -> Bool
prop_post_initMonstres [] = True
prop_post_initMonstres list = C.listAnd (fmap prop_inv_Monstre (initMonstres list))


-- Reset les Monstre
reset :: [Monstre] -> [Monstre]
reset m = map (\(Monster m c i cpt a) -> (Monster m c i cpt True)) m


----------- Arbitrary ------------
type Gen a = State R.StdGen a

class Arb a where 
  arb :: Gen a

instance Arb Int where
  arb = state R.next

generate :: Gen Int
generate = liftM (`mod` 1000) arb

-- Retourne une direction arbitraire, ici n est la somme des coordonées du monstre plus celle se son compteur
getArbitraryDir :: Int -> String
getArbitraryDir n = let index = mod (foldr (+) 0  (evalState (sequence (replicate 10 (generate))) (R.mkStdGen n))) 4 in ["Haut", "Bas", "Droite", "Gauche"]!!index





-- Modifie les coordonnées du monstre selon son pattern
moveMonster :: Monstre -> Monstre
moveMonster mo@(Monster m (C.C x y) index cpt a)| (m == Undead) = (Monster m (moveToDir (getArbitraryDir (x + y + index)) (C.C x y)) (index+1) cpt a)
                                                | (cpt == 0 && a) = (Monster m (C.C x y) ((index + 1) `mod` (length (getMonsterPattern m))) (getCptInit m)  a) 
                                                | (cpt > 0 && a) = (Monster m (moveToDir ((getMonsterPattern m)!!index) (C.C x y) ) index (cpt-1) a)
                                                | otherwise = mo

-- Précondition moveMonstre: le monstre doit vérifié l'invariant 
prop_pre_moveMonster :: Monstre -> Bool
prop_pre_moveMonster monstre = prop_inv_Monstre monstre

-- Postcondition moveMonstre: le monstre doit vérifié l'invariant en sortie de fonction
prop_post_moveMonster::Monstre -> Bool
prop_post_moveMonster monstre = prop_inv_Monstre (moveMonster monstre)




-- Elimine tout les monstres qui ont pour coordonnées ce passé en argument 
elimineMonstres :: Int -> Int -> [Monstre] -> [Monstre]
elimineMonstres px py [] = []
elimineMonstres px py ((Monster m (C.C x y) index cpt a):xs)| (px == x && py == y) = ((Monster m (C.C x y) index cpt False):(elimineMonstres px py xs))
                                                            | otherwise = ((Monster m (C.C x y) index cpt a):(elimineMonstres px py xs))

-- PreCondition elimineMonstres : verifie les coordonnées et que les monstres soient dans les normes 
prop_pre_elimineMonstres :: Int -> Int -> [Monstre] -> Bool
prop_pre_elimineMonstres px py ms = py>=0 && px>=0 && ((mod px 50) == 0) && ((mod py 50) == 0) && (C.listAnd (fmap prop_inv_Monstre ms) )
 
-- PostCondition elimineMonstres : verifie que les monstres en sortie de fonction soit dans les normes
-- et que l'affichage des monstres eliminés soit bien à False
prop_post_elimineMonstres :: Int -> Int -> [Monstre] -> Bool
prop_post_elimineMonstres px py ms = let res = elimineMonstres px py ms in (C.listAnd (fmap prop_inv_Monstre res)) && (checkAff res)
    where checkAff list | (length list == 0) = True
                        | otherwise = let (Monster m (C.C x y) index cpt a) = (head list) in if (x==px && y==py) then ((not a) && checkAff (tail list)) else checkAff (tail list)




-- Verifie si un des monstres est en collision selon les coordonnées x et y donnée
collisionMonstres :: Int -> Int -> [Monstre] -> Bool
collisionMonstres _ _ [] = False
collisionMonstres px py ((Monster m (C.C x y) index cpt a):xs)  | (px == x && py ==y && a ) = True --collision detecté
                                                                | otherwise = (collisionMonstres px py xs) --verifie les autres

-- PreCondition collisionMonstres : verifie les coordonnées et que les monstres soient dans les normes
prop_pre_collisionMonstres :: Int -> Int -> [Monstre] -> Bool
prop_pre_collisionMonstres px py ms =  py>=0 && px>=0 && ((mod px 50) == 0) && ((mod py 50) == 0) && (C.listAnd (fmap prop_inv_Monstre ms) )


-- Recupere les données du fichier texte pour initialiser la liste des monstres
readCarte :: String -> [Monstre]
readCarte [] = []
readCarte txt = getMonsters  txt 0 0
    where getMonsters  (x:xs) cx cy = (if xs == [] 
                                    then []
                                    else case x of
                                            'o' -> (Monster Orc (C.C cx cy) 0 (getCptInit Orc) True) : (getMonsters  xs (cx+50) cy)
                                            'f' -> (Monster Fantome (C.C cx cy) 0 (getCptInit Fantome) True) : (getMonsters  xs (cx+50) cy)
                                            's' -> (Monster Skeleton (C.C cx cy) 0 (getCptInit Skeleton) True) : (getMonsters  xs (cx+50) cy)
                                            'd' -> (Monster Demon (C.C cx cy) 0 (getCptInit Demon) True) : (getMonsters  xs (cx+50) cy)
                                            'u' -> (Monster Undead (C.C cx cy) 0 (getCptInit Undead) True) : (getMonsters  xs (cx+50) cy)
                                            '\n' -> getMonsters  xs 0 (cy+50) 
                                            otherwise -> getMonsters  xs (cx+50) cy)

-- PostCondition de readCarte: que la liste des montres soient corrects
prop_post_readCarte :: String -> Bool
prop_post_readCarte txt = C.listAnd $ fmap prop_inv_Monstre (readCarte txt)




