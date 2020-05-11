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


----- INVARIANT MONSTRE -----

prop_inv_coord_monstre :: Monstre -> Bool
prop_inv_coord_monstre (Monster _ (C.C x y) _ _ _) = y>=0 && x>=0 && ((mod x 50) == 0) && ((mod y 50) == 0)

prop_inv_direction_monstre :: Monstre -> Bool
prop_inv_direction_monstre (Monster m _ direction _ _) = direction < (length (getMonsterPattern m))

prop_inv_cpt_monstre :: Monstre -> Bool
prop_inv_cpt_monstre (Monster m _ _ cpt _) = cpt>=0 && cpt <= (getCptInit m)


----- MONSTER FUNCTIONS -----

-- Retourne le string liée au type du monstre
especeToString :: Espece -> String
especeToString e = case e of
    Orc -> "Orc"
    Skeleton -> "Skeleton"
    Fantome -> "Fantome"

-- Permet de recuperer selon l'espece le nombre de deplacement
-- par défaut pour un mouvement (direction)
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

prop_pre_MoveToDir ::String -> Bool
prop_pre_MoveToDir str =
    case str of 
        "Haut" -> True
        "Droite" -> True
        "Gauche" -> True
        "Bas" -> True
        x -> False


initMonstres :: [((Int,Int),String)] -> [Monstre]
initMonstres (((x,y),str):[]) | str == "Orc" = [(Monster Orc (C.C x y) 0 (getCptInit Orc) True)]
                              | str == "Skeleton" = [(Monster Skeleton (C.C x y) 0 (getCptInit Skeleton) True)]
                              | str == "Fantome" = [(Monster Fantome (C.C x y) 0 (getCptInit Fantome) True)]
initMonstres (((x,y),str):xs) | str == "Orc" = (Monster Orc (C.C x y) 0 (getCptInit Orc) True): (initMonstres xs)
                              | str == "Skeleton" = (Monster Skeleton (C.C x y) 0 (getCptInit Skeleton) True) : (initMonstres xs)
                              | str == "Fantome" = (Monster Fantome (C.C x y) 0 (getCptInit Fantome) True) : (initMonstres xs)

prop_pre_MonstreValide :: [((Int,Int),String)] -> Bool
prop_pre_MonstreValide (((x,y),id):[]) | id == "Orc" && ((mod x 50) == 0) && ((mod y 50) == 0) = True
                             | id == "Skeleton" && ((mod x 50) == 0) && ((mod y 50) == 0) = True
                             | id == "Fantome" && ((mod x 50) == 0) && ((mod y 50) == 0) = True
                             | otherwise = False
prop_pre_MonstreValide (((x,y),id):xs) | id == "Orc" && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_MonstreValide xs
                             | id == "Skeleton" && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_MonstreValide xs
                             | id == "Fantome" && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_MonstreValide xs
                             | otherwise = False

-- Modifie les coordonnées du monstre selon son pattern
moveMonster :: Monstre -> Monstre
moveMonster mo@(Monster m (C.C x y) index cpt a) | cpt == 0 && a = (Monster m (C.C x y) ((index + 1) `mod` (length (getMonsterPattern m))) (getCptInit m)  a) 
                                            | cpt > 0 && a = (Monster m (moveToDir ((getMonsterPattern m)!!index) (C.C x y) ) index (cpt-1) a)
                                            | otherwise = mo

prop_moveMonster :: Monstre -> Bool
prop_moveMonster mo@(Monster m (C.C x y) index cpt a) | ((mod x 50) == 0) && ((mod y 50) == 0) && x>=0 && y>=0 && cpt>=0 = True
                                                      | otherwise = False



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



prop_elimineMonstres_pre :: Int -> Int -> Bool
prop_elimineMonstres_pre px py | py>=0 && px>=0 && ((mod px 50) == 0) && ((mod py 50) == 0) = True
                          | otherwise = False
 





-- Verifie si un des monstres est en collision selon les coordonnées x et y donnée
collisionMonstres :: Int -> Int -> [Monstre] -> Bool
collisionMonstres px py ((Monster m (C.C x y) index cpt a):[])| px == x && py ==y && a= True
                                                           | otherwise = False
collisionMonstres px py ((Monster m (C.C x y) index cpt a):xs)| px == x && py ==y && a = True
                                                           | otherwise = (collisionMonstres px py xs)

 
prop_collisionMonstres_pre :: Int -> Int -> Bool
prop_collisionMonstres_pre px py | py>=0 && px>=0 && ((mod px 50) == 0) && ((mod py 50) == 0) = True
                            | otherwise = False






--- ** Generate Monster ** ---
{--chooseMonsters :: Int -> String
chooseMonsters i =
    case i of
        1 -> "Orc"
        2 -> "Fantome"
        3 -> "Skeleton"
        x -> "error"

genMonstresOk :: Gen ((Int,Int),String)
genMonstresOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    i <- choose(1,3)
    return $ (((x*50),(y*50)),chooseMonsters i)

prop_initMonstres_inv :: Property
prop_initMonstres_inv = forAll genMonstresOk $ prop_MonstreValide--}