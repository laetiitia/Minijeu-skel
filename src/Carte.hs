module Carte where

import qualified Data.Map.Strict as M
import Data.Maybe



---------------------------------
------------- CASE --------------
---------------------------------

data Case = Vide -- une case vide
  | Perso       --Id Personnage
  | AngleBD     --Coin Mur Bas Gauche
  | AngleBG     --Coin Mur Bas Droite
  | Horizontal  --Mur Horizontal
  | VerticalG   --Mur Vertical Gauche
  | VerticalD   --Mur Vertical Droit infranchissable (sauf pour les fantomes ...)
  | PorteNSF    --Porte Nord Sud Fermé
  | PorteEOFG   --Porte Est Ouest Fermé Gauche
  | PorteEOFD   --Porte Est Ouest Fermé Droite
  | PorteNSO    --Porte Nord Sud Ouverte
  | PorteEOOG   --Porte Est Ouest Ouverte Gauche
  | PorteEOOD   --Porte Est Ouest Ouverte Droite
  deriving Eq


instance Show Case where
  show c = caseToName c


associate :: Char -> Case
associate x = case x of
  '1' -> AngleBG
  '3' -> AngleBD
  '2' -> Horizontal
  '4' -> VerticalG
  '6' -> VerticalD
  ' ' -> Vide
  '@' -> Perso
  'a' -> PorteNSF
  'b' -> PorteNSO
  'c' -> PorteEOFD
  'd' -> PorteEOOD
  'e' -> PorteEOFG
  'f' -> PorteEOOG
  otherwise -> Vide

caseToName :: Case -> String
caseToName x = case x of
  AngleBG -> "angleBG"        --Coin Mur Bas Gauche
  AngleBD -> "angleBD"        --Coin Mur Bas Droite
  Horizontal -> "Horizontal"  --Mur Horizontal
  VerticalG -> "VerticalG"    --Mur Vertical Gauche
  VerticalD -> "VerticalD"    --Mur Vertical Droit
  Vide -> "sol"
  Perso -> "perso"
  PorteEOFG -> "PorteEOFG"    --Porte Est Ouest Fermé Gauche
  PorteEOOG -> "PorteEOOG"    --Porte Est Ouest Ouverte Gauche
  PorteEOFD -> "PorteEOFD"    --Porte Est Ouest Fermé Droite
  PorteEOOD -> "PorteEOOD"    --Porte Est Ouest Ouverte Droite
  PorteNSF -> "PorteNSF"      --Porte Nord Sud Fermé
  PorteNSO -> "PorteNSO"      --Porte Nord Sud Ouverte

-- Verifie que la case soit un mur
isWall :: Case -> Bool
isWall c = (c == AngleBD) || (c == AngleBG) || (c == Horizontal) || (c == VerticalG) || (c == VerticalD) 

-- Verifie que c'est une porte
isDoor :: Case -> Bool
isDoor c = (c == PorteEOFG) || (c == PorteEOOG) || (c == PorteEOFD) || (c == PorteEOOD) || (c == PorteNSF) || (c == PorteNSO) 

---------------------------------
------------ COORD --------------
---------------------------------

data Coord = C {cx :: Int , cy :: Int} 
    deriving Eq

instance Ord Coord where
  (<=) (C x1 y1) (C x2 y2) = (y1 < y2 || ( y1 == y2 && x1 <= x2))

instance Show Coord where
  show (C cx cy) = "{ cx: "++(show cx)++" , cy: "++(show cy)++" }"

-- Verifie que les coordonnées soit positif et qu'ils soient conforme à la hauteur et largeur
propCoord :: Coord -> Int -> Int -> Bool
propCoord (C cx cy) h l = 0 <= cx && cx <= l && 0 <= cy && cy <= h


---------------------------------
------------ CARTE --------------
---------------------------------

data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte 
                     }



-- *** INVARIANTS SUR LES CARTES *** --

propCarte ::  Carte -> Bool
propCarte carte = (propTailleCarte carte) && (propCaseCarte carte) && (propCasePorte carte)

-- Verifie que:
-- la hauteur et largeur de la carte par rapport aux nombres de cases
propTailleCarte :: Carte -> Bool
propTailleCarte (Carte larg haut contenue) = ((M.size contenue) == ((haut `div` 50) * (larg `div` 50)))

-- Verifie que:
-- chaque case existe et que les coordonnées soient correctes
-- que la carte soit bien entouré de mur
propCaseCarte :: Carte -> Bool
propCaseCarte (Carte larg haut contenu) = let list = M.foldrWithKey checkCase [] contenu in listAnd list
  where checkCase (C cx cy) val acc = (let inv1 = (propCoord (C cx cy) haut larg) in if (cx == 0 || cx == (larg - 50) || cy == 0 || cy == (haut - 50)) 
                                                                                      then ((isWall val) && inv1):acc 
                                                                                      else inv1:acc )

-- Verifie que:
-- chaque porte est bien maintenu par des murs
propCasePorte :: Carte -> Bool
propCasePorte (Carte larg haut contenu) = let list = M.foldrWithKey checkCase [] contenu in listAnd list
  where checkCase (C cx cy) val acc = (if (isDoor val)
                                        then (if (val == PorteNSF || val == PorteNSO)
                                          then (checkWall (C (cx-50) cy) contenu):(checkWall (C (cx+50) cy) contenu):acc
                                          else (checkWall (C cx (cy-50)) contenu):(checkWall (C cx (cy+50)) contenu):acc )
                                        else acc)

-- Verifie dans les coordonnées de cases si cette coordonnée correspond bien a un mur
checkWall :: Coord -> M.Map Coord Case -> Bool
checkWall c cases = let (Just res)= M.lookup c cases in isWall res 

-- Realise un 'and' sur tout les elements de la liste
listAnd :: [Bool] -> Bool
listAnd [] = True
listAnd (x:xs) 
  | x == False = False
  | otherwise = listAnd xs




-- *** OPERATIONS SUR LES CARTES *** --

-- Permet d'initialiser le contenu de la carte (les cases vont de 50 à 50)
initMapFromFile :: String -> Coord -> M.Map Coord Case -> M.Map Coord Case
initMapFromFile [] _ acc = acc
initMapFromFile (head:tail) (C x y) acc = if head == '\n' 
                                   then initMapFromFile tail (C 0 (y+50)) acc 
                                   else initMapFromFile tail (C (x+50) y) (M.insert (C x y) (associate head) acc)


-- Recupere du string represantant la carte, le nombre de case maximum dans une ligne et dans une colonne.
getFormat :: String -> (Int, Int)
getFormat str = let list = lines str in (maximum (map (\x -> length x) list), length list) 


-- Permet d'initialiser une carte grace au string reprensantant la date
readCarte :: String -> Carte
readCarte txt = let (x, y) = (getFormat txt) in Carte (x*50) (y*50) (initMapFromFile txt (C 0 0) M.empty)


-- Test si la case est accesible (ie soit vide soit la porte est ouverte)
caseAccesible :: Int -> Int -> M.Map Coord Case -> Bool
caseAccesible x y map = case M.lookup (C x y) map of
    Just Vide -> True
    Just PorteEOOD -> True
    Just PorteNSO -> True
    otherwise -> False


changePorte :: Int -> Int -> Int -> M.Map Coord Case -> (M.Map Coord Case,Bool)
changePorte x y cpt map = case cpt of
  0 ->let res = (aux (M.lookup (C (x+50) y) map)) in if (isJust res) then ((M.insert (C (x+50) y) (fromJust res) map) ,True ) else changePorte x y (cpt+1) map 
  1 ->let res = (aux (M.lookup (C (x-50) y) map)) in if (isJust res) then ((M.insert (C (x-50) y) (fromJust res) map) ,True ) else changePorte x y (cpt+1) map 
  2 ->let res = (aux (M.lookup (C x (y+50)) map)) in if (isJust res) then ((M.insert (C x (y+50)) (fromJust res) map) ,True ) else changePorte x y (cpt+1) map 
  3 ->let res = (aux (M.lookup (C x (y-50)) map)) in if (isJust res) then ((M.insert (C x (y-50)) (fromJust res) map) ,True ) else (map,False) 




aux :: Maybe Case -> Maybe Case
aux c = case c of 
  Just PorteEOFG -> Just PorteEOOG
  Just PorteEOFD -> Just PorteEOOD
  Just PorteNSF -> Just PorteNSO
  x -> Nothing



