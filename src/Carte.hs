module Carte where

import qualified Data.Map.Strict as M
import Data.Maybe



---------------------------------
------------- CASE --------------
---------------------------------

data Case = Vide -- une case vide (et porte ouverte)
  | Perso        --Id Personnage
  | AngleT       --Angle T Mur 
  | Horizontal   --Mur Horizontal
  | Vertical     --Mur Vertical  infranchissable (sauf pour les fantomes ...)
  | PorteNS      --Porte Nord Sud (forcement fermé car ouvert = vide )
  | PorteEO      --Porte Est Ouest (forcement fermé car ouvert = vide )
  deriving Eq


instance Show Case where
  show c = caseToName c

-- Associe un caractere à un element de type Case
associate :: Char -> Case
associate x = case x of
  'T' -> AngleT
  '=' -> Horizontal
  '|' -> Vertical
  ' ' -> Vide
  '@' -> Perso
  'n' -> PorteNS
  'c' -> PorteEO
  otherwise -> Vide

-- Retourne le nom de la Case 
caseToName :: Case -> String
caseToName x = case x of
  AngleT -> "angleT"          --Angle T Mur 
  Horizontal -> "Horizontal"  --Mur Horizontal
  Vertical -> "Vertical"      --Mur Vertical 
  Vide -> "sol"
  Perso -> "perso"
  PorteEO -> "PorteEO"        --Porte Est Ouest 
  PorteNS -> "PorteNS"        --Porte Nord Sud 

-- Verifie que la case soit un mur
isWall :: Case -> Bool
isWall c = (c == AngleT) || (c == Horizontal) || (c == Vertical) 

-- Verifie que c'est une porte
isDoor :: Case -> Bool
isDoor c = (c == PorteEO) || (c == PorteNS) 



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
-- de plus chaque coordonnée est multiple de 50
prop_Coord :: Coord -> Int -> Int -> Bool
prop_Coord (C cx cy) l h = 0 <= cx && cx <= l && 0 <= cy && cy <= h && ((mod cx 50) == 0) && ((mod cy 50) == 0)



---------------------------------
------------ CARTE --------------
---------------------------------

data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte 
                     }



-- *** INVARIANTS SUR LES CARTES *** --

-- Verifie toute les propriétés de la carte
prop_inv_Carte ::  Carte -> Bool
prop_inv_Carte carte = (prop_TailleCarte carte) && (prop_CaseCarte carte) && (prop_CasePorte carte)

-- Verifie que:
-- la hauteur et largeur de la carte par rapport aux nombres de cases
-- et ainsi leur existance
prop_TailleCarte :: Carte -> Bool
prop_TailleCarte (Carte larg haut contenue) = ((M.size contenue) == ((haut `div` 50) * (larg `div` 50)))

-- Verifie que:
-- chaque case possede des coordonnées correctes
-- que la carte soit bien entouré de mur
prop_CaseCarte :: Carte -> Bool
prop_CaseCarte (Carte larg haut contenu) = let list = M.foldrWithKey checkCase [] contenu in listAnd list
  where checkCase (C cx cy) val acc = (let inv1 = (prop_Coord (C cx cy) larg haut) in if (cx == 0 || cx == (larg - 50) || cy == 0 || cy == (haut - 50)) 
                                                                                      then ((isWall val) && inv1):acc 
                                                                                      else inv1:acc )

-- Verifie que:
-- chaque porte est bien maintenu par des murs
prop_CasePorte :: Carte -> Bool
prop_CasePorte (Carte larg haut contenu) = let list = M.foldrWithKey checkCase [] contenu in listAnd list
  where checkCase (C cx cy) val acc = (if (isDoor val)
                                        then (if (val == PorteNS)
                                          then (checkWall (C (cx-50) cy) contenu):(checkWall (C (cx+50) cy) contenu):acc
                                          else (checkWall (C cx (cy-50)) contenu):(checkWall (C cx (cy+50)) contenu):acc )
                                        else acc)

-- Verifie dans les coordonnées de cases si cette coordonnée correspond bien a un mur
checkWall :: Coord -> M.Map Coord Case -> Bool
checkWall c cases = case M.lookup c cases of
  (Just v) -> isWall v
  otherwise -> False 

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

-- Précondition de readCarte: le String ne doit pas etre vide
prop_pre_readCarte :: String -> Bool
prop_pre_readCarte s = not (null s)

-- Postcondition de readCarte: cela retourne une carte correcte
prop_post_readCarte :: String -> Bool
prop_post_readCarte str = prop_inv_Carte (readCarte str)




-- Test si la case est accesible (ie la case est vide)
caseAccesible :: Int -> Int -> Carte -> Bool
caseAccesible x y (Carte _ _ map) = case M.lookup (C x y) map of
    Just Vide -> True
    otherwise -> False



-- Cherche une porte autour des coordonnées données s'il y en a une.
-- L'unique appel a cette méthode ce fait au niveau de openDoor, or 
-- la pré-condition de openDoor vérifie que les coordonnées soit correct
-- et que la map ne soit pas null.
findDoor :: Int -> Int -> Int -> M.Map Coord Case -> Maybe Coord
findDoor x y cpt map =  case cpt of
  0 -> let res = (M.lookup (C (x+50) y) map) in if (isDoor (fromJust res)) then (Just (C (x+50) y)) else findDoor x y (cpt+1) map
  1 -> let res = (M.lookup (C (x-50) y) map) in if (isDoor (fromJust res)) then (Just (C (x-50) y)) else findDoor x y (cpt+1) map
  2 -> let res = (M.lookup (C x (y+50)) map) in if (isDoor (fromJust res)) then (Just (C x (y+50))) else findDoor x y (cpt+1) map
  3 -> let res = (M.lookup (C x (y-50)) map) in if (isDoor (fromJust res)) then (Just (C x (y-50))) else Nothing
  otherwise -> Nothing


-- Ouvre la porte fermé autour des coordonnées données s'il y en a une (lors de l'ouverture la case devient vide)
openDoor :: Int -> Int -> Carte -> (Carte, Bool)
openDoor x y (Carte l h map) = let res = findDoor x y 0 map in if (isJust res) 
                                                                then ((Carte l h (M.insert (fromJust res) Vide map)), True)
                                                                else ((Carte l h map), False)


-- Précondition de changePorte: que la carte soit correct ainsi que la coordonnée donnée (et vérification d'invariant de carte)
prop_pre_openDoor :: Int -> Int -> Carte -> Bool
prop_pre_openDoor x y (Carte l h map) = (prop_Coord (C x y) l h) && (not (M.null map)) && (prop_inv_Carte (Carte l h map))

-- PostCondition de changePorte: la carte est toujours correct et le booléan est à true (et vérification d'invariant de carte)
prop_post_openDoor :: Int -> Int -> Carte -> Bool
prop_post_openDoor x y (Carte l h map) = let ((Carte l2 h2 map2), b) = openDoor x y (Carte l h map) in b && (prop_inv_Carte (Carte l2 h2 map2)) && (isOpen map2)
  where isOpen m = let res = (findDoor x y 0 map) in (if (isJust res) 
                                                      then let c = (M.lookup (fromJust res) m) in if (isJust c) then (Vide == (fromJust c)) else False
                                                      else False)




