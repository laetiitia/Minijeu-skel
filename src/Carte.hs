module Carte where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Data.Map.Strict as M

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM


---------------------------------
------------- CASE --------------
---------------------------------

data Case = Vide -- une case vide
  | Perso --id
  | AngleBD
  | AngleBG
  | Horizontal
  | VerticalG
  | VerticalD -- infranchissable (sauf pour les fantomes ...)
  | PorteNSF
  | PorteEOFG
  | PorteEOFD
  | PorteNSO
  | PorteEOOG
  | PorteEOOD
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
  AngleBG -> "angleBG"
  AngleBD -> "angleBD"
  Horizontal -> "Horizontal"
  VerticalG -> "VerticalG"
  VerticalD -> "VerticalD"
  Vide -> "sol"
  Perso -> "perso"
  PorteEOFG -> "PorteEOFG"
  PorteEOOG -> "PorteEOOG"
  PorteEOFD -> "PorteEOFD"
  PorteEOOD -> "PorteEOOD"
  PorteNSF -> "PorteNSF"
  PorteNSO -> "PorteNSO"

-- Verifie que la case soit un mur
isMur :: Case -> Bool
isMur c = (c == AngleBD) || (c == AngleBG) || (c == Horizontal) || (c == VerticalG) || (c == VerticalD) 

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
propCarte carte = (propTailleCarte carte) && (propCaseCarte carte)

-- Verifie que:
-- la hauteur et largeur de la carte par rapport aux nombres de cases
propTailleCarte :: Carte -> Bool
propTailleCarte (Carte haut larg contenue) = ((M.size contenue) == ((haut `div` 50) * (larg `div` 50)))

-- Verifie que:
-- chaque case existe et que les coordonnées soient correctes
-- que la carte soit bien entouré de mur
propCaseCarte :: Carte -> Bool
propCaseCarte (Carte haut larg contenu) = let list = M.foldrWithKey checkMap [] contenu in listAnd list
  where checkMap (C cx cy) val acc = (let inv1 = (propCoord (C cx cy) haut larg) in if (cx == 0 || cx == (larg - 50) || cy == 0 || cy == (haut - 50)) 
                                                                                      then ((isMur val) && inv1):acc 
                                                                                      else inv1:acc )

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





