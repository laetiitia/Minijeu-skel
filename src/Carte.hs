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
  | AngleHD
  | AngleHG
  | Horizontal
  | VerticalG
  | VerticalD -- infranchissable (sauf pour les fantomes ...)
  deriving Eq

instance Show Case where
  show c = caseToName c

--propCase :: Case -> Bool
--propCase c = (c == Vide ) || (c == Mur) || (c == Perso)

associate :: Char -> Case
associate x = case x of
  '7' -> AngleHG
  '9' -> AngleHD
  '2' -> Horizontal
  '4' -> VerticalG
  '6' -> VerticalD
  ' ' -> Vide
  'X' -> Perso
  otherwise -> Vide

caseToName :: Case -> String
caseToName x = case x of
  AngleHG -> "angleHG"
  AngleHD -> "angleHD"
  Horizontal -> "Horizontal"
  VerticalG -> "VerticalG"
  VerticalD -> "VerticalD"
  Vide -> "sol"
  Perso -> "perso"

---------------------------------
------------ COORD --------------
---------------------------------

data Coord = C {cx :: Int , cy :: Int} 
    deriving Eq

instance Ord Coord where
  (<=) (C x1 y1) (C x2 y2) = (y1 < y2 || ( y1 == y2 && x1 <= x2))

instance Show Coord where
  show (C cx cy) = "{ cx: "++(show cx)++" , cy: "++(show cy)++" }"

-- Verifie que les coordonnées soit positif et qu'ils soient conforme à la hauteur est largeur
propCoord :: Coord -> Int -> Int -> Bool
propCoord (C cx cy) h l = 0 <= cx && cx <= l && 0 <= cy && cy <= h


---------------------------------
------------ CARTE --------------
---------------------------------

data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte 
                     }

-- Verifie que:
-- la hauteur et largeur de la carte par rapport à la fenetre
-- chaque case existe et que les coordonnées soient correctes
-- que la carte soit bien entouré de mur
{-propCarte ::  Carte -> Bool
propCarte (Carte haut larg contenu) = let inv0 = ((0 < haut && haut <= 500 ) && (0 < larg && larg <= 700 )) in
  if not inv0 
    then False
    else let list = M.foldrWithKey checkMap [] contenu in
      if (length list) == ((haut `div` 50) * (larg `div` 50)) 
        then listAnd list
        else False
        where checkMap (C cx cy) val acc = let inv1 = ((propCoord (C cx cy) haut larg) && (propCase val)) in (if (cx == 0 || cx == (larg - 50) || cy == 0 || cy == (haut - 50)) then (val == Mur && inv1):acc else inv1:acc)
-}

listAnd :: [Bool] -> Bool
listAnd [] = True
listAnd (x:xs) 
  | x == False = False
  | otherwise = listAnd xs


initMapFromFile :: String -> Coord -> M.Map Coord Case -> M.Map Coord Case
initMapFromFile [] _ acc = acc
initMapFromFile (head:tail) (C x y) acc = if head == '\n' 
                                   then initMapFromFile tail (C 0 (y+50)) acc 
                                   else initMapFromFile tail (C (x+50) y) (M.insert (C x y) (associate head) acc)

readCarte :: Int -> Int -> String -> Carte
readCarte x y txt = Carte x y (initMapFromFile txt (C 0 0) M.empty)





