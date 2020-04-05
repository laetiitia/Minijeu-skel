module Carte where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Data.Map.Strict as M

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

data Case = Vide -- une case vide
  | Perso --id
  | Mur -- infranchissable (sauf pour les fantomes ...)
  deriving Eq


----
data Coord = C {cx :: Int , cy :: Int} 
    deriving Eq

instance Ord Coord where
  (<=) (C x1 y1) (C x2 y2) = (y1 < y2 || ( y1=y2 && x1 <= x2))

instance Show Coord where
  show (C cx cy) = "{ cx: "++(show cx)++" , cy: "++(show cy)++" }"

---
data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte 
                     }



------------- FUNCION FOR READFILE -------------
associate :: Char -> Case
associate x = case x of
  'X' -> Mur
  ' ' -> Vide
  '@' -> Perso
  otherwise -> Mur

caseToName :: Case -> String
caseToName x = case x of
   Mur -> "mur"
   Vide -> "sol"
   Perso -> "perso"

initMapFromFile :: String -> Coord -> M.Map Coord Case -> M.Map Coord Case
initMapFromFile [] _ acc = acc
initMapFromFile (head:tail) (C x y) acc = if head == '\n' 
                                   then initMapFromFile tail (C 0 (y+50)) acc 
                                   else initMapFromFile tail (C (x+50) y) (M.insert (C x y) (associate head) acc)

readCarte :: Int -> Int -> String -> Carte
readCarte x y txt = Carte x y (initMapFromFile txt (C 0 0) M.empty)





