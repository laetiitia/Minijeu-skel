module Items where

import qualified Data.Map.Strict as M
import Carte (Coord)
import Carte (Case)
import qualified Carte as C


data Type =
    Epee
    | Clef
    | Tresor
    | ErrorItem


data Item = Item {id :: Type, affichage :: Bool}


typeToString :: Type -> String
typeToString id = case id of
    Epee -> "epee"
    Clef -> "clef"
    Tresor -> "tresor"
    ErrorItem -> "ErrorItem"

-- Initialise les Items
initItems :: [(String,(Int,Int))] -> M.Map Coord Item
initItems ((id,(x,y)):[])  | id == "clef" = M.singleton (C.C x y) (Item Clef True)
                           | id == "epee" = M.singleton (C.C x y) (Item Epee True)
                           | id == "tresor" = M.singleton (C.C x y) (Item Tresor True)
initItems ((id,(x,y)):xs)  | id == "clef" = M.insert (C.C x y) (Item Clef True) (initItems xs)
                           | id == "epee" = M.insert (C.C x y) (Item Epee True) (initItems xs)        
                           | id == "tresor" = M.insert (C.C x y) (Item Tresor True) (initItems xs)                   



-- Verifie si c'est une épée ou non 
isSword :: Int -> Int ->Bool -> M.Map Coord Item -> Bool
isSword x y False map = case M.lookup (C.C x y) map of
    Just (Item Epee True) -> True
    otherwise -> False
isSword x y b map = False
-- Verifie si c'est une clé ou non
isKey :: Int -> Int -> Bool -> M.Map Coord Item -> Bool
isKey x y False map = case M.lookup (C.C x y) map of
    Just (Item Clef True) -> True
    otherwise -> False
isKey x y b map = False

-- Desactive l'affichage de l'item
changeItems :: Coord -> Type -> M.Map Coord Item -> M.Map Coord Item
changeItems c t map = M.insert c (Item t False) map