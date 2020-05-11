module Items where

import qualified Data.Map.Strict as M
import Carte (Coord)
import Carte (Case)
import qualified Carte as C

-- Structures :
data Type =
    Epee
    | Clef
    | Tresor
    | ErrorItem
    deriving (Eq)


data Item = Item {id :: Type, affichage :: Bool}


-- Verifie si le type de l'item est correcte
prop_inv_ItemType :: Item -> Bool
prop_inv_ItemType (Item ErrorItem _) = False
prop_inv_ItemType (Item id _) = True




-- Fonctions : 

-- Recupere le string par rapport au type de l'item
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
                           | otherwise = M.empty
initItems ((id,(x,y)):xs)  | id == "clef" = M.insert (C.C x y) (Item Clef True) (initItems xs)
                           | id == "epee" = M.insert (C.C x y) (Item Epee True) (initItems xs)        
                           | id == "tresor" = M.insert (C.C x y) (Item Tresor True) (initItems xs)                   
                           | otherwise = initItems xs



prop_pre_initItems :: [(String,(Int,Int))] -> Bool
prop_pre_initItems ((id,(x,y)):[]) | id == "clef" && ((mod x 50) == 0) && ((mod y 50) == 0) = True
                             | id == "epee" && ((mod x 50) == 0) && ((mod y 50) == 0)  = True
                             | id == "tresor" && ((mod x 50) == 0) && ((mod y 50) == 0) = True
                             | otherwise = False
prop_pre_initItems ((id,(x,y)):xs) | id == "clef" && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_initItems xs
                             | id == "epee" && ((mod x 50) == 0) && ((mod y 50) == 0)  = prop_pre_initItems xs
                             | id == "tresor" && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_initItems xs
                             | otherwise = False

prop_post_initItems :: M.Map Coord Item -> Bool
prop_post_initItems m = M.foldr (\x y -> (prop_inv_ItemType x) &&  y) True m


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

aux_pre_coord_prop :: Coord ->  M.Map Coord Item -> Bool
aux_pre_coord_prop c map = case M.lookup c map of
    Just i -> True
    otherwise -> False 

prop_pre_coord_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_pre_coord_changeItems c@(C.C x y) t map = (y>=0 && x>=0 && ((mod x 50) == 0) && ((mod y 50) == 0) && (aux_pre_coord_prop c map)) 

prop_pre_exist_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_pre_exist_changeItems (C.C x y) t map = case M.lookup (C.C x y) map of
    Just (Item tp _) -> tp == t 
    otherwise -> False


prop_post_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_post_changeItems (C.C x y) t map = case M.lookup (C.C x y) map of
     Just i -> prop_inv_ItemType i
     otherwise -> False
