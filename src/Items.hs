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

---- INVARIANT ITEM ----

-- Verifie si le type de l'item est correcte
prop_inv_ItemType :: Item -> Bool
prop_inv_ItemType (Item ErrorItem _) = False
prop_inv_ItemType (Item id _) = True




---- ITEM FONCTIONS ----

-- Recupere le string associé au Type de l'item
typeToString :: Type -> String
typeToString id = case id of
    Epee -> "epee"
    Clef -> "clef"
    Tresor -> "tresor"
    ErrorItem -> "ErrorItem"

-- Recupere le Type associé au string
stringToType :: String -> Type
stringToType str = case str of
    "epee" -> Epee
    "clef" -> Clef
    "tresor" -> Tresor
    otherwise -> ErrorItem

-- Verifie que le string soit bien assimilé à un type (sauf ErrorItem)
stringIsType :: String -> Bool
stringIsType str = case str of
    "epee" -> True
    "clef" -> True
    "tresor" -> True
    otherwise -> False


-- Initialise les Items (sauf ErrorItem)
initItems :: [(String,(Int,Int))] -> M.Map Coord Item
initItems []  = M.empty
initItems ((id,(x,y)):xs)  | (stringIsType id) = M.insert (C.C x y) (Item (stringToType id) True) (initItems xs)              
                           | otherwise = initItems xs

-- Precondition initItems: les coordonnées soient des multiples de 50 et que le string corresponde à un Type
prop_pre_initItems :: [(String,(Int,Int))] -> Bool
prop_pre_initItems [] = True 
prop_pre_initItems ((id,(x,y)):xs)  | (stringIsType id) && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_initItems xs
                                    | otherwise = False

-- Postcondition initItems
prop_post_initItems :: M.Map Coord Item -> Bool
prop_post_initItems m = M.foldr (\x y -> (prop_inv_ItemType x) &&  y) True m



-- Verifie si c'est une épée ou non 
isSword :: Int -> Int -> Bool -> M.Map Coord Item -> Bool
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



-- Desactive l'affichage de l'item (passer à False)
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

-- PostCondition changeItems: Verifie si l'item est correct et que sont affichage a bien changer
prop_post_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_post_changeItems c t m = let map = changeItems c t m in
    case M.lookup c map of
        Just (Item id aff) -> prop_inv_ItemType (Item id aff) && (not aff)
        otherwise -> False
