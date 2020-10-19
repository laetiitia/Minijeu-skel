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
    | Escalier
    | ErrorItem
    deriving (Eq)

instance Show Type where
    show(t)=typeToString t

data Item = Item {id :: Type, affichage :: Bool}

instance Show Item where
    show(Item id b) =  ((typeToString id) ++ " " ++(show b))


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
    Escalier -> "escalier"
    ErrorItem -> "ErrorItem"

-- Recupere le Type associé au string
stringToType :: String -> Type
stringToType str = case str of
    "epee" -> Epee
    "clef" -> Clef
    "tresor" -> Tresor
    "escalier" -> Escalier
    otherwise -> ErrorItem

-- Verifie que le string soit bien assimilé à un type (sauf ErrorItem)
stringIsType :: String -> Bool
stringIsType str = case str of
    "epee" -> True
    "clef" -> True
    "tresor" -> True
    "escalier" -> True
    otherwise -> False


-- Initialise les Items (sauf ErrorItem)
initItems :: [(String, Coord)] -> M.Map Coord Item
initItems []  = M.empty
initItems ((id,c):xs)  | (stringIsType id) = M.insert c (Item (stringToType id) True) (initItems xs)              
                           | otherwise = initItems xs

-- Reset les Items
reset :: (M.Map Coord Item) -> (M.Map Coord Item)
reset m =( M.map (\(Item x y) -> (Item x True)) m)

-- Precondition initItems: les coordonnées soient des multiples de 50 et que le string corresponde à un Type
prop_pre_initItems :: [(String,Coord)] -> Bool
prop_pre_initItems [] = True 
prop_pre_initItems ((id,(C.C x y)):xs)  | (stringIsType id) && ((mod x 50) == 0) && ((mod y 50) == 0) = prop_pre_initItems xs
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

-- Verifie si c'est un Tresor ou non
isTresor :: Int -> Int -> M.Map Coord Item -> Bool
isTresor x y map = case M.lookup (C.C x y) map of
    Just (Item Tresor _) -> True
    otherwise -> False

-- Verifie si c'est un Escalier ou non
isEscape :: Int -> Int -> M.Map Coord Item -> Bool
isEscape x y map = case M.lookup (C.C x y) map of
    Just (Item Escalier _) -> True
    otherwise -> False




-- Desactive l'affichage de l'item (passer à False)
changeItems :: Coord -> Type -> M.Map Coord Item -> M.Map Coord Item
changeItems c t map = M.insert c (Item t False) map


-- Verifie que l'item soit correcte
aux_pre_coord_prop :: Coord ->  M.Map Coord Item -> Bool
aux_pre_coord_prop c map = case M.lookup c map of
    Just i -> True
    otherwise -> False 

-- Verifie les coordonnées de la case de l'item
prop_pre_coord_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_pre_coord_changeItems c@(C.C x y) t map = (y>=0 && x>=0 && ((mod x 50) == 0) && ((mod y 50) == 0) && (aux_pre_coord_prop c map)) 

-- Verifie que la case corresponde au type de l'item que nous cherchons
prop_pre_exist_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_pre_exist_changeItems (C.C x y) t map = case M.lookup (C.C x y) map of
    Just (Item tp _) -> tp == t 
    otherwise -> False

-- PreCondition changeItems
prop_pre_changeItems :: Coord -> Type -> M.Map Coord Item -> Bool
prop_pre_changeItems c t map = (prop_pre_exist_changeItems c t map) && (prop_pre_coord_changeItems c t map)

-- PostCondition changeItems: Verifie si l'item est correct et que sont affichage a bien changer
prop_post_changeItems ::Coord -> Type -> M.Map Coord Item -> Bool
prop_post_changeItems c t m = let map = changeItems c t m in
    case M.lookup c map of
        Just (Item id aff) -> prop_inv_ItemType (Item id aff) && (not aff)
        otherwise -> False


-- Recupere les données du fichier texte pour initialiser la liste des items
readCarte :: String ->  M.Map Coord Item
readCarte [] = M.empty
readCarte txt = getItems txt 0 0
    where getItems (x:xs) cx cy = (if xs == [] 
                                    then M.empty
                                    else case x of
                                            'c' -> M.insert (C.C cx cy) (Item Clef True) (getItems xs (cx+50) cy)
                                            'e' -> M.insert (C.C cx cy) (Item Epee True) (getItems xs (cx+50) cy)
                                            't' -> M.insert (C.C cx cy) (Item Tresor True) (getItems xs (cx+50) cy)
                                            'E' -> M.insert (C.C cx cy) (Item Escalier True) (getItems xs (cx+50) cy)
                                            '\n' -> getItems xs 0 (cy+50) 
                                            otherwise -> getItems xs (cx+50) cy)

-- PostCondition de readCarte: que la liste des items soient corrects
prop_post_readCarte :: String -> Bool
prop_post_readCarte txt = M.foldr (\x y -> (prop_inv_ItemType x) &&  y) True (readCarte txt)