
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import qualified Data.Map.Strict as M

-- CARTE
import Carte (Carte)
import Carte (Coord)
import Carte (Case)
import qualified Carte as C

-- MONSTERS
import Monster (Monstre)
import qualified Monster as Mst

-- ITEMS
import Items (Item)
import Items (Type)
import qualified Items as I
import Debug.Trace as T


data GameState = 
    Title {file :: String}
    | Victory
    | Bug {message :: String}
    | End { num :: Int
            , scene :: String
            , cpt :: Int}
    |GameState { persoX :: Int
                , persoY :: Int
                , epee :: Bool
                , clef :: Bool
                , speed :: Int 
                , monstres :: [Mst.Monstre]     --Liste des monstres du gamestate
                , items :: (M.Map Coord Item)   --Emplacement des Items
                , initCarte :: (Carte,Int,Bool) --CarteInitiale dans le cas ou on reset celle-ci sera récupérer
                , carte :: Carte                --Carte courante du gamestate
                }

---- INVARIANTS GAMESTATE ----

-- Verifie que:
-- Les cartes soient valides
-- La vitesse soit superieure à 0
prop_inv_GameState :: GameState -> Bool
prop_inv_GameState (GameState px py epee clef sp monstres items (init,n,_) c@(C.Carte l h start cont)) = sp>=0 && (prop_inv_Perso px py c) &&  (C.prop_inv_Carte init) && (C.prop_inv_Carte c) && (prop_inv_Monsters monstres) && (prop_inv_Items items l h) && (n>=0)
prop_inv_GameState (End num scene cpt) = (num >= 0) 
prop_inc_GameState _ = True

-- Verifie que: Les coordonnées du personnage soit bien comprise dans la carte
prop_inv_Perso :: Int -> Int -> Carte -> Bool
prop_inv_Perso px py (C.Carte larg haut start contenu) = px >= 0 && py >= 0 && px <= larg && py <= haut && ((mod px 50) == 0) &&((mod py 50) == 0)


-- Verifie que: Les monstres soient valides
prop_inv_Monsters :: [Mst.Monstre] -> Bool
prop_inv_Monsters monstres = C.listAnd (fmap Mst.prop_inv_Monstre monstres)

-- Verifie que: Les items soient valides
prop_inv_Items :: (M.Map Coord Item) -> Int -> Int -> Bool
prop_inv_Items items l h = let list =  M.foldrWithKey checkItem [] items in C.listAnd list
    where checkItem c item acc = ((I.prop_inv_ItemType item) && (C.prop_Coord c l h) ):acc


-------------------------------------------

-- Initialise l'état du jeu
createGameState :: String -> Int -> GameState
createGameState file niveau = let carte@(C.Carte _ _ (C.C x y) _) = C.readCarte file in  GameState x y False False 50 (Mst.readCarte file) (I.readCarte file) (carte,niveau,False) carte

-- PreCondition createGameState: la carte doit etre valide
prop_pre_createGameState :: String -> Int -> Bool
prop_pre_createGameState "" _ = False 
prop_pre_createGameState _  niveau = niveau >= 0

-- PostCondition create GameState: verifie que le gamestate est valide (invariant)
prop_post_createGameState :: String -> Int  -> Bool
prop_post_createGameState file niveau = let gs = createGameState file niveau in prop_inv_GameState gs




-----------------------------
-- *** DELACEMENT HERO *** --
-----------------------------
-- Deplace le perso vers la gauche
moveLeft :: GameState -> GameState
moveLeft gs@(GameState px py _ _ sp _ _ _ carte) | px > 0 && (C.caseAccesible (px - sp) py carte) = gs { persoX = px - sp }
                                | otherwise = gs

-- Deplace le perso vers la droite
moveRight :: GameState -> GameState
moveRight gs@(GameState px py _  _ sp _ _ _ (C.Carte l h start map)) | px < l && (C.caseAccesible (px + sp) py (C.Carte l h start map))= gs { persoX = px + sp }
                                 | otherwise = gs

-- Deplace le perso vers le haut                           
moveUp :: GameState -> GameState
moveUp gs@(GameState px py _ _ sp _ _ _ carte) | py > 0 && (C.caseAccesible px (py - sp) carte)= gs { persoY = py - sp }
                              | otherwise = gs

-- Deplace le perso vers le bas
moveDown :: GameState -> GameState
moveDown gs@(GameState px py _ _ sp _ _ _ (C.Carte l h start map)) | py < h && (C.caseAccesible px (py + sp) (C.Carte l h start map)) = gs { persoY = py + sp }
                                | otherwise = gs


-- PreCondition move_ : Les coordonnées du personnage doit etre comprise dans la carte et la vitesse est un entier positif
prop_pre_move :: GameState -> Bool
prop_pre_move gs@(GameState _ _ _ _ _ _ _ _ _) = prop_inv_GameState gs
prop_pre_move _  = False

-- PostCondition move_ : verifie que le gamestate soit valide selon l'action du move
prop_post_move :: GameState -> String -> Bool
prop_post_move gs dir = case dir of
    "up" ->  prop_inv_GameState (moveUp gs)
    "down" ->  prop_inv_GameState (moveDown gs)
    "right" ->  prop_inv_GameState (moveRight gs)
    "left" ->  prop_inv_GameState (moveLeft gs)
    otherwise -> False


------------------------------
-- *** AUTRE DELACEMENT *** --
------------------------------

-- Filtre les monstres qui se déplacent vers une case non vide ou si un monstre est a la meme position qu'un autre monstre
-- Cela ne s'applique pas pour les fantomes (m1 etant les nouvelles coordonnées des monstres et m2 les anciennes)
filterMonsters :: [Mst.Monstre] -> [Mst.Monstre] -> C.Carte -> [C.Coord] -> [Mst.Monstre]
filterMonsters [] [] _ _ = []
filterMonsters [] ms _ _ = ms
filterMonsters ms [] _ _ = ms
filterMonsters ( m1@(Mst.Monster esp (C.C x y) dir cpt aff):ms1) ( m2@(Mst.Monster _ c _ _ _):ms2) carte@(C.Carte _ _ _ contenu) acc = case esp of
    Mst.Fantome -> m1 : (filterMonsters ms1 ms2 carte ((C.C x y):acc))
    otherwise -> if (C.caseAccesible x y carte) && (not (foldr (||) False (fmap (\val -> val == (C.C x y)) acc))) 
                    then m1 : (filterMonsters ms1 ms2 carte ((C.C x y):acc)) 
                    else (Mst.Monster esp c dir cpt aff) : (filterMonsters ms1 ms2 carte ((C.C x y):acc) )


-- Deplace les monstres lorsque le compteurs est à 0
moveMonsters :: Int -> GameState -> GameState
moveMonsters 0 gs@(GameState _ _ _ _ _ ms _ _ carte) = gs { monstres = (filterMonsters (fmap Mst.moveMonster ms) ms carte [])}
moveMonsters _ gs = gs

-- PreCondition moveMonsters : le gamestate doit etre valide
prop_pre_moveMonsters :: Int -> GameState -> Bool
prop_pre_moveMonsters i gs@(GameState _ _ _ _ _ _ _ _ _) = (prop_inv_GameState gs) && i >= 0
prop_pre_moveMonsters _ _ = False

-- PostCondition moveMonsters : le gamestate doit etre valide
prop_post_moveMonsters :: Int -> GameState -> Bool
prop_post_moveMonsters cpt gs = prop_inv_GameState (moveMonsters cpt gs)


-------------------------------
-- *** MODIFICATION ETAT *** --
--------------------------------
changeLevel :: (Carte,Int,Bool) -> (Carte,Int,Bool)
changeLevel (carte,num,b) = (carte,num,True)


-- Equipe le heros des items se trouvant sur mla meme case que lui
changeItems :: GameState -> GameState
changeItems gs@(GameState px py e c _ _ items init _)  | (I.isSword px py e items || I.isKey px py c items) = let (I.Item id aff) = M.findWithDefault (I.Item I.ErrorItem False) (C.C px py) items in case id of
                                                                                                                                                    I.Epee -> let o = (I.changeItems (C.C px py) I.Epee items) in gs { epee = True, items = o }
                                                                                                                                                    I.Clef -> let o = (I.changeItems (C.C px py) I.Clef items) in gs { clef = True, items = o }
                                                                                                                                                    otherwise -> gs
                                                    | (I.isEscape px py items) = gs{initCarte = (changeLevel init)}
                                                    | (I.isTresor px py items) = End 0 "" 0
                                                    |otherwise = gs

-- PreCondition changeItems : le gamestate doit etre valide
prop_pre_changeItems :: GameState -> Bool
prop_pre_changeItems gs@(GameState _ _ _ _ _ _ _ _ _) = prop_inv_GameState gs
prop_pre_changeItems _ = False

-- PostCondition changeItems : le gamestate doit etre valide
prop_post_changeItems :: GameState -> Bool
prop_post_changeItems gs = prop_inv_GameState (changeItems gs)

resetLevel :: Carte -> [Mst.Monstre] -> (M.Map Coord Item) -> Int -> GameState
resetLevel carte@(C.Carte _ _ (C.C x y) _) monstres items niveau = GameState x y False False 50 (Mst.reset monstres) (I.reset items) (carte,niveau,False) carte


-- S'il y a une collision avec un des monstres alors celui-ci sera éliminé si le personnage a une épée
changeMonstres :: GameState -> GameState
changeMonstres gs@(GameState px py True _ _ monstres _ _ _) | Mst.collisionMonstres px py monstres = let m = (Mst.elimineMonstres px py monstres) in gs { monstres = m, epee = False }
                                                            | otherwise = gs
changeMonstres gs@(GameState px py False _ _ monstres items (init,niv,_) _) | Mst.collisionMonstres px py monstres =  resetLevel init monstres items niv
                                                               |otherwise = gs
changeMonstres gs = gs

-- PreCondition changeItems : le gamestate doit etre valide
prop_pre_changeMonstres :: GameState -> Bool
prop_pre_changeMonstres gs@(GameState _ _ _ _ _ _ _ _ _) = prop_inv_GameState gs
prop_pre_changeMonstres _ = False

-- PostCondition changeItems : le gamestate doit etre valide
prop_post_changeMonstres :: GameState -> Bool
prop_post_changeMonstres gs = prop_inv_GameState (changeMonstres gs)



-- Permet d'ouvrir une porte si le personnage a une clef
activePorte :: GameState -> GameState
activePorte gs@(GameState px py _ True sp _ _ _ carte) =let (carte2, b)=(C.openDoor px py carte) in if b then gs{carte = carte2, clef = False} else gs
activePorte gs = gs

-- PreCondition activePorte : le gamestate doit etre valide
prop_pre_activePorte :: GameState -> Bool
prop_pre_activePorte gs@(GameState _ _ _ _ _ _ _ _ _) = prop_inv_GameState gs
prop_pre_activePorte _ = False

-- PostCondition changeItems : le gamestate doit etre valide
prop_post_activePorte :: GameState -> Bool
prop_post_activePorte gs = prop_inv_GameState (activePorte gs)




--------------------------------
-- *** DELACEMENT GENERAL *** --
--------------------------------
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = getEvent gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD,KeycodeK]

-- Gere l'événement liée au clavier
getEvent :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
getEvent gstate keyb deltaTime (x : []) = if K.keypressed x keyb then changeMonstres (moveTo gstate x deltaTime) else (changeMonstres gstate)
getEvent gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then getEvent (changeMonstres (moveTo gstate x deltaTime)) keyb deltaTime xs
                                                                 else getEvent (changeMonstres gstate) keyb deltaTime xs 

-- Repercution de l'évenement sur les elements de la gamestate
moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime =  changeItems (moveUp gstate)
moveTo gstate KeycodeQ deltaTime =  changeItems (moveLeft gstate)
moveTo gstate KeycodeS deltaTime =  changeItems (moveDown gstate)
moveTo gstate KeycodeD deltaTime =  changeItems (moveRight gstate)
moveTo gstate KeycodeK deltaTime =  activePorte gstate
moveTo gstate _ _ = gstate
