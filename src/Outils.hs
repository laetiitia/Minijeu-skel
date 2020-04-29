module Outils where

import SDL

import qualified Data.Map.Strict as M
import Carte (Coord)
import Carte (Case)
import qualified Carte as C


data Type =
    Epee
    | Clef
    | ErrorOutil


data Outil = Outil {id :: Type, affichage :: Bool}


typeToString :: Type -> String
typeToString id = case id of
    Epee -> "epee"
    Clef -> "clef"
    ErrorOutil -> "ErrorOutil"

-- Initialise les outils
initOutils :: [(String,(Int,Int))] -> M.Map Coord Outil
initOutils ((id,(x,y)):[]) | id == "clef" = M.singleton (C.C x y) (Outil Clef True)
                           | id == "epee" = M.singleton (C.C x y) (Outil Epee True)
initOutils ((id,(x,y)):xs) | id == "clef" = M.insert (C.C x y) (Outil Clef True) (initOutils xs)
                           | id == "epee" = M.insert (C.C x y) (Outil Epee True) (initOutils xs)                          



-- Verifie si c'est une épée ou non 
isSword :: Int -> Int -> M.Map Coord Outil -> Bool
isSword x y map = case M.lookup (C.C x y) map of
    Just (Outil Epee True) -> True
    otherwise -> False

-- Verifie si c'est une clé ou non
isKey :: Int -> Int -> M.Map Coord Outil -> Bool
isKey x y map = case M.lookup (C.C x y) map of
    Just (Outil Clef True) -> True
    otherwise -> False


changeOutils :: Coord -> Type -> M.Map Coord Outil -> M.Map Coord Outil
changeOutils c t map = M.insert c (Outil t False) map