
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import System.Random as R
import System.IO.Unsafe
import Data.Map as M

data Case = Normal -- une case vide
  | Perso --id
  | Mur -- infranchissable (sauf pour les fantomes ...)
  deriving (Eq,Show)

data Coord = C {cx :: Int , cy :: Int} deriving (Eq,Show)

instance Ord Coord where
  (C x1 y1) <= (C x2 y2) = if y1 < y2 then True else if y1 > y2 then False else x1 <= x2

--instance Show Case where
--show x = "mur"

data Carte = Carte { cartel :: Int , -- largeur
                     carteh :: Int , -- hauteur
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte 
                     }
 -- deriving (Show Carte)


--randomInt :: Int -> Int -> Int
--randomInt min max = unsafePerformIO (R.getStdRandom (R.randomR (min, max)))

--initGameState :: GameState
--initGameState = GameState 200 300 (randomInt 0 540) (randomInt 0 380) 4



associate :: Char -> Case
associate x = case x of
  '*' -> Mur
  ' ' -> Model.Normal
  'X' -> Perso
  otherwise -> Mur
caseToName :: Case -> String
caseToName x = case x of
   Mur -> "mur"
   Model.Normal -> "sol"
   Perso -> "perso"

initCarte :: String -> Coord -> M.Map Coord Case -> M.Map Coord Case
initCarte [] _ acc = acc
initCarte (head:tail) (C x y) acc = if head == '\n' then initCarte tail (C 0 (y+50)) acc else initCarte tail (C (x+50) y) (M.insert (C x y) (associate head) acc)


initGameState :: Int -> Int -> String -> Carte
initGameState x y entre = Carte 200 300 (initCarte entre (C 0 0) M.empty)



{-
moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = moveAux gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD]


moveAux :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
moveAux gstate keyb deltaTime (x : []) = if K.keypressed x keyb then moveTo gstate x deltaTime else gstate
moveAux gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then moveAux (moveTo gstate x deltaTime) keyb deltaTime xs
                                                                 else moveAux gstate keyb deltaTime xs 


testTouch :: GameState -> Bool
testTouch gs@(GameState px py vx vy _)  =  px < vx + 80 &&
                                            px + 80 > vx &&
                                            py < vy + 80 &&
                                            80 + py > vy


moveVirus :: GameState -> Int -> Int -> GameState
moveVirus gs@(GameState _ _ vx vy _) nx ny = gs{virusX = nx , virusY = ny}


moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime = let newG = moveUp gstate in if testTouch newG then moveVirus newG (-100) (-100) else newG 
moveTo gstate KeycodeQ deltaTime = let newG = moveLeft gstate in if testTouch newG then moveVirus newG (-100) (-100) else newG
moveTo gstate KeycodeS deltaTime = let newG = moveDown gstate in if testTouch newG then moveVirus newG (-100) (-100) else newG
moveTo gstate KeycodeD deltaTime = let newG = moveRight gstate in if testTouch newG then moveVirus newG (-100) (-100) else newG
moveTo gstate _ _ = gstate

-}