
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import Carte (Carte)

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int 
                           , carte :: Carte}



initGameState :: Carte -> GameState
initGameState carte = GameState 50 50 25 carte

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp _) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp _) | px < 630 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp _) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp _) | py < 430 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate keyb deltaTime = moveAux gstate keyb deltaTime [KeycodeZ,KeycodeQ,KeycodeS,KeycodeD]


moveAux :: RealFrac a => GameState -> Keyboard -> a -> [Keycode] -> GameState 
moveAux gstate keyb deltaTime (x : []) = if K.keypressed x keyb then moveTo gstate x deltaTime else gstate
moveAux gstate keyb deltaTime (x : xs) = if K.keypressed x keyb then moveAux (moveTo gstate x deltaTime) keyb deltaTime xs
                                                                 else moveAux gstate keyb deltaTime xs 



moveTo :: RealFrac a => GameState -> Keycode -> a -> GameState
moveTo gstate KeycodeZ deltaTime = moveUp gstate
moveTo gstate KeycodeQ deltaTime = moveLeft gstate
moveTo gstate KeycodeS deltaTime = moveDown gstate
moveTo gstate KeycodeD deltaTime = moveRight gstate
moveTo gstate _ _ = gstate

