{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import qualified Data.Map.Strict as Map

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

import Monster (Monstre)
import qualified Monster as Mst

import Outils (Outil)
import qualified Outils as O

import Carte (Carte)
import Carte (Coord)
import qualified Carte as Carte

import qualified LoadSprite as LS



------------------------------------------
----------------- MAIN -------------------
------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 700 500 }
  renderer <- createRenderer window (-1) defaultRenderer

  -- chargement des sprites
  (tmap', smap') <- LS.loadPerso renderer "assets/perso.png" TM.createTextureMap SM.createSpriteMap
  (tmap2, smap2) <- LS.loadangleT renderer "assets/texture/angleT.png" tmap' smap'
  (tmap3, smap3) <- LS.loadSol renderer "assets/texture/sol.png" tmap2 smap2
  (tmap4, smap4) <- LS.loadHorizontal renderer "assets/texture/Horizontal.png" tmap3 smap3
  (tmap5, smap5) <- LS.loadVertical renderer "assets/texture/Vertical.png" tmap4 smap4
  (tmap6, smap6) <- LS.loadPorteEO renderer "assets/texture/porteEO.png" tmap5 smap5
  (tmap7, smap7) <- LS.loadPorteNS renderer "assets/texture/porteNS.png" tmap6 smap6
  (tmap8, smap8) <- LS.loadOrc renderer "assets/orc.png" tmap7 smap7
  (tmap9, smap9) <- LS.loadPerso2 renderer "assets/perso2.png" tmap8 smap8
  (tmap10, smap10) <- LS.loadEpee renderer "assets/epee.png" tmap9 smap9
  (tmap11, smap11) <- LS.loadClef renderer "assets/clef.jpg" tmap10 smap10
  (tmap12, smap12) <- LS.loadSkeleton renderer "assets/skeleton.png" tmap11 smap11

  -- initialisation de l'état du jeu
  -- let gameState = M.initGameState

  map <- readFile "assets/defaultMap.txt"
  let carte = Carte.readCarte map

  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 10 renderer tmap12 smap12 kbd (M.initGameState carte) 0


-------------------------------------------
---------------- AFFICHAGE ----------------
-------------------------------------------
affichage ::Int -> Int -> Coord -> Carte.Case -> SpriteMap ->Sprite
affichage x2 y2 (Carte.C x y) cases smap = (S.moveTo (SM.fetchSprite (SpriteId (Carte.caseToName cases)) smap)
                                  (fromIntegral (x-x2+350))
                                  (fromIntegral (y-y2+250)))

affichageMap :: Int -> Int -> [(Coord,Carte.Case)] -> Renderer -> TextureMap -> SpriteMap -> IO ()
affichageMap x y ((coord,cases):[]) renderer tmap smap = S.displaySprite renderer tmap (affichage x y coord cases smap)
affichageMap x y ((coord,cases):tail) renderer tmap smap = do
  S.displaySprite renderer tmap (affichage x y coord cases smap)
  affichageMap x y tail renderer tmap smap
      

affichagePerso :: Int -> Int -> Bool -> Renderer -> TextureMap -> SpriteMap -> IO()
affichagePerso x y True renderer tmap smap= S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                  (fromIntegral 350)
                                  (fromIntegral 250))
affichagePerso x y False renderer tmap smap= S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso2") smap)
                                  (fromIntegral 350)
                                  (fromIntegral 250))


affichageMonstres :: Int -> Int -> [Monstre] -> Renderer -> TextureMap -> SpriteMap -> IO()
affichageMonstres px py [] renderer tmap smap = do {return ();}
affichageMonstres px py ((Mst.Monster e (Carte.C x y) _ _ True) : xs) renderer tmap smap = do { S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (Mst.especeToString e)) smap)
                                                                                                                      (fromIntegral (x-px+350))
                                                                                                                      (fromIntegral (y-py+250)));
                                                                                      affichageMonstres px py xs renderer tmap smap;}
affichageMonstres px py ((Mst.Monster e (Carte.C x y) _ _ _) : xs) renderer tmap smap = affichageMonstres px py xs renderer tmap smap

affichageOutils :: Int -> Int -> [(Coord,Outil)] -> Renderer -> TextureMap -> SpriteMap -> IO()
affichageOutils px py [] renderer tmap smap = do {return ();}
affichageOutils px py (((Carte.C x y),(O.Outil e True) ) : xs) renderer tmap smap = do {S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (O.typeToString e)) smap)
                                                                                                                    (fromIntegral (x-px+350))
                                                                                                                    (fromIntegral (y-py+250)));
                                                                                  affichageOutils px py xs renderer tmap smap;}
affichageOutils px py (((Carte.C x y),(O.Outil e _) ) : xs) renderer tmap smap = affichageOutils px py xs renderer tmap smap

------------------------------------------- 
refresh::[Event] -> Keyboard -> Keyboard
refresh [] kbd = kbd
refresh events kbd = K.handleEvent (head events) kbd





------------------------------------------
---------------- GAMELOOP------------------
-------------------------------------------

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState ->Int -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState@(M.GameState x y e c sp m o _ carte) cpt= do
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer


  affichageMap  x y (Map.toList (Carte.carte_contenu carte)) renderer tmap smap
  affichagePerso x y e renderer tmap smap 
  affichageMonstres x y m renderer tmap smap
  affichageOutils x y (Map.toList o) renderer tmap smap
  present renderer
  let gameState' = M.moveMonsters cpt gameState
  
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState'' = M.gameStep gameState' kbd' deltaTime
  
  unless (quit || (K.keypressed KeycodeEscape kbd')) (gameLoop frameRate renderer tmap smap K.createKeyboard gameState'' ((cpt+1) `mod` 10))
