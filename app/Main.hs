{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import GHC.IO.Exception (IOException(..))

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

import Items (Item)
import qualified Items as I

import Carte (Carte, Coord)
import qualified Carte as Carte



------------- LoadSprite --------------

loadScene :: String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadScene scene rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId scene) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId scene) (S.mkArea 0 0 800 500)
  let smap' = SM.addSprite (SpriteId scene) sprite smap
  return (tmap', smap')

loadSprite :: String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSprite spr rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId spr) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId spr) (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId spr) sprite smap
  return (tmap', smap')



------------------------------------------
----------------- MAIN -------------------
------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 800 500 }
  renderer <- createRenderer window (-1) defaultRenderer

  -- chargement des sprites
  (tmap, smap) <- loadScene "background" renderer "assets/background.png" TM.createTextureMap SM.createSpriteMap
  (tmap0, smap0) <- loadScene "endScene" renderer "assets/endScene.png" tmap smap
  (tmap1, smap1) <- loadSprite "perso" renderer "assets/perso.png" tmap0 smap0
  (tmap2, smap2) <- loadSprite "angleT" renderer "assets/textures/angleT.png" tmap1 smap1
  (tmap3, smap3) <- loadSprite "sol" renderer "assets/textures/sol.png" tmap2 smap2
  (tmap4, smap4) <- loadSprite "Horizontal" renderer "assets/textures/Horizontal.png" tmap3 smap3
  (tmap5, smap5) <- loadSprite "Vertical" renderer "assets/textures/Vertical.png" tmap4 smap4
  (tmap6, smap6) <- loadSprite "PorteEO" renderer "assets/textures/porteEO.png" tmap5 smap5
  (tmap7, smap7) <- loadSprite "PorteNS" renderer "assets/textures/porteNS.png" tmap6 smap6
  (tmap8, smap8) <- loadSprite "Orc" renderer "assets/monsters/orc.png" tmap7 smap7
  (tmap9, smap9) <- loadSprite "perso2" renderer "assets/perso2.png" tmap8 smap8
  (tmap10, smap10) <- loadSprite "epee" renderer "assets/items/epee.png" tmap9 smap9
  (tmap11, smap11) <- loadSprite "clef" renderer "assets/items/clef.png" tmap10 smap10
  (tmap12, smap12) <- loadSprite "Skeleton" renderer "assets/monsters/skeleton.png" tmap11 smap11
  (tmap13, smap13) <- loadSprite "Fantome" renderer "assets/monsters/fantome.png" tmap12 smap12
  (tmap14, smap14) <- loadSprite "tresor" renderer "assets/items/tresor.png" tmap13 smap13
  (tmap15, smap15) <- loadSprite "escalier" renderer "assets/items/staircase.png" tmap14 smap14
  (tmap16, smap16) <- loadScene "bug" renderer "assets/bug.jpg" tmap15 smap15
  (tmap17, smap17) <- loadScene "Scene_0" renderer "assets/Scene_0.jpg" tmap16 smap16
  (tmap18, smap18) <- loadSprite "Demon" renderer "assets/monsters/demon.png" tmap17 smap17
  (tmap19, smap19) <- loadSprite "Undead" renderer "assets/monsters/undead.png" tmap18 smap18
  (tmap20, smap20) <- loadScene "Scene_1" renderer "assets/Scene_1.jpg" tmap19 smap19
  (tmap21, smap21) <- loadScene "Scene_2" renderer "assets/Scene_2.jpg" tmap20 smap20
  (tmap22, smap22) <- loadScene "Scene_3" renderer "assets/Scene_3.jpg" tmap21 smap21
  (tmap23, smap23) <- loadScene "Scene_4" renderer "assets/Scene_4.png" tmap22 smap22
  (tmap24, smap24) <- loadScene "Scene" renderer "assets/Scene.jpg" tmap23 smap23

  file <- readFile "assets/monde0.txt"
  

  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 10 renderer tmap24 smap24 kbd (M.Title file) 0
  SDL.destroyWindow window
  SDL.quit


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

affichageItems :: Int -> Int -> [(Coord,Item)] -> Renderer -> TextureMap -> SpriteMap -> IO()
affichageItems px py [] renderer tmap smap = do {return ();}
affichageItems px py (((Carte.C x y),(I.Item e True) ) : xs) renderer tmap smap = do {S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (I.typeToString e)) smap)
                                                                                                                    (fromIntegral (x-px+350))
                                                                                                                    (fromIntegral (y-py+250)));
                                                                                  affichageItems px py xs renderer tmap smap;}
affichageItems px py (((Carte.C x y),(I.Item e _) ) : xs) renderer tmap smap = affichageItems px py xs renderer tmap smap

------------------------------------------- 
refresh::[Event] -> Keyboard -> Keyboard
refresh [] kbd = kbd
refresh events kbd = K.handleEvent (head events) kbd





------------------------------------------
---------------- GAMELOOP------------------
-------------------------------------------

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState ->Int -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState@(M.Title file) cpt = do  -- TITLE SCENE
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  present renderer
  unless (quit || (K.keypressed KeycodeEscape kbd')) $ do
    if (K.keypressed KeycodeReturn kbd') 
      then (gameLoop frameRate renderer tmap smap K.createKeyboard (M.createGameState file 0) cpt)  
      else (gameLoop frameRate renderer tmap smap K.createKeyboard gameState cpt)

gameLoop frameRate renderer tmap smap kbd gameState@(M.GameState x y e c sp m o (_,n,False) carte) cpt= do -- GAMESTATE SCENE
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer

  affichageMap  x y (Map.toList (Carte.carte_contenu carte)) renderer tmap smap
  affichagePerso x y e renderer tmap smap 
  affichageMonstres x y m renderer tmap smap
  affichageItems x y (Map.toList o) renderer tmap smap
  present renderer
  let gameState' = M.moveMonsters cpt gameState
  
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime

  --- update du game state
  let gameState'' = M.gameStep gameState' kbd' deltaTime
  unless (quit || (K.keypressed KeycodeEscape kbd')) (gameLoop frameRate renderer tmap smap K.createKeyboard gameState'' ((cpt+1) `mod` 10))

-- Changement de niveau 
gameLoop frameRate renderer tmap smap kbd gameState@(M.GameState _ _ _ _ _ _ _ (_,n,True) _) cpt = do -- NEXT LEVEL UPLOAD
  putStrLn ("assets/monde"++(show (n+1)) ++".txt") 
  file <- liftIO $ try $ readFile ("assets/monde"++(show (n+1)) ++".txt")
  case file :: Either IOException String of 
    Left exception -> gameLoop frameRate renderer tmap smap kbd (M.Bug "ERROR: Fichier du prochain niveau non trouvé.") cpt
    Right contents -> (do if (Carte.prop_post_readCarte contents)
                              then gameLoop frameRate renderer tmap smap kbd (M.createGameState contents (n+1)) (cpt+1) 
                              else gameLoop frameRate renderer tmap smap kbd (M.Bug "ERROR: Carte du niveau suivant incorrect.") cpt)
    
gameLoop frameRate renderer tmap smap kbd (M.Bug message) cpt = do  -- BUG SCENE
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "bug") smap)
  putStrLn $ message
  present renderer
  unless (quit || (K.keypressed KeycodeEscape kbd')) $ gameLoop frameRate renderer tmap smap kbd (M.Bug message) cpt


gameLoop frameRate renderer tmap smap kbd (M.End n s c) cpt = do  -- END SCENE
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer
  let (scene,res, sceneinit ) = S.scene c n s
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId scene) smap)
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  present renderer
  unless (quit || (K.keypressed KeycodeEscape kbd')) $ gameLoop frameRate renderer tmap smap kbd (M.End res sceneinit ((c+1) `mod` 35)) cpt