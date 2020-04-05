{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

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

----
import qualified Data.Map.Strict as Map
import Carte (Carte)
import Carte (Coord)
import qualified Carte as Carte


--------------------------------------------
--------------- LOAD SPRITE ----------------
--------------------------------------------

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 700 500)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')


loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')


loadangleHD :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadangleHD rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "angleHD") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "angleHD") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "angleHD") sprite smap
  return (tmap', smap')


loadangleHG :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadangleHG rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "angleHG") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "angleHG") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "angleHG") sprite smap
  return (tmap', smap') 


loadSol :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSol rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "sol") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "sol") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "sol") sprite smap
  return (tmap', smap')

loadHorizontal :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadHorizontal rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Horizontal") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Horizontal") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Horizontal") sprite smap
  return (tmap', smap')

loadVerticalD :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVerticalD rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "VerticalD") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "VerticalD") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "VerticalD") sprite smap
  return (tmap', smap')

loadVerticalG :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVerticalG rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "VerticalG") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "VerticalG") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "VerticalG") sprite smap
  return (tmap', smap')

-------------------------------------------
---------------- AFFICHAGE ----------------
-------------------------------------------
affichage ::Coord -> Carte.Case -> SpriteMap ->Sprite
affichage (Carte.C x y) cases smap = (S.moveTo (SM.fetchSprite (SpriteId (Carte.caseToName cases)) smap)
                                  (fromIntegral x)
                                  (fromIntegral y))

affichageMap :: [(Coord,Carte.Case)] -> Renderer -> TextureMap -> SpriteMap -> IO ()
affichageMap ((coord,cases):[]) renderer tmap smap = S.displaySprite renderer tmap (affichage coord cases smap)
affichageMap ((coord,cases):tail) renderer tmap smap = do
  S.displaySprite renderer tmap (affichage coord cases smap)
  affichageMap tail renderer tmap smap
      

------------------------------------------
----------------- MAIN -------------------
------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 700 500 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.png" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap
  -- chargement du virus
  (tmap4, smap4) <- loadangleHD renderer "assets/texture/angleHD.png" tmap' smap'
  (tmap5, smap5) <- loadangleHG renderer "assets/texture/angleHG.png" tmap4 smap4
  (tmap6, smap6) <- loadSol renderer "assets/texture/sol.png" tmap5 smap5
  (tmap7, smap7) <- loadHorizontal renderer "assets/texture/Horizontal.png" tmap6 smap6
  (tmap8, smap8) <- loadVerticalD renderer "assets/texture/verticalD.png" tmap7 smap7
  (tmap9, smap9) <- loadVerticalG renderer "assets/texture/VerticalG.png" tmap8 smap8
  -- initialisation de l'état du jeu
  -- let gameState = M.initGameState

  map <- readFile "assets/defaultMap.txt"
  let carte = Carte.readCarte 500 350 map

  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap9 smap9 kbd carte 





-------------------------------------------
---------------- GAMELOOP------------------
-------------------------------------------

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Carte -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState = do
  startTime <- time
  events <- pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)

  --- display perso 
 {- S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState))) -}
  ---
  
  affichageMap (Map.toList (Carte.carte_contenu gameState)) renderer tmap smap 
  present renderer

  {-
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  ---}
  unless (quit || (K.keypressed KeycodeEscape kbd')) (gameLoop frameRate renderer tmap smap kbd' gameState)
