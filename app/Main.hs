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

import Data.Map as Map

import Model (Carte)
import Model (Coord)
import qualified Model as M
import System.Random as R

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1280 720)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadVirus :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "mur") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "mur") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "mur") sprite smap
  return (tmap', smap')


loadSol :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSol rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "sol") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "sol") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "sol") sprite smap
  return (tmap', smap')



main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1280 720 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.jpg" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap
  -- chargement du virus
  (tmap'', smap'') <- loadVirus renderer "assets/angle_wall2.png" tmap' smap'
  (tmap3, smap3) <- loadSol renderer "assets/sol.png" tmap'' smap''
  x <- randomRIO (0 ,540)
  y <- randomRIO (0 ,380)
  -- initialisation de l'état du jeu
  map <- readFile "assets/map1.txt"
  let carte = M.initGameState x y map
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap3 smap3 kbd carte

  --print (M.carte_contenu carte) 


affichage ::Coord -> M.Case -> SpriteMap ->Sprite
affichage (M.C x y) cases smap= (S.moveTo (SM.fetchSprite (SpriteId (M.caseToName cases)) smap)
                                 (fromIntegral x)
                                 (fromIntegral y))


affichageMap :: [(M.Coord,M.Case)] -> Renderer -> TextureMap -> SpriteMap -> IO ()
affichageMap ((coord,cases):[]) renderer tmap smap = S.displaySprite renderer tmap (affichage coord cases smap)
affichageMap ((coord,cases):tail) renderer tmap smap =do
  S.displaySprite renderer tmap (affichage coord cases smap)
  affichageMap tail renderer tmap smap

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Carte -> IO ()
gameLoop frameRate renderer tmap smap kbd carte = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer

  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  {--- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))
  ---
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap)
                                 (fromIntegral (M.virusX gameState))
                                 (fromIntegral (M.virusY gameState)))
  -}
  affichageMap (Map.toList (M.carte_contenu carte)) renderer tmap smap 
  present renderer
  --endTime <- time
  --let refreshTime = endTime - startTime
  --let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  --threadDelay $ delayTime * 1000 -- microseconds
  --endTime <- time
  --let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  --let gameState' = M.gameStep gameState kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' carte)
