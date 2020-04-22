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

import Model (Monstre)
----
import qualified Data.Map.Strict as Map
import Carte (Carte)
import Carte (Coord)
import qualified Carte as Carte


--------------------------------------------
--------------- LOAD SPRITE ----------------
--------------------------------------------

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


loadangleBD :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadangleBD rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "angleBD") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "angleBD") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "angleBD") sprite smap
  return (tmap', smap')


loadangleBG :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadangleBG rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "angleBG") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "angleBG") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "angleBG") sprite smap
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


loadPorteNSO :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteNSO rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteNSO") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteNSO") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteNSO") sprite smap
  return (tmap', smap')


loadPorteNSF :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteNSF rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteNSF") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteNSF") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteNSF") sprite smap
  return (tmap', smap')
  

loadPorteEOOD :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEOOD rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteEOOD") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteEOOD") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteEOOD") sprite smap
  return (tmap', smap')


loadPorteEOOG :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEOOG rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteEOOG") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteEOOG") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteEOOG") sprite smap
  return (tmap', smap')

loadPorteEOFD :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEOFD rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteEOFD") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteEOFD") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteEOFD") sprite smap
  return (tmap', smap')


loadPorteEOFG :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEOFG rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteEOFG") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteEOFG") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteEOFG") sprite smap
  return (tmap', smap')

loadOrc :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadOrc rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Orc") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Orc") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Orc") sprite smap
  return (tmap', smap')





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
      

------------------------------------------
----------------- MAIN -------------------
------------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 700 500 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  --(tmap, smap) <- loadBackground renderer "assets/background.png" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" TM.createTextureMap SM.createSpriteMap
  -- chargement du virus
  (tmap4, smap4) <- loadangleBD renderer "assets/texture/angleBD.png" tmap' smap'
  (tmap5, smap5) <- loadangleBG renderer "assets/texture/angleBG.png" tmap4 smap4
  (tmap6, smap6) <- loadSol renderer "assets/texture/sol.png" tmap5 smap5
  (tmap7, smap7) <- loadHorizontal renderer "assets/texture/horizontal.png" tmap6 smap6
  (tmap8, smap8) <- loadVerticalD renderer "assets/texture/verticalD.png" tmap7 smap7
  (tmap9, smap9) <- loadVerticalG renderer "assets/texture/verticalG.png" tmap8 smap8
  (tmap10, smap10) <- loadPorteEOFG renderer "assets/texture/PorteEOFG.png" tmap9 smap9
  (tmap11, smap11) <- loadPorteEOOG renderer "assets/texture/PorteEOOG.png" tmap10 smap10
  (tmap12, smap12) <- loadPorteNSF renderer "assets/texture/walls.png" tmap11 smap11
  (tmap13, smap13) <- loadPorteNSO renderer "assets/texture/porteO.png" tmap12 smap12
  (tmap14, smap14) <- loadOrc renderer "assets/orc.png" tmap13 smap13
  (tmap15, smap15) <- loadPorteEOOD renderer "assets/texture/PorteEOOD.png" tmap14 smap14
  (tmap16, smap16) <- loadPorteEOFD renderer "assets/texture/PorteEOFD.png" tmap15 smap15


  -- initialisation de l'état du jeu
  -- let gameState = M.initGameState

  map <- readFile "assets/defaultMap.txt"
  let carte = let (x,y)= Carte.getFormat map in Carte.readCarte (x*50) (y*50) map

  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 10 renderer tmap16 smap16 kbd (M.initGameState carte) 0



affichagePerso :: Int -> Int -> Renderer -> TextureMap -> SpriteMap -> IO()
affichagePerso x y renderer tmap smap= S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                  (fromIntegral 350)
                                  (fromIntegral 250))


affichageMonstres :: Int -> Int -> [Monstre] -> Renderer -> TextureMap -> SpriteMap -> IO()
affichageMonstres px py ((M.Monster e (Carte.C x y) _ _) : []) renderer tmap smap= S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (M.especeToString e)) smap)
                                  (fromIntegral (x-px+350))
                                  (fromIntegral (y-py+250)))

------------------------------------------- 
refresh::[Event] -> Keyboard -> Keyboard
refresh [] kbd = kbd
refresh events kbd = K.handleEvent (head events) kbd

---------------- GAMELOOP------------------
-------------------------------------------

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState ->Int -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState@(M.GameState x y sp m carte) cpt= do
  startTime <- time
  events <- pollEvents  
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events
  let kbd' = refresh events kbd
  clear renderer


  affichageMap  x y (Map.toList (Carte.carte_contenu carte)) renderer tmap smap
  affichagePerso x y renderer tmap smap 
  affichageMonstres x y m renderer tmap smap
  present renderer
  let gameState' = M.mooveMonstre cpt gameState
  
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
