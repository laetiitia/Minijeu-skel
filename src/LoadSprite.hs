module LoadSprite where

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



loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 800 500)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')


loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')


loadangleT :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadangleT rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "angleT") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "angleT") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "angleT") sprite smap
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


loadVertical :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVertical rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Vertical") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Vertical") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Vertical") sprite smap
  return (tmap', smap')


loadPorteNS :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteNS rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteNS") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteNS") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteNS") sprite smap
  return (tmap', smap')


loadPorteEO :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPorteEO rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "PorteEO") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "PorteEO") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "PorteEO") sprite smap
  return (tmap', smap')
  

loadOrc :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadOrc rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Orc") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Orc") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Orc") sprite smap
  return (tmap', smap')

loadSkeleton :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSkeleton rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Skeleton") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Skeleton") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Skeleton") sprite smap
  return (tmap', smap')


loadPerso2 :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso2 rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso2") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso2") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "perso2") sprite smap
  return (tmap', smap')

loadEpee :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadEpee rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "epee") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "epee") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "epee") sprite smap
  return (tmap', smap')

loadClef :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadClef rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "clef") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "clef") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "clef") sprite smap
  return (tmap', smap')

loadTresor :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadTresor rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "tresor") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "tresor") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "tresor") sprite smap
  return (tmap', smap')

loadFantome :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadFantome rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "Fantome") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Fantome") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Fantome") sprite smap
  return (tmap', smap')
