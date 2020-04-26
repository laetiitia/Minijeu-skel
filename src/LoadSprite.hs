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


