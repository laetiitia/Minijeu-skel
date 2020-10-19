module Sprite where

import Control.Monad.IO.Class (MonadIO)

import Foreign.C.Types (CInt)

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import SDL.Vect (V2 (..), Point (..))

import SDL.Video.Renderer (Renderer, Texture, Rectangle (..))
import qualified SDL.Video.Renderer as R

import TextureMap (TextureMap, TextureId)
import qualified TextureMap as TM

import qualified Debug.Trace as T


type Area = Rectangle CInt

data Image =
  Image { textureId :: TextureId
        , sourceArea :: Area }

-- | Les lutins sont associés à des images (textures)
createImage :: TextureId -> Area -> Image
createImage txt rct = Image txt rct

data Sprite =
  Sprite { images :: Seq Image
         , current :: Int
         , destArea :: Area }

-- | création d'un lutin "vide"
createEmptySprite :: Sprite
createEmptySprite = Sprite Seq.empty 0 (mkArea 0 0 0 0) 

-- | ajouter une image à un lutin
addImage :: Sprite -> Image -> Sprite
addImage sp@(Sprite { images=is }) img = sp { images = is :|> img }

-- | changer l'image d'un lutin  (par son numéro)
changeImage :: Sprite -> Int -> Sprite
changeImage sp@(Sprite { images = imgs }) new
  | Seq.null imgs = error $ "Cannot change sprite image, no image in sprite"
  | (new < 0) || (new > Seq.length imgs) = error $ "Cannot change sprite image, bad index: " <> (show new)
  | otherwise = sp { current= new }

-- | cycler l'image d'un lutin
cycleImage :: Sprite -> Sprite
cycleImage sp@(Sprite { images = imgs, current = cur }) =
  let new = if cur == Seq.length imgs - 1 then 0 else cur + 1
  in changeImage sp new

-- | une zone rectangulaire
mkArea :: CInt -> CInt -> CInt -> CInt -> Area
mkArea x y w h = Rectangle (P (V2 x y)) (V2 w h)

-- | déplacement d'une zone
moveArea :: Area -> CInt -> CInt -> Area
moveArea rect@(Rectangle _ wh) x y = Rectangle (P (V2 x y)) wh

-- | redimensionnement
resizeArea :: Area -> CInt -> CInt -> Area
resizeArea rect@(Rectangle p _) w h = Rectangle p (V2 w h)

-- | déplacement d'un lutin
moveTo :: Sprite -> CInt -> CInt -> Sprite
moveTo sp@(Sprite { destArea = dest }) x y = sp { destArea = moveArea dest x y }

-- | mise à l'échelle d'un lutin
scale :: Sprite -> CInt -> CInt -> Sprite
scale sp@(Sprite { destArea = dest}) w h = sp { destArea = resizeArea dest w h }

-- | récupération de l'image courante d'un lutin
currentImage :: Sprite -> Image
currentImage (Sprite imgs cur _) = Seq.index imgs cur

-- | échelle par défaut du lutin, en fonction de son image courante
defaultScale :: Sprite -> Sprite
defaultScale sp = case currentImage sp of
                    (Image _ (Rectangle _ (V2 w h))) -> scale sp w h

-- | affichage d'un lutin sur le `renderer` SDL2
displaySprite :: Renderer -> TextureMap -> Sprite -> IO ()
displaySprite rdr tmap sp@(Sprite imgs cur dest) =
  case currentImage sp of
    (Image tid src) -> do
      let txt = TM.fetchTexture tid tmap
      R.copy rdr txt Nothing (Just dest)


-- Defile les images de fin du jeux
scene :: Int -> Int -> String -> (String, Int, String)
scene 0 i s= case i of  
  0 -> ("Scene",(i+1),"Scene")
  1 -> ("Scene_0",(i+1),"Scene_0")
  2 -> ("Scene_1",(i+1),"Scene_1")
  3 -> ("Scene_2",(i+1),"Scene_2")
  4 -> ("Scene_3",(i+1),"Scene_3")
  5 -> ("Scene_4",(i+1),"Scene_4")
  otherwise -> ("endScene",(i+1),"endScene")
scene x i s= (s,i,s)