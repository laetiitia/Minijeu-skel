import Test.Hspec
import CarteSpec as CS
import ItemsSpec as IS
import KeyboardSpec as KS
import LoadSpriteSpec as LS
import ModelSpec as MODS
import MonsterSpec as MONS
import SpriteMapSpec as SMS
import SpriteSpec as SS
import TextureMapSpec as TS


main :: IO ()
main = hspec $ do
    CS.cFunSpec
    IS.cFunSpec
 --   KS.cFunSpec
 --   LS.cFunSpec
--    MODS.cFunSpec
 --   MONS.cFunSpec
--    SMS.cFunSpec
 --   SS.cFunSpec
 --   TS.cFunSpec