import Test.Hspec
import CarteSpec as CS
import ItemsSpec as IS
import ModelSpec as MODS
import MonsterSpec as MONS


main :: IO ()
main = hspec $ do
    CS.cFunSpec
    IS.cFunSpec
   -- MONS.cFunSpec
