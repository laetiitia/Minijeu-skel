import Test.Hspec
import CarteSpec as CS
import ItemsSpec as IS
import ModelSpec as MODS
import MonsterSpec as MONS
import ItemsQuickCheck as IQC
import MonstreQuickCheck as MQC

main :: IO ()
main = hspec $ do
    CS.cFunSpec
    IS.cFunSpec
    MONS.cFunSpec
    MODS.cFunSpec
    IQC.cFunSpec
    MQC.cFunSpec

