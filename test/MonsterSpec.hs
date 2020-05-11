module MonsterSpec where

import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import qualified Carte as C
import Monster as Mon
import qualified Data.Map.Strict as M

auxMonstres :: Int -> String
auxMonstres i =
    case i of
        1 -> "Orc"
        2 -> "Fantome"
        3 -> "Skeleton"
        x -> "error"

genMonstresOk :: Gen ((Int,Int),String)
genMonstresOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    i <- choose(1,3)
    return $ (((x*50),(y*50)),auxMonstres i)

prop_initMonstres_inv :: Property
prop_initMonstres_inv = forAll genMonstresOk $ Mon.propMonstreValide


initSpeck = do
    describe "Verifie l'initialisation des monstres" $ do
        it "liste Monstre valide" $
            property prop_initMonstres_inv 


cFunSpec = do 
    initSpeck