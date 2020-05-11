module MonsterSpec where

import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import qualified Carte as C
import Monster as Mon
import qualified Data.Map.Strict as M


initSpeck = do
    describe "Verifie l'initialisation des monstres" $ do
        it "liste Monstre valide" $
            property Mon.prop_initMonstres_inv 


cFunSpec = do 
    initSpeck