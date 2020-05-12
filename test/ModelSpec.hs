module ModelSpec where

import Test.Hspec

import Model (GameState)
import qualified Model as GS
import Carte (Case)
import Carte (Carte)
import Carte (Coord)
import qualified Carte as C


initSpec = do 
    describe "Verifie l'initialisation du GameState" $ do
        it "Initialisation d'un gameState valide" $ do 
            GS.prop_pre_InitGameState (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Initialisation d'un gameState non valide" $ do 
            GS.prop_pre_InitGameState (C.readCarte "T===T\n|c |\n=====")
            `shouldBe` False




cFunSpec = do 
    initSpec
