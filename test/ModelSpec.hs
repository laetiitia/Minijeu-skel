module ModelSpec where

import Test.Hspec

import Model (GameState)
import qualified Model as GS


initSpec = do 
    describe "Verifie l'initialisation du GameState" $ do
        it "" $ do 
            GS.prop_TailleCarte (C.Carte 100 100 M.empty)
            `shouldBe` False




cFunSpec = do 
    initSpec
