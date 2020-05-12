module ModelSpec where

import Test.Hspec

import Model (GameState)
import qualified Model as GS
import Carte (Case)
import Carte (Carte)
import Carte (Coord)
import qualified Carte as C

import qualified Monster as Mst
import qualified Items as I


initSpec = do 
    describe "Verifie l'initialisation du GameState" $ do
        it "PreCondition: Initialisation d'un gameState valide" $ do 
            GS.prop_pre_InitGameState (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "PreCondition:  Initialisation d'un gameState non valide (carte)" $ do 
            GS.prop_pre_InitGameState (C.readCarte "T===T\n| |\n=====")
            `shouldBe` False
        it "PostCondition: Initialisation d'un gameState valide" $ do 
            GS.prop_post_InitGameState (C.readCarte "T========T\n|        |\n|        |\n|        |\n|        |\n|        |\n|        |\n==========")
            `shouldBe` True
        it "PostCondition: Initialisation d'un gameState non valide (1)" $ do 
            GS.prop_post_InitGameState (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` False
        it "PostCondition: Initialisation d'un gameState non valide (2)" $ do 
            GS.prop_post_InitGameState (C.readCarte "T==========T\n|  |\n===========")
            `shouldBe` False


movePersoSpec = do
    describe "Verifie le deplacement du personnage" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_move (GS.GameState 50 50 False False 50 (Mst.initMonstres [((100,100),"Orc")]) (I.initItems [("epee",(150,150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` True
        it "PreCondition non valide (position personnage)" $ do
            GS.prop_pre_move (GS.initGameState (C.readCarte "T===T\n|   |\n====="))
            `shouldBe` False
        it "PreCondition non valide (item)" $ do
            GS.prop_pre_move (GS.GameState 50 50 False False 50 (Mst.initMonstres [((100,100),"Orc")]) (I.initItems [("epee",((-150),150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` False
        it "PreCondition non valide (monstre)" $ do
            GS.prop_pre_move (GS.GameState 50 50 False False 50 (Mst.initMonstres [(((-100),100),"Orc")]) (I.initItems [("epee",(150,150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` False
        it "PostCondition valide" $ do 
            GS.prop_post_move (GS.initGameState (C.readCarte "T========T\n|        |\n|        |\n|        |\n|        |\n|        |\n|        |\n==========")) "down"
            `shouldBe` True
        it "PostCondition non valide " $ do 
            GS.prop_post_move (GS.initGameState (C.readCarte "T===T\n|   |\n=====")) "up"
            `shouldBe` False



moveMonsterSpec = do 
    describe "Verifie le deplacement des monstres" $ do
        it "PreCondition non valide" $ do
            GS.prop_pre_moveMonsters 0 (GS.initGameState (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` False



itemSpec = do 
    describe "Verifie le changement d'état des items" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_changeItems (GS.GameState 50 50 False False 50 (Mst.initMonstres [((100,100),"Orc")]) (I.initItems [("epee",(150,150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` True



monstresSpec = do 
    describe "Verifie le changement d'état des monstres" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_changeMonstres (GS.GameState 50 50 False False 50 (Mst.initMonstres [((100,100),"Orc")]) (I.initItems [("epee",(150,150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` True



portesSpec = do 
    describe "Verifie le changement d'état des portes" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_activePorte (GS.GameState 50 50 False False 50 (Mst.initMonstres [((100,100),"Orc")]) (I.initItems [("epee",(150,150))]) (C.readCarte "T===T\n| c |\n=====") (C.readCarte "T===T\n| c |\n====="))
            `shouldBe` True





cFunSpec = do 
    initSpec
    movePersoSpec
    moveMonsterSpec
    itemSpec
    monstresSpec
    portesSpec
