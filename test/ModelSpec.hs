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
            GS.prop_pre_createGameState "T===T\n|Xp |\n=====" 0
            `shouldBe` True
        it "PreCondition:  Initialisation d'un gameState non valide " $ do 
            GS.prop_pre_createGameState "" 0
            `shouldBe` False
        it "PostCondition: Initialisation d'un gameState valide" $ do 
            GS.prop_post_createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0
            `shouldBe` True
        it "PostCondition: Initialisation d'un gameState non valide (1)" $ do 
            GS.prop_post_createGameState "T===T\n|Xp |\n======" 0
            `shouldBe` False
        it "PostCondition: Initialisation d'un gameState non valide (2)" $ do 
            GS.prop_post_createGameState "T==========T\n|  |\n===========" 0
            `shouldBe` False




movePersoSpec = do
    describe "Verifie le deplacement du personnage" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_move (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PreCondition non valide (position personnage)" $ do
            GS.prop_pre_move (GS.createGameState "T========T\n|        |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` False
        it "PreCondition non valide (item)" $ do
            let carte = (C.readCarte "T===T\n|X  |\n=====") in GS.prop_pre_move (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 155 150))]) (carte, 0, False) carte )
            `shouldBe` False
        it "PreCondition non valide (monstre)" $ do
            let carte = (C.readCarte "T===T\n|X  |\n=====") in GS.prop_pre_move (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 155 150),"Orc")]) (I.initItems [("epee",(C.C 150 150))]) (carte, 0, False) carte)
            `shouldBe` False
        it "PreCondition non valide" $ do
            GS.prop_pre_move GS.Victory
            `shouldBe` False
        it "PostCondition valide" $ do 
            GS.prop_post_move (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0) "down"
            `shouldBe` True
        it "PostCondition non valide " $ do 
            GS.prop_post_move (GS.createGameState "T========T\n|        |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0) "up"
            `shouldBe` False




moveMonsterSpec = do 
    describe "Verifie le deplacement des monstres" $ do
        it "PreCondition valide" $ do
           let carte = (C.readCarte "T===T\n|Xp |\n=====") in GS.prop_pre_moveMonsters 0 (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 150 150))])  (carte, 0, False) carte)
            `shouldBe` True
        it "PreCondition non valide (carte)" $ do
            GS.prop_pre_moveMonsters 0 (GS.createGameState  "T===T\n|Xp |\n======" 0)
            `shouldBe` False
        it "PreCondition non valide (gamestate)" $ do
            GS.prop_pre_moveMonsters 0 GS.Bug
            `shouldBe` False
        it "PostCondition valide (1)" $ do
            GS.prop_pre_moveMonsters 1 (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PostCondition valide (0)" $ do
            GS.prop_pre_moveMonsters 0 (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PostCondition non valide (carte)" $ do
            GS.prop_pre_moveMonsters 0 (GS.createGameState "T========T\n| X     n|\n==========" 0)
            `shouldBe` False



itemSpec = do 
    describe "Verifie le changement d'état des items" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_changeItems (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PreCondition non valide (item)" $ do
            let carte =  (C.readCarte "T===T\n|Xp |\n=====") in GS.prop_pre_changeItems (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 205 200))]) (carte,0,False) carte)
            `shouldBe` False
        it "PreCondition non valide (gameState)" $ do
            GS.prop_pre_changeItems GS.Bug
            `shouldBe` False
        it "PostCondition valide" $ do
            GS.prop_post_changeItems (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PostCondition non valide (item)" $ do
            let carte =  (C.readCarte "T===T\n|Xp |\n=====") in GS.prop_post_changeItems (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 205 200))]) (carte,0,False) carte)
            `shouldBe` False



monstresSpec = do 
    describe "Verifie le changement d'état des monstres" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_changeMonstres (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PreCondition non valide (monstre)" $ do
            let carte =  (C.readCarte "T===T\n|Xp |\n=====") in GS.prop_pre_changeMonstres (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 105 100),"Orc")]) (I.initItems [("epee",(C.C 150 150))]) (carte,0,False) carte)
            `shouldBe` False
        it "PreCondition non valide (gameState)" $ do
            GS.prop_pre_changeMonstres GS.Bug
            `shouldBe` False
        it "PostCondition valide" $ do
            GS.prop_post_changeMonstres (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PostCondition non valide (monstre)" $ do
            let carte =  (C.readCarte "T===T\n|Xp |\n=====") in GS.prop_post_changeMonstres (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 105 100),"Orc")]) (I.initItems [("epee",(C.C 150 150))]) (carte,0,False) carte)
            `shouldBe` False



portesSpec = do 
    describe "Verifie le changement d'état des portes" $ do
        it "PreCondition valide" $ do
            GS.prop_pre_activePorte (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PreCondition non valide (porte)" $ do
            let carte = (C.readCarte "T===T\n|Xn |\n=====") in GS.prop_pre_activePorte (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 150 150))])  (carte,0,False) carte)
            `shouldBe` False
        it "PreCondition non valide (gameState)" $ do
            GS.prop_pre_activePorte GS.Bug
            `shouldBe` False
        it "PostCondition valide " $ do
            GS.prop_post_activePorte (GS.createGameState "T========T\n| X      |\n|        |\n|        |\n|     o  |\n|        |\n|       E|\n==========" 0)
            `shouldBe` True
        it "PostCondition non valide (pas de porte trouvé)" $ do
            let carte = (C.readCarte "T===T\n| | |\n|Xp |\n| | |\n=====") in GS.prop_post_activePorte (GS.GameState 50 50 False False 50 (Mst.initMonstres [((C.C 100 100),"Orc")]) (I.initItems [("epee",(C.C 150 150))]) (carte,0,False) carte)
            `shouldBe` True




cFunSpec = do 
    initSpec
    movePersoSpec
    moveMonsterSpec
    itemSpec
    monstresSpec
    portesSpec
