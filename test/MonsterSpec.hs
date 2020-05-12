module MonsterSpec where

import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import qualified Carte as C
import Monster as Mon
import qualified Data.Map.Strict as M


invSpec = do
    describe "Verifie les invariants d'un monstre" $ do
        it "Monstre valide (coordonnées)" $ do
            Mon.prop_inv_coord_monstre (Monster Orc (C.C 50 50) 0 3 True)
            `shouldBe` True
        it "Monstre valide (direction)" $ do
            Mon.prop_inv_direction_monstre (Monster Orc (C.C 50 50) 0 3 True)
            `shouldBe` True
        it "Monstre valide (compteur de pas)" $ do
            Mon.prop_inv_cpt_monstre (Monster Orc (C.C 50 50) 0 3 True)
            `shouldBe` True
        it "Monstre non valide (coordonnées)" $ do
            Mon.prop_inv_coord_monstre (Monster Orc (C.C 15 50) 0 3 True)
            `shouldBe` False
        it "Monstre non valide (direction)" $ do
            Mon.prop_inv_direction_monstre (Monster Orc (C.C 50 50) (-1) 3 True)
            `shouldBe` False
        it "Monstre non valide (compteur de pas)" $ do
            Mon.prop_inv_cpt_monstre (Monster Orc (C.C 50 50) 0 6 True)
            `shouldBe` False

initSpec = do 
    describe "Verifie que l'initialisation des monstres soit correcte" $ do
        it "PreCondition à l'initialisation des monstres valides (1)" $ do
            Mon.prop_pre_initMonstres []
            `shouldBe` True
        it "PreCondition à l'initialisation des monstres valides (2)" $ do
            Mon.prop_pre_initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")]
            `shouldBe` True
        it "Post Condition à l'initialisation des monstres valides (1)" $ do 
            Mon.prop_post_initMonstres []
            `shouldBe` True
        it "Post Condition à l'initialisation des monstres valides (2)" $ do 
            Mon.prop_post_initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")]
            `shouldBe` True
        it "Post Condition à l'initialisation des monstres (avec une Espece non valide)" $ do 
            Mon.prop_post_initMonstres [((0,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Monstre")]
            `shouldBe` True
        it "PreCondition à l'initialisation des monstres non valides (Espece)" $ do
            Mon.prop_pre_initMonstres [((150,250), "Orc"), ((500,350), "Monstre")]
            `shouldBe` False
        it "PreCondition à l'initialisation des monstres non valides (Coordonnées)" $ do
            Mon.prop_pre_initMonstres [((150,250), "Orc"), ((55,50), "Fantome")]
            `shouldBe` False
        it "Post Condition à l'initialisation des monstres non valides (1)" $ do 
            Mon.prop_post_initMonstres [((0,250), "Orc"), (((-50),50), "Fantome")]
            `shouldBe` False   
        it "Post Condition à l'initialisation des monstres non valides (2)" $ do 
            Mon.prop_post_initMonstres [((0,250), "Orc"), ((505,350), "Fantome")]
            `shouldBe` False      
    
moveSpec = do 
    describe "Verifie le deplacement d'un monstre (moveMonstre)" $ do
        it "PreCondition au deplacement d'un monstre valide" $ do
            Mon.prop_pre_moveMonster (Monster Orc (C.C 50 50) 0 3 True)
            `shouldBe` True
        it "PostCondition au deplacement d'un monstre valide" $ do
            Mon.prop_post_moveMonster (Monster Orc (C.C 50 50) 0 3 True)
            `shouldBe` True
        it "PreCondition au deplacement d'un monstre non valide (Coordonnées)" $ do
            Mon.prop_pre_moveMonster (Monster Orc (C.C 50 55) 0 3 True)
            `shouldBe` False
        it "PreCondition au deplacement d'un monstre non valide (direction)" $ do
            Mon.prop_pre_moveMonster (Monster Orc (C.C 50 50) (-1) 3 True)
            `shouldBe` False
        it "PreCondition au deplacement d'un monstre non valide (compteur)" $ do
            Mon.prop_pre_moveMonster (Monster Orc (C.C 50 50) 0 6 True)
            `shouldBe` False
        it "PostCondition au deplacement d'un monstre non valide (Coordonnées)" $ do
            Mon.prop_post_moveMonster (Monster Orc (C.C 50 55) 0 3 False)
            `shouldBe` False
        it "PostCondition au deplacement d'un monstre non valide (compteur)" $ do
            Mon.prop_post_moveMonster (Monster Orc (C.C 50 50) 0 6 True)
            `shouldBe` False

elimineSpec = do 
    describe "Verifie l'élimination d'un monstre (non affichage)" $ do
        it "PreCondition à l'élimination de monstres valide (1)" $ do
            Mon.prop_pre_elimineMonstres 50 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "PreCondition à l'élimination de monstres valide (2)" $ do
            Mon.prop_pre_elimineMonstres 500 500 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "PostCondition à l'élimination de monstres valide (1)" $ do
            Mon.prop_post_elimineMonstres 50 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "PreCondition à l'élimination de monstres valide (2)" $ do
            Mon.prop_post_elimineMonstres 500 500 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "PreCondition à l'élimination de monstres non valide (1)" $ do
            Mon.prop_pre_elimineMonstres 55 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False
        it "PreCondition à l'élimination de monstres non valide (2)" $ do
            Mon.prop_pre_elimineMonstres 50 50 (Mon.initMonstres [((155,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False
        it "PostCondition à l'élimination de monstres non valide " $ do
            Mon.prop_post_elimineMonstres 50 50 (Mon.initMonstres [((155,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False

collisionSpec = do
    describe "Verifie la collision de monstres par rapport à une coordonnée" $ do
        it "PreCondition a la collision de monstre valide" $ do
            Mon.prop_pre_collisionMonstres 50 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "PreCondition a la collision de monstre non valide (1)" $ do
            Mon.prop_pre_collisionMonstres 55 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False
        it "PreCondition a la collision de monstre non valide (2)" $ do
            Mon.prop_pre_collisionMonstres 50 50 (Mon.initMonstres [((155,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False
        it "Collision de monstre détecté" $ do
            Mon.collisionMonstres 50 50 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` True
        it "Collision de monstre non détecté" $ do
            Mon.collisionMonstres 100 100 (Mon.initMonstres [((150,250), "Orc"), ((50,50), "Fantome"), ((500,350), "Skeleton")])
            `shouldBe` False
        



cFunSpec = do 
    invSpec
    initSpec
    moveSpec
    elimineSpec
    collisionSpec