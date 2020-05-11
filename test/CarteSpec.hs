module CarteSpec where

import Test.Hspec
import Carte (Case)
import Carte (Carte)
import Carte (Coord)
import qualified Carte as C
import qualified Data.Map.Strict as M

tailleSpec = do 
    describe "Verifie si le nombre de cases correspond à la taille de la carte" $ do
        it "Carte avec un contenu vide mais une taille " $ do 
            C.prop_TailleCarte (C.Carte 100 100 M.empty)
            `shouldBe` False
        it "Une bonne carte (creer manuellement)"  $ do
            C.prop_TailleCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Vide))
            `shouldBe` True
        it "Une bonne carte en lecture" $ do
            C.prop_TailleCarte (C.readCarte "T=T\n| |\n===")
            `shouldBe` True
        it "Une mauvaise carte en lecture" $ do
            C.prop_TailleCarte (C.readCarte "T=T\n||\n===")
            `shouldBe` False

caseSpec = do
    describe "Verifie les coordonnées de chaque cases et si la carte est bien entouré de mur" $ do
        it "Carte avec une case (correct)" $ do 
            C.prop_CaseCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Horizontal))
            `shouldBe` True
        it "Carte avec une case (Vide)" $ do 
            C.prop_CaseCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Vide))
            `shouldBe` False
        it "Carte avec une case (coordonnée negatif)" $ do 
            C.prop_CaseCarte (C.Carte 50 50 (M.singleton (C.C (-10) (-10)) C.Vide))
            `shouldBe` False
        it "Une bonne carte en lecture" $ do
            C.prop_CaseCarte (C.readCarte "T=T\n| |\n===")
            `shouldBe` True
        it "Une mauvaise carte en lecture (non entouré de mur)" $ do
            C.prop_CaseCarte (C.readCarte "T=T\n| c\n===")
            `shouldBe` False
        it "Une mauvaise carte en lecture (case en dehors de la taille donnée)" $ do
            C.prop_CaseCarte (C.readCarte "T=T\n| |c\n====")
            `shouldBe` False

porteSpec = do
    describe "Verifie que les portes sont supportées par des murs" $ do
        it "Carte avec une case (porte)" $ do
            C.prop_CasePorte (C.Carte 50 50 (M.singleton (C.C 0 0) C.PorteNS))
            `shouldBe` False
        it "Une bonne carte en lecture" $ do
            C.prop_CasePorte (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Une mauvaise carte en lecture (porte NS)" $ do
            C.prop_CasePorte (C.readCarte "T===T\n|   |\n| n |\n=====")
            `shouldBe` False
        it "Une mauvaise carte en lecture (porteEO)" $ do
            C.prop_CasePorte (C.readCarte "T===T\n|   |\n| c |\n=====")
            `shouldBe` False

fullSpec = do
    describe "Verifie toute les propriétées d'une Carte" $ do
        it "Lecture d'une bonne carte" $ do
            C.prop_inv_Carte (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Lecture d'une mauvaise carte (porte)" $ do
            C.prop_inv_Carte (C.readCarte "T===T\n|   |\n| n |\n=====")
            `shouldBe` False
        it "Lecture d'une mauvaise carte (case)" $ do
            C.prop_inv_Carte (C.readCarte "T=T\n| c\n===")
            `shouldBe` False
        it "Lecture d'une mauvaise carte (taille)" $ do
            C.prop_inv_Carte (C.readCarte "T=T\n||\n===")
            `shouldBe` False

openDoorSpec = do
    describe "Verifie l'ouverture d'une porte s'effectue correctement" $ do
        it "Invariant d'ouverture d'une porte sur une carte correcte" $ do
            C.prop_inv_Carte (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Precondition d'ouverture d'une porte correcte (+ invariant)" $ do
            C.prop_pre_openDoor 50 50 (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "PostCondition: Ouverture d'une porte (+ invariant)" $ do
            C.prop_post_openDoor 50 50 (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Invariant apres l'ouverture d'une porte" $ do
            let (carte, _) = (C.openDoor 50 50 (C.readCarte "T===T\n| c |\n=====")) in C.prop_inv_Carte carte
            `shouldBe` True
        it "Precondition d'ouverture d'une porte qui n'existe pas (+ invariant)" $ do
            C.prop_pre_openDoor 50 50 (C.readCarte "T===T\n|   |\n=====")
            `shouldBe` True
        it "PostCondition: non ouverture d'une porte qui n'existe pas (+ invariant)" $ do
            C.prop_post_openDoor 50 50 (C.readCarte "T===T\n|   |\n=====")
            `shouldBe` False
        it "Invariant de la carte n'ayant pas de carte apres la méthode" $ do
            let (carte, _) = (C.openDoor 50 50 (C.readCarte "T===T\n|   |\n=====")) in C.prop_inv_Carte carte
            `shouldBe` True


            

cFunSpec = do 
    tailleSpec
    caseSpec
    porteSpec
    fullSpec
    openDoorSpec