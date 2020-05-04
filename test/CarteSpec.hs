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
            C.propTailleCarte (C.Carte 100 100 M.empty)
            `shouldBe` False
        it "Une bonne carte (creer manuellement)"  $ do
            C.propTailleCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Vide))
            `shouldBe` True
        it "Une bonne carte en lecture" $ do
            C.propTailleCarte (C.readCarte "T=T\n| |\n===")
            `shouldBe` True
        it "Une mauvaise carte en lecture" $ do
            C.propTailleCarte (C.readCarte "T=T\n||\n===")
            `shouldBe` False

caseSpec = do
    describe "Verifie les coordonnées de chaque cases et si la carte est bien entouré de mur" $ do
        it "Carte avec une case (correct)" $ do 
            C.propCaseCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Horizontal))
            `shouldBe` True
        it "Carte avec une case (Vide)" $ do 
            C.propCaseCarte (C.Carte 50 50 (M.singleton (C.C 0 0) C.Vide))
            `shouldBe` False
        it "Carte avec une case (coordonnée negatif)" $ do 
            C.propCaseCarte (C.Carte 50 50 (M.singleton (C.C (-10) (-10)) C.Vide))
            `shouldBe` False
        it "Une bonne carte en lecture" $ do
            C.propCaseCarte (C.readCarte "T=T\n| |\n===")
            `shouldBe` True
        it "Une mauvaise carte en lecture (non entouré de mur)" $ do
            C.propCaseCarte (C.readCarte "T=T\n| c\n===")
            `shouldBe` False
        it "Une mauvaise carte en lecture (case en dehors de la taille donnée)" $ do
            C.propCaseCarte (C.readCarte "T=T\n| |c\n====")
            `shouldBe` False

porteSpec = do
    describe "Verifie que les portes sont supportées par des murs" $ do
        it "Carte avec une case (porte)" $ do
            C.propCasePorte (C.Carte 50 50 (M.singleton (C.C 0 0) C.PorteNS))
            `shouldBe` False
        it "Une bonne carte en lecture" $ do
            C.propCasePorte (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True
        it "Une mauvaise carte en lecture (porte NS)" $ do
            C.propCasePorte (C.readCarte "T===T\n|   |\n| n |\n=====")
            `shouldBe` False
        it "Une mauvaise carte en lecture (porteEO)" $ do
            C.propCasePorte (C.readCarte "T===T\n|   |\n| c |\n=====")
            `shouldBe` False

fullSpec = do
    describe "Verifie que les portes sont supportées par des murs" $ do
        it "Lecture d'une bonne carte" $ do
            C.propCasePorte (C.readCarte "T===T\n| c |\n=====")
            `shouldBe` True


            

cFunSpec = do 
    tailleSpec
    caseSpec
    porteSpec