module ItemsSpec where     
    
import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import Items (Type)
import qualified Items as I
import qualified Carte as C
import qualified Data.Map.Strict as M


isSwordSpec = do 
    describe "Verifie isSword" $ do
        it "Une map vide "  $ do 
            I.isSword 10 10 False M.empty
            `shouldBe` False
        it "Avec de mauvaise coordonnée" $ do
            I.isSword 150 23 False (M.singleton (C.C 0 0) (I.Item I.Epee True))
            `shouldBe` False
        it "Avec de bonne coordonnée mais une demande de négation" $ do
            I.isSword 0 0 True (M.singleton (C.C 0 0) (I.Item I.Epee True))
            `shouldBe` False
        it "Avec de bonne coordonnée et sans demande de négation" $ do
            I.isSword 0 0 False (M.singleton (C.C 0 0) (I.Item I.Epee True))
            `shouldBe` True
        it "Avec de bonne coordonnée et sans demande de négation" $ do
            I.isSword 0 0 False (M.singleton (C.C 0 0) (I.Item I.Clef True))
            `shouldBe` False

isKeySpec = do 
    describe "Verifie isKey" $ do
        it "Une map vide "  $ do 
            I.isKey 10 10 False M.empty
            `shouldBe` False
        it "Avec de mauvaise coordonnée" $ do
            I.isKey 150 23 False (M.singleton (C.C 0 0) (I.Item I.Clef True))
            `shouldBe` False
        it "Avec de bonne coordonnée mais une demande de négation" $ do
            I.isKey 0 0 True (M.singleton (C.C 0 0) (I.Item I.Clef True))
            `shouldBe` False
        it "Avec de bonne coordonnée et sans demande de négation" $ do
            I.isKey 0 0 False (M.singleton (C.C 0 0) (I.Item I.Clef True))
            `shouldBe` True
        it "Avec de bonne coordonnée et sans demande de négation mais mauvais items" $ do
            I.isKey 0 0 False (M.singleton (C.C 0 0) (I.Item I.Epee True))
            `shouldBe` False

changeItemsSpec = do 
    describe "Verifie changeItems" $ do
        it "pre condition coordonner inchoérente "  $ do 
            I.prop_pre_coord_changeItems (C.C 47 (-2)) I.Clef (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` False
        it "pre condition coordonner correcte" $ do
            I.prop_pre_coord_changeItems (C.C 50 50) I.Clef (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` True
        it "pre condition case non presente " $ do
            I.prop_pre_exist_changeItems (C.C 150 200) I.Clef (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` False
        it "pre condition case presente" $ do
            I.prop_pre_exist_changeItems (C.C 50 50) I.Clef (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` True
        it "post condition Type incorrecte" $ do
            I.prop_post_changeItems (C.C 50 50) I.ErrorItem (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` False
        it "post condition Type correcte" $ do
            I.prop_post_changeItems (C.C 50 50) I.Clef (M.fromList [((C.C 50 50),(I.Item I.Clef False))])
            `shouldBe` True

initItemsSpec = do
    describe "Verifie initItems" $ do
        it "pre condition Type d'entré valide et coordonnée valide" $ do
           I.prop_pre_initItems [("clef",(50,50)),("epee",(100,100)),("tresor",(150,150))]
           `shouldBe` True 
        it "pre condition Type d'entré valide et coordonnée invalide" $ do
           I.prop_pre_initItems [("clef",(30,50)),("epee",(100,100)),("tresor",(150,150))]
           `shouldBe` False
        it "pre condition Type d'entré invalide et coordonnée valide" $ do
           I.prop_pre_initItems [("bob",(50,50)),("epee",(100,100)),("tresor",(150,150))]
           `shouldBe` False
        it "pre condition Type d'entré invalide et coordonnée invalide" $ do
           I.prop_pre_initItems [("clef",(30,50)),("bob",(100,100)),("tresor",(150,150))]
           `shouldBe` False
        it "post condition verification de l'invariant" $ do
           I.prop_post_initItems (I.initItems [("clef",(50,50)),("epee",(100,100)),("tresor",(150,150))])
           `shouldBe` True
        it "post condition sur résultat incohérant" $ do
           I.prop_post_initItems (M.fromList [((C.C 50 50),(I.Item I.Clef False)),((C.C 100 50),(I.Item I.ErrorItem False))])
           `shouldBe` False
invarianSpec = do
    describe "verifie un Item" $ do
        it "un item de Type valide" $ do
            I.prop_inv_ItemType (I.Item I.Epee True)
            `shouldBe` True
        it "un item de Type invalide" $ do
            I.prop_inv_ItemType (I.Item I.ErrorItem True)
            `shouldBe` False

cFunSpec = do 
    isSwordSpec
    isKeySpec
    changeItemsSpec
    initItemsSpec
    invarianSpec

{--
genItemsOk ::Bool -> Gen I.Item
genItemsOk b = do
    i <- choose(1,4)
    auxItem i b

auxType :: Int -> String
auxType i =
    case i of
        1 -> "epee"
        2 -> "clef"
        3 -> "tresor"
        4 -> "ErrorItem"
        x -> "bob"

genTypeOk :: Gen (String,(Int,Int))
genTypeOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    i <- choose(1,4)
    return $ (auxType i, ((x*50),(y*50)))


typeToStringSpec = do
    describe "verifie typeToString" $ do
        it "epee" $ do
            I.typeToString I.Epee 
            `shouldBe` "epee"
        it "clef" $ do
            I.typeToString I.Clef 
            `shouldBe` "clef"
        it "tresor" $ do
            I.typeToString I.Tresor 
            `shouldBe` "tresor"
        it "ErrorItem" $ do
            I.typeToString I.ErrorItem
            `shouldBe` "ErrorItem"


prop_initItem_inv :: Property
prop_initItem_inv = forAll genTypeOk $ I.propIdValide

initSpeck = do
    describe "verifie ques les entré sont valide" $ do
        it "liste valide" $
            property prop_initItem_inv 
--}



