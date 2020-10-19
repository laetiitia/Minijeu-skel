module MonstreQuickCheck where     
    
import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import Monster (Monstre)
import Monster (Espece)
import qualified ItemsQuickCheck as I
import qualified Carte as C
import qualified Monster as MO

import qualified Data.Map.Strict as M



chooseEspece:: Int -> Espece
chooseEspece i =
    case i of
        1 -> MO.Orc
        2 -> MO.Fantome
        3 -> MO.Skeleton

chooseMonsters :: Int -> String
chooseMonsters i =
    case i of
        1 -> "Orc"
        2 -> "Fantome"
        3 -> "Skeleton"
        x -> "error"

genEspeceOK :: Gen Espece
genEspeceOK = do
    i <- choose(1,3)
    return $ (chooseEspece i)

genMonstresOk :: Gen Monstre
genMonstresOk = do
    e <- genEspeceOK
    c <- I.genCoordOk
    direct <- choose(0,((length (MO.getMonsterPattern e))-1))
    b <- choose(1,2)
    cpt <- choose(0,(MO.getCptInit e))
    return $ (MO.Monster e c direct cpt (I.auxBool b))

geninitMonstresOk :: Gen (Coord,String)
geninitMonstresOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    i <- choose(1,3)
    return $ ((C.C (x*50) (y*50)),chooseMonsters i)

genListInitMonstresOK :: Gen [(Coord,String)]
genListInitMonstresOK = do
    l <- vectorOf 100 geninitMonstresOk
    return $ l

genElimineMonstreOK :: Gen (Int, Int, [Monstre])
genElimineMonstreOK = do
    x <- choose(0,20)
    y <- choose(0,20)
    m <- vectorOf 100 genMonstresOk
    return $ ((x*50),(y*50),m)


aux_prop_pre_elimineMonstres::(Int, Int, [Monstre]) -> Bool
aux_prop_pre_elimineMonstres (x,y,m) = MO.prop_pre_elimineMonstres x y m

aux_prop_post_elimineMonstres::(Int, Int, [Monstre]) -> Bool
aux_prop_post_elimineMonstres (x,y,m) = MO.prop_post_elimineMonstres x y m

aux_prop_pre_collisionMonstres:: (Int, Int, [Monstre]) -> Bool
aux_prop_pre_collisionMonstres (x,y,m) = MO.prop_pre_collisionMonstres x y m

prop_Monstre_inv :: Property
prop_Monstre_inv = forAll genMonstresOk $ MO.prop_inv_Monstre

prop_post_getMonstrePattern :: Property
prop_post_getMonstrePattern = forAll genEspeceOK $ MO.prop_post_getMonsterPattern

prop_pre_initMonstres:: Property
prop_pre_initMonstres = forAll genListInitMonstresOK $ MO.prop_pre_initMonstres

prop_post_initMonstres::Property
prop_post_initMonstres = forAll genListInitMonstresOK $ MO.prop_post_initMonstres

prop_pre_moveMonster::Property
prop_pre_moveMonster = forAll genMonstresOk $ MO.prop_pre_moveMonster

prop_post_moveMonster::Property
prop_post_moveMonster = forAll genMonstresOk $ MO.prop_post_moveMonster

prop_pre_elimineMonstres::Property
prop_pre_elimineMonstres = forAll genElimineMonstreOK $ aux_prop_pre_elimineMonstres

prop_post_elimineMonstres::Property
prop_post_elimineMonstres = forAll genElimineMonstreOK $ aux_prop_post_elimineMonstres

prop_pre_collisionMonstres::Property
prop_pre_collisionMonstres = forAll genElimineMonstreOK $ aux_prop_pre_collisionMonstres

monstreSpeck = do
    describe "verifie l'invariant sur les items" $ do
        it "liste valide" $
            property prop_Monstre_inv  

post_getMonsterPatternSpeck = do
    describe "verifie la post condition sur get monstre pattern" $ do
        it "liste valide" $
            property prop_post_getMonstrePattern 

prop_pre_initMonstresSpeck = do
    describe "verifie la pre condition sur intmonstre " $ do
        it "liste de 100 valide" $
            property  prop_pre_initMonstres

prop_post_initMonstresSpeck = do
    describe "verifie la post condition sur intmonstre " $ do
        it "liste de 100 valide" $
            property  prop_post_initMonstres

prop_pre_moveMonsterSpeck = do
    describe "verifie la pre condition sur movemonstre " $ do
        it "liste de 100 valide" $
            property prop_pre_moveMonster
{-
prop_post_moveMonsterSpeck = do
    describe "verifie la post condition sur movemonstre " $ do
        it "liste de 100 valide" $
            property prop_post_moveMonster-}

prop_pre_elimineMonstresSpeck = do
    describe "verifie la pre condition sur elimineMonstre " $ do
        it "liste de 100 valide" $
            property prop_pre_elimineMonstres

prop_post_elimineMonstresSpeck = do
    describe "verifie la post condition sur elimineMonstre " $ do
        it "liste de 100 valide" $
            property prop_post_elimineMonstres

prop_pre_collisionMonstresSpeck = do
    describe "verifie la pre condition sur collisionMonstre " $ do
        it "liste de 100 valide" $
            property prop_pre_collisionMonstres 

cFunSpec = do 
    monstreSpeck
    post_getMonsterPatternSpeck
    prop_pre_initMonstresSpeck
    prop_post_initMonstresSpeck
    prop_pre_moveMonsterSpeck
    {-prop_post_moveMonsterSpeck-}
    prop_pre_elimineMonstresSpeck
    prop_post_elimineMonstresSpeck
    prop_pre_collisionMonstresSpeck