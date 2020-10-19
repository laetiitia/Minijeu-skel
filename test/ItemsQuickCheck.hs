module ItemsQuickCheck where     
    
import Test.Hspec
import Test.QuickCheck
import Carte (Coord)
import Items (Type)
import Items (Item)
import qualified Items as I
import qualified Carte as C
import qualified Data.Map.Strict as M


{-genItemsOk ::Bool -> Gen I.Item
genItemsOk b = do
    i <- choose(1,4)
    auxItem i b
-}
auxType :: Int -> Type
auxType i =
    case i of
        1 -> I.Epee
        2 -> I.Clef
        3 -> I.Escalier

auxBool :: Int -> Bool
auxBool i =
    case i of
        1 -> True
        2 -> False

genTypeOK :: Gen Type
genTypeOK = do
    i <- choose(1,3)
    return $ (auxType i)

genItemOK :: Gen Item
genItemOK = do
    t <- choose(1,3)
    a <- choose(1,2)
    return $ (I.Item (auxType t) (auxBool a))


genCoordOk :: Gen Coord
genCoordOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    return $ (C.C (50 * x) (50*y))

genCoordItemOK :: Gen (Coord,Item)
genCoordItemOK = do
    c <- genCoordOk
    i <- genItemOK
    return $ (c,i)

genListCoordItemOK :: Gen [(Coord,Item)]
genListCoordItemOK = do
    vectorOf 100 genCoordItemOK

auxinitType :: Int -> String
auxinitType i =
    case i of
        1 -> "epee"
        2 -> "clef"
        3 -> "tresor"
        4 -> "ErrorItem"

auxgeninitTypeOk :: Gen (String, Coord)
auxgeninitTypeOk = do
    x <- choose(0,20)
    y <- choose(0,20)
    i <- choose(1,3)
    return $ (auxinitType i, (C.C (x*50) (y*50)))

geninitTypeOK :: Gen [(String,Coord)]
geninitTypeOK = do
    vectorOf 100 auxgeninitTypeOk

genMapItemsOK :: Gen (M.Map Coord Item)
genMapItemsOK = do
    list <- genListCoordItemOK
    return $ (M.fromList list)

genChangeItems :: Gen (Coord,Type,M.Map Coord Item)
genChangeItems = do
    c <- genCoordOk
    t <- genTypeOK
    m <- genMapItemsOK
    return $ (c,t,m)

aux_prop_coord_changeItems :: (Coord , Type , M.Map Coord Item) -> Bool
aux_prop_coord_changeItems (c,t,m) =I.prop_post_changeItems c t m

prop_Item_inv :: Property
prop_Item_inv = forAll genItemOK $ I.prop_inv_ItemType

prop_initItem_pre :: Property
prop_initItem_pre = forAll geninitTypeOK $ I.prop_pre_initItems

prop_initItem_post :: Property
prop_initItem_post = forAll genMapItemsOK $ I.prop_post_initItems

prop_changeItems_post :: Property
prop_changeItems_post = forAll genChangeItems $ aux_prop_coord_changeItems


itemsSpeck = do
    describe "verifie l'invariant sur les items" $ do
        it "liste valide" $
            property prop_Item_inv 

preinititemsSpeck = do
    describe "verifie pre condition sur les initialisation d'items" $ do
        it "liste valide de 100 elements" $
            property  prop_initItem_pre

postinititemsSpeck = do
    describe "verifie post condition sur les initialisation d'items" $ do
        it "map de 100 elements" $
            property  prop_initItem_post

postchangeItemsSpeck = do
    describe "verifie post condition sur les Change items" $ do
        it "map de 100 elements" $
            property  prop_changeItems_post

cFunSpec = do 
    itemsSpeck
    preinititemsSpeck
    postinititemsSpeck
    postchangeItemsSpeck