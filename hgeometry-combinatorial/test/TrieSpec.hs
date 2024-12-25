module TrieSpec(spec) where

-- import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           HGeometry.Sequence.KV
import           HGeometry.Trie
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Trie tests" $ do
         it "fold1 Map" $
           fold1 myTrie `shouldBe` "rootfoograndchildbar"
         it "foldable1 to list Map" $
           toNonEmpty myTrie `shouldBe` NonEmpty.fromList ["root","foo","grandchild","bar"]

         it "fold1 KV" $
           fold1 myTrie2 `shouldBe` "rootfoograndchildbar"
         it "foldable1 to list KV" $
           toNonEmpty myTrie2 `shouldBe` NonEmpty.fromList ["root","foo","grandchild","bar"]



myTrie :: TrieF Map.Map Int String
myTrie = Node "root" (Map.fromAscList [ (1,Node "foo" (Map.fromAscList
                                                        [(10, Node "grandchild" Map.empty)]
                                                      )
                                        )
                                      , (2,Node "bar" Map.empty)
                                      ]

                     )


myTrie2 :: TrieF (KV []) Int String
myTrie2 = Node "root" (KV [ (1,Node "foo" (KV [(10, Node "grandchild" empty)]))
                          , (2,Node "bar" empty)
                          ]

                     )
