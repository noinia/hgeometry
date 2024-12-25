module TrieSpec(spec) where

import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           HGeometry.Trie
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Trie tests" $ do
         it "fold1" $
           fold1 myTrie `shouldBe` "rootbarfoograndchild"
         it "foldable1 to list" $
           toNonEmpty myTrie `shouldBe` NonEmpty.fromList ["root","bar","foo","grandchild"]


myTrie :: TrieF Map.Map Int String
myTrie = Node "root" (Map.fromAscList [ (1,Node "foo" (Map.fromAscList
                                                        [(10, Node "grandchild" Map.empty)]
                                                      )
                                        )
                                      , (2,Node "bar" Map.empty)
                                      ]

                     )
