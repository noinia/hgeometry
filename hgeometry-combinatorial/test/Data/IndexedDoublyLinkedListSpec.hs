module Data.IndexedDoublyLinkedListSpec where

import           Control.Monad.ST
import           Data.IndexedDoublyLinkedList
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "IndexedDoublyLinkedList tests" $ do
    it "myTest" $
      runT myProg `shouldBe` (4 :| [2,1,0],4 :| [5,8])
    it "myTest'" $
      runT myProg' `shouldBe` ('e' :| "cba",'e' :| "fi")


runT   :: (forall s. DLListMonad s Char a) -> a
runT c = runDLListMonad (V.fromList "abcdefghi") c

myProg :: DLListMonad s b (NonEmpty Index, NonEmpty Index)
myProg = do writeList (0 :| [2,4,6,8])
            delete 6
            insertBefore 2 1
            insertAfter 4 5
            as <- toListFromR 4
            bs <- toListFrom 4
            pure (as,bs)

myProg' :: DLListMonad s b (NonEmpty b, NonEmpty b)
myProg' = do (as,bs) <- myProg
             as' <- mapM valueAt as
             bs' <- mapM valueAt bs
             pure (as',bs')
