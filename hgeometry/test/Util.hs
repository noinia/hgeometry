module Util where

import           Control.Exception.Base (bracket)
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Ipe
import qualified Data.List as L
import           Data.Proxy
import           Data.Singletons (Apply)
import           Data.Vinyl
import           System.Directory (removeFile, getTemporaryDirectory)
import           System.FilePath (takeExtension)
import           System.IO (hClose,openTempFile, Handle)
import           Test.Hspec

--------------------------------------------------------------------------------

byStrokeColour :: (Stroke ∈ ats, Ord (Apply f Stroke))
               => [a :+ Attributes f ats] -> [[a :+ Attributes f ats]]
byStrokeColour = map (map fst) . L.groupBy ((==) `on` snd) . L.sortOn snd
               . map (\x -> (x,lookup' x))
  where
    lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy Stroke) ats

-- | Computes all elements on which the two lists differ
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = (xs L.\\ ys) ++ (ys L.\\ xs)

-- differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]


diffBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
diffBy p xs ys = foldr (L.deleteBy p) ys xs

-- | \(O(n^2)\) set that ignores duplicates and order
newtype NaiveSet a = NaiveSet [a] deriving (Show)

instance Eq a => Eq (NaiveSet a) where
  (NaiveSet xs) == (NaiveSet ys) = L.null $ difference xs ys


-- | Given a file with some file contents and a procedure that produces a
-- bytestring. Verify that the bytestring that we produce is the same as the
-- one stored in the file. If not, the output is stored in a temporary file so
-- that we can later look at the details.
runOnFile             :: String    -- ^ the description
                      -> FilePath  -- ^ the expected output file
                      -> IO B.ByteString -- ^ the algorithm to run.
                      -> Spec
runOnFile s expFP alg = runOnFile' s expFP (\h -> alg >>= B.hPut h)


data Res = Res Bool FilePath FilePath
         | True' deriving (Show)

instance Eq Res where
  Res b _ _ == _ = b
  True'     == _ = True

-- | Given a file with some file contents and a procedure that produces a
-- bytestring. Verify that the bytestring that we produce is the same as the
-- one stored in the file. If not, the output is stored in a temporary file so
-- that we can later look at the details.
runOnFile'                 :: String    -- ^ the description
                           -> FilePath  -- ^ the expected output file
                           -> (Handle -> IO ()) -- ^ the algorithm to run.
                           -> Spec
runOnFile' descr expFP alg = it descr $ do
                               runAlgo `shouldReturn` True'
  where
    runAlgo = do
                dir <- getTemporaryDirectory
                outFP <- bracket (openTempFile dir outFPName)
                                 (hClose . snd)
                                 (\(fp,h) -> do
                                     alg h
                                     pure fp)
                res <- sameFile expFP outFP
                when res $ removeFile outFP
                pure $ Res res expFP outFP
    outFPName = "hgeometry_runOnFile_algo" <> takeExtension expFP


-- | Test if two files are the same. Warning: uses lazy IO.
sameFile       :: FilePath -> FilePath -> IO Bool
sameFile fa fb = do a <- LB.readFile fa
                    b <- LB.readFile fb
                    pure $ a == b
