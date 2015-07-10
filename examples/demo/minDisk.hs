module MinDisk where

import           Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ball
import           Data.Geometry
import           Data.Geometry.Line
import           Data.Geometry.PolyLine

import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Writer
import           Data.Geometry.Ipe.Reader
import           Data.Maybe
import           Data.Seq2
import           System.Environment(getArgs)
import           System.Random




-- diskResult :: Floating r => IpeOut (DiskResult p r) (IpeObject r ('IpePath '[]))
diskResult = IpeOut f
  where
    f (DiskResult d pts) = asIpeObject' d emptyPathAttributes


main = do
  (fp:_) <- getArgs
  main' fp


main' fp = do
  pls    <- polylinesFromIpeFile fp
  gen <- getStdGen
  print pls
  mapM_ (print . (^.enclosingDisk) . minDisk' gen) pls
  mapM_ (printAsIpeSelection . asIpe diskResult . minDisk' gen) pls


minDisk' :: RandomGen g => g -> PolyLine 2 () Double -> DiskResult () Double
minDisk' = minDisk

minDisk    :: (Ord r, Fractional r, RandomGen g) => g -> PolyLine 2 () r -> DiskResult () r
minDisk gen pl = let (a :<< (pts :> b)) = viewl $ pl^.points
             in smallestEnclosingDisk gen a b (F.toList pts)
