module MinDisk where

import qualified Data.Foldable as F
import           Control.Lens
import           Data.Geometry.Algorithms.SmallestEnclosingBall
import           Data.Geometry.Ball
import           Data.Geometry.Point
import           Data.Geometry.Line
import           Data.Ext

import           Data.Geometry.Ipe.Reader
import           Data.Geometry.Ipe.Writer
import           Data.Maybe
import           Data.Seq2
import           System.Environment(getArgs)
import           System.Random



instance (Floating r, IpeWriteText r) => IpeWrite (DiskResult p r) where
  ipeWrite (DiskResult d pts) = combine $
     (maybeToList $ ipeWrite d) ++ (mapMaybe (ipeWritePoint . (^.core)) . F.toList $ pts)


main = do
  (fp:_) <- getArgs
  main' fp


main' fp = do
  pls    <- polylinesFromIpeFile fp
  gen <- getStdGen
  mapM_ (print . ipeWrite . minDisk' gen) pls


minDisk' :: RandomGen g => g -> PolyLine 2 () Double -> DiskResult () Double
minDisk' = minDisk

minDisk    :: (Ord r, Fractional r, RandomGen g) => g -> PolyLine 2 () r -> DiskResult () r
minDisk gen pl = let (a :<< (pts :> b)) = viewl $ pl^.points
             in smallestEnclosingDisk gen a b (F.toList pts)
