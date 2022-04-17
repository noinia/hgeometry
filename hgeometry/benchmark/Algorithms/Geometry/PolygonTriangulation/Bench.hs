module Algorithms.Geometry.PolygonTriangulation.Bench where
{-
import Algorithms.Geometry.LineSegmentIntersection (hasSelfIntersections)
import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotone as New
import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotoneOld as Old
import           Benchmark.Util
import           Control.DeepSeq
import           Control.Lens
import           Data.Ext
import           Test.Tasty.Bench
import qualified Data.Foldable as F
import           Ipe
import           Geometry.LineSegment
import           Geometry.Polygon
import           Geometry.PlanarSubdivision
import           Geometry.Point
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import           Data.Proxy
import           Test.QuickCheck

--------------------------------------------------------------------------------

data PX = PX

main :: IO ()
main = do
    polies <- getPolies "/home/frank/tmp/antarctica.ipe"
    defaultMain [ benchBuild polies ]

getPolies inFile = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> error $ show err
      Right (page :: IpePage Rational) -> pure $ runPage page
  where
    runPage page =
      let polies  = page^..content.to flattenGroups.traverse._withAttrs _IpePath _asSimplePolygon
      in filter (not . hasSelfIntersections . (^.core)) polies


process f polies = let subdivs = map (\(pg :+ _) -> f (Identity PX) pg) polies
                   in concatMap (\ps -> map (^._2.core) . F.toList . edgeSegments $ ps) subdivs

-- benchmark :: Benchmark
-- benchmark = bgroup "MakeMonotoneBench"
--     [ env (genPts (Proxy :: Proxy Rational) 100) benchBuild
--     ]

--------------------------------------------------------------------------------

-- | Benchmark computing the closest pair
benchBuild    :: (Ord r, Fractional r, NFData r) => [Polygon t () r :+ p] -> Benchmark
benchBuild ss = bgroup "MakeMonotone" [ bgroup (show n) (build $ take n ss)
                                      | n <- sizes' ss
                                      ]
  where
    sizes' xs = [length xs]
      -- let n = length pts in [ n*i `div` 100 | i <- [10,20,25,50,75,100]]

    build ps = [ bench "Old"      $ nf (process Old.makeMonotone) ps
               , bench "New"      $ nf (process New.makeMonotone) ps
               ]
-}
