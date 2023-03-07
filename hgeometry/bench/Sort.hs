import           Control.DeepSeq (force)
import           Test.Tasty.Bench

import qualified Data.List as List
import           HGeometry.Point.PointF
import           HGeometry.Point
import           ManualV2
import           System.Random

import qualified HGeometry.Point as BoxedPoint

import           HGeometry.Foldable.Sort
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as UnBoxed

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

type Point2 = PointF Vec2

type BoxedPoint2 = BoxedPoint.Point 2 Int


randomPoints   :: ( Uniform point
                  ) => Int -> IO [point]
randomPoints n = take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

thePoints   :: Int -> IO ([Point2], [BoxedPoint2], [Point 2 Int]
                         )
thePoints n = (\pts -> (pts, map pointFromPoint pts, map pointFromPoint pts
                       )) <$> randomPoints @Point2 n

-- | I expect  this to work, but apparently it doesn't
-- main :: IO ()
-- main = defaultMain
--   [ env (thePoints 1000) $ \(pts,boxedPts) ->
--     bgroup "sorting tests"
--     [ bench "unboxed"     $ nf List.sort pts
--     , bench "boxed "      $ nf List.sort boxedPts
--     ]
--   ]

main :: IO ()
main = do
  (!pts,!boxedPts,!optPts) <- force <$> thePoints 100000
  defaultMain
    [ bgroup "sorting tests"
      [ bench "unboxed"           $ nf List.sort pts
      , bench "boxed "            $ nf List.sort boxedPts
      , bench "optimal "          $ nf List.sort optPts
      , bench "introSort unpacked boxed" $ nf (sort @Boxed.Vector) pts
      , bench "introSort boxed"   $ nf (sort @Boxed.Vector) boxedPts
      , bench "introSort unpacked unboxed" $ nf (sort @UnBoxed.Vector) pts
      , bench "introSort optimal points" $ nf (sort @UnBoxed.Vector) optPts
      ]
    ]
