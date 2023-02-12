
import           Control.DeepSeq
import           Test.Tasty.Bench
import           GHC.Generics (Generic)
import qualified Data.List as List
import qualified HGeometry.Vector.Unpacked as Unpacked
import qualified Linear.V2
-- import qualified HGeometry.Point.Boxed as BoxedPoint
-- import           HGeometry.Point.PointF
-- import           HGeometry.Point
-- import           HGeometry.Vector.Unboxed.V2
import           System.Random
import           System.Random.Stateful (UniformRange(..), Uniform(..))

-- import           Data.Foldable.Sort
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

--------------------------------------------------------------------------------

data ManualInt2 = Manual {-# UNPACK #-}!Int {-# UNPACK #-}!Int
                deriving stock (Show,Eq,Ord,Generic)

instance NFData ManualInt2

instance Uniform ManualInt2
instance UniformRange ManualInt2 where
  uniformRM (Manual lowX lowR, Manual highX highR) gen = Manual <$> uniformRM (lowX, highX) gen
                                                                <*> uniformRM (lowR, highR) gen


--------------------------------------------------------------------------------

-- type Point2 = PointF Vec2

-- type BoxedPoint2 = BoxedPoint.Point 2 Int


randomPoints   :: ( Uniform point
                  ) => Int -> IO [point]
randomPoints n = take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

type Point2 = Unpacked.Vector 2 Int -- quick hack :)

thePoints   :: Int -> IO ( [Unpacked.Vector 2 Int]
                         , [Linear.V2.V2 Int]
                         , [ManualInt2]
                         )
thePoints n = (\pts -> ( map (\(Manual x y) -> Unpacked.Vector2 x y) pts
                       , map (\(Manual x y) -> Linear.V2.V2 x y) pts
                       , pts
                       )
              ) <$> randomPoints @ManualInt2 n

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (  !unpackedPts
    ,!linearPts
    ,!manualPts) <- force <$> thePoints 100000

  defaultMain
    [ bgroup "sorting tests"
      [ bench "unpacked"          $ nf List.sort unpackedPts
      , bench "linar "            $ nf List.sort linearPts
      , bench "manual "           $ nf List.sort manualPts
      -- , bench "introSort unpacked boxed vector" $ nf (sort @V.Vector) unpackedPts
      -- , bench "introSort boxed"   $ nf (sort @Boxed.Vector) boxedPts
      -- , bench "introSort unpacked unboxed" $ nf (sort @UnBoxed.Vector) pts
      -- , bench "introSort optimal points" $ nf (sort @UnBoxed.Vector) optPts
      ]
    ]
