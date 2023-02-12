{-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -ddump-to-file #-}
import           Control.DeepSeq
import           Test.Tasty.Bench
import           GHC.Generics (Generic)
import qualified Data.List as List
import qualified HGeometry.Vector.Unpacked as Unpacked
import qualified Linear.V2
import qualified Linear.V4
-- import qualified HGeometry.Point.Boxed as BoxedPoint
-- import           HGeometry.Point.PointF
-- import           HGeometry.Point
-- import           HGeometry.Vector.Unboxed.V2
import           System.Random
import           System.Random.Stateful (UniformRange(..))

-- import           Data.Foldable.Sort
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as UV

--------------------------------------------------------------------------------

data ManualInt2 = Manual2 {-# UNPACK #-}!Int
                          {-# UNPACK #-}!Int
                deriving stock (Show,Eq,Ord,Generic)

instance NFData ManualInt2

instance Uniform ManualInt2
instance UniformRange ManualInt2 where
  uniformRM ( Manual2 lowX lowR
            , Manual2 highX highR
            ) gen = Manual2 <$> uniformRM (lowX, highX) gen
                            <*> uniformRM (lowR, highR) gen


data ManualInt4 = Manual4 {-# UNPACK #-}!Int
                          {-# UNPACK #-}!Int
                          {-# UNPACK #-}!Int
                          {-# UNPACK #-}!Int
                deriving stock (Show,Eq,Ord,Generic)

instance NFData ManualInt4

instance Uniform ManualInt4
instance UniformRange ManualInt4 where
  uniformRM ( Manual4 lowX  lowY  lowZ  lowW
            , Manual4 highX highY highZ highW
            ) gen = Manual4
                    <$> uniformRM (lowX, highX) gen
                    <*> uniformRM (lowY, highY) gen
                    <*> uniformRM (lowZ, highZ) gen
                    <*> uniformRM (lowW, highW) gen

--------------------------------------------------------------------------------

-- type Point2 = PointF Vec2

-- type BoxedPoint2 = BoxedPoint.Point 2 Int


randomPoints   :: ( Uniform point
                  ) => Int -> IO [point]
randomPoints n = take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

type Point4 = Unpacked.Vector 4 Int

thePoints   :: Int -> IO ( [Unpacked.Vector 4 Int]
                         , [Linear.V4.V4 Int]
                         , [ManualInt4]
                         )
thePoints n = (\pts -> ( pts
                       , map (\(Unpacked.Vector4 x y z w) -> Linear.V4.V4 x y z w) pts
                       , map (\(Unpacked.Vector4 x y z w) -> Manual4 x y z w) pts
                       )
              ) <$> randomPoints @Point4 n


-- thePoints n = (\pts -> ( map (\(Manual4 x y z w) -> Unpacked.Vector4 x y z w) pts
--                        , map (\(Manual4 x y z w) -> Linear.V4.V4 x y z w) pts
--                        , pts
--                        )
--               ) <$> randomPoints @ManualInt4 n

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (  !unpackedPts
    ,!linearPts
    ,!manualPts) <- force <$> thePoints 100000

  defaultMain
    [ bgroup "sorting tests"
      [ bench "ignore this"       $ nf List.sort unpackedPts
      , bench "linear"            $ nf List.sort linearPts
      , bench "manual"            $ nf List.sort manualPts
      , bench "unpacked"          $ nf List.sort unpackedPts
      , bench "linearAg"          $ nf List.sort linearPts
      -- , bench "introSort unpacked boxed vector" $ nf (sort @V.Vector) unpackedPts
      -- , bench "introSort boxed"   $ nf (sort @Boxed.Vector) boxedPts
      -- , bench "introSort unpacked unboxed" $ nf (sort @UnBoxed.Vector) pts
      -- , bench "introSort optimal points" $ nf (sort @UnBoxed.Vector) optPts
      ]
    ]
