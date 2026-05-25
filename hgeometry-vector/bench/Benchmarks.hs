{-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -ddump-to-file #-}
import           Control.DeepSeq
import           Test.Tasty.Bench
import           GHC.Generics (Generic)
import qualified Data.List as List
import           HGeometry.Vector
-- import qualified Linear.V2
import qualified Linear.V4
import           System.Random
import           System.Random.Stateful (UniformRange(..))
import           HGeometry.Foldable.Sort
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

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

type Point4 = Vector 4 Int

thePoints   :: Int -> IO ( [Vector 4 Int]
                         , [Linear.V4.V4 Int]
                         , [ManualInt4]
                         )
thePoints n = (\pts -> ( pts
                       , map (\(Vector4 x y z w) -> Linear.V4.V4 x y z w) pts
                       , map (\(Vector4 x y z w) -> Manual4 x y z w) pts
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
  (  !vectorPts
    ,!linearPts
    ,!manualPts) <- force <$> thePoints 100000

  defaultMain
    [ bgroup "sorting tests"
      [ bench "ignore this"       $ nf List.sort vectorPts
      , bench "linear"            $ nf List.sort linearPts
      , bench "manual"            $ nf List.sort manualPts
      , bench "Vector"            $ nf List.sort vectorPts
      , bench "linearAg"          $ nf List.sort linearPts
      , bench "introSort Boxed.Vector (Vector 4 Int)" $ nf (sort @V.Vector) vectorPts
      , bench "introSort Unboxed.Vector (Vector 4 Int)" $ nf (sort @UV.Vector) vectorPts
      , bench "introSort Boxed.Vector Manual4" $ nf (sort @V.Vector) manualPts
      -- , bench "introSort Unboxed.Vector Manual4" $ nf (sort @UV.Vector) manualPts
      ]
    ]
