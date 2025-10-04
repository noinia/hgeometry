module Main(main) where

import Data.Word
import HGeometry.Plane.LowerEnvelope.Connected.Randomized qualified as Randomized
import HGeometry.Number.Real.Rational
import HGeometry.VoronoiDiagram (VoronoiDiagram, voronoiDiagramWith')
import HGeometry.Kernel
import Control.Lens
import System.Random
import Data.Foldable1
import Control.DeepSeq (force)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
--------------------------------------------------------------------------------

type R = RealNumber 5


voronoiDiagram :: ( Point_ point 2 r, Functor f, Ord point
                   , Ord r, Fractional r, Foldable1 f
                   , Show point, Show r
                   ) => f point -> VoronoiDiagram point ()
voronoiDiagram = voronoiDiagramWith' $ Randomized.computeVertexForm (mkStdGen 1)


-- | pre: n > 0
randomPoints   :: ( Uniform point
                  ) => Int -> IO (NonEmpty point)
randomPoints n = NonEmpty.fromList . take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

fromPoint   :: Point 2 Word8 -> Point 2 R
fromPoint p = p&coordinates %~ fromIntegral

main :: IO ()
main = do
  let n = 50
  pts <- force . fmap fromPoint <$> randomPoints n
  let vd = voronoiDiagram pts
  print vd
