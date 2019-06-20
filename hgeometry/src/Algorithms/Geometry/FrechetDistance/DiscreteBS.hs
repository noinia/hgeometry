module Algorithms.Geometry.FrechetDistance.DiscreteBS where

import           Control.Lens ((^.))
import           Control.Monad.ST (ST,runST)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import qualified Data.List as List
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Sequence.Util (binarySearchVec)
import           Data.Util (SP(..))
import           Data.Vector ((!?))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

--------------------------------------------------------------------------------


-- | Returns the discrete frechet distance between two point sequences
-- using the squared Euclidean distance. In other words, returns the
-- square of the (Euclidean) frechet distance.
--
-- running time: \(O((nm)\log (nm))\)
discreteFrechetDistance :: (Foldable f, Foldable g,  Functor f, Functor g, Ord r, Num r)
                        => f (Point 2 r :+ p) -> g (Point 2 r :+ q) -> r
discreteFrechetDistance = discreteFrechetDistanceWith squaredEuclideanDist


discreteFrechetDistanceWith :: ( Foldable f, Functor f, Functor g
                               , Foldable g, Ord r) => (Point 2 r -> Point 2 r -> r)
  -> f (Point 2 r :+ p) -> g (Point 2 r :+ q) -> r
discreteFrechetDistanceWith d ta tb = (dists V.!) . fromJust
                                    $ binarySearchVec (\eps -> decideDFDWith d eps ta' tb') dists
  where
    dists = allDistances d ta' tb'
    ta' = Builder.build . Builder.foldable . fmap (^.core) $ ta
    tb' = Builder.build . Builder.foldable . fmap (^.core) $ tb


-- liftD                     :: (Point 2 r -> Point 2 r -> a) -> Point 2 r :+ p -> Point 2 r :+ q -> a
-- liftD d (p :+ _) (q :+ _) = d p q

allDistances         :: Ord r => (p -> p -> r) -> V.Vector p -> V.Vector p -> V.Vector r
allDistances d ta tb = V.fromList . List.sort
                     $ [ d p q | p <- V.toList ta, q <- V.toList tb ]


--------------------------------------------------------------------------------
-- * Implicit Representation of a Free space diagram

type Loc = SP Int Int

data FreeSpaceDiagram a = FreeSpaceDiagram { rows     :: V.Vector a
                                           , cols     :: V.Vector a
                                           , evalFree :: a -> a -> Bool
                                           }

numRows, numCols :: FreeSpaceDiagram a -> Int
numRows = V.length . rows
numCols = V.length . cols

isFree             :: FreeSpaceDiagram a -> SP Int Int -> Bool
isFree (FreeSpaceDiagram ta tb f) (SP r c) = fromMaybe False $
                                               f <$> ta !? r
                                                 <*> tb !? c

-- | Computes the reachable neighbours in a free space diagram
neighbours             :: Loc -> FreeSpaceDiagram a -> [Loc]
neighbours (SP r c) fd = filter (isFree fd)
                       $ [ SP (r + 1) c, SP (r + 1) (c + 1), SP r (c + 1)]
                       -- we can either go up, go up and right, or go right

--------------------------------------------------------------------------------
-- * The Decision Procedure

-- | Decide if the upper point is reachable with distance threshold eps.
decideDFDWith             :: Ord r
                          => (Point 2 r -> Point 2 r -> r) -> r
                          -> V.Vector (Point 2 r)
                          -> V.Vector (Point 2 r) -> Bool
decideDFDWith d eps ta tb = reachable (FreeSpaceDiagram ta tb f)
  where
    f p q = d p q <= eps

-- | Stores the DFSState: number of rows, number of cols, and the bit marks
data DFSState s = DFSState Int Int (UMV.MVector s Bool)

reachable    :: FreeSpaceDiagram a -> Bool
reachable fd = runST $ do
                      v <- (UMV.new $ n*m)
                      let st = DFSState n m v
                      reachableFrom (SP 0 0) fd st
                      visited (SP (n-1) (m-1)) st
  where
    n = numRows fd
    m = numCols fd



reachableFrom         :: Loc -> FreeSpaceDiagram a -> DFSState s -> ST s ()
reachableFrom p fd st = do
                           b <- visited p st
                           if b then pure ()
                                else do
                                       visit p st
                                       F.forM_ (neighbours p fd) $ \q ->
                                         reachableFrom q fd st


-- -- | Test if some given position is free; if the field would be out of
-- -- range also just returns false.
-- isFree             :: FreeSpaceDiagram Bool -> SP Int Int -> Bool
-- isFree fd (SP r c) = fromMaybe False $ fd !? r >>= (!? c)


--------------------------------------------------------------------------------
-- * Helper functions for marking cells visited.

visit                           :: Loc -> DFSState s -> ST s ()
visit (SP r c) (DFSState _ m v) = UMV.write v (r*m+c) True

visited                           :: Loc -> DFSState s -> ST s Bool
visited (SP r c) (DFSState _ m v) = UMV.read v (r*m+c)



--------------------------------------------------------------------------------

trajA = map ext [origin, Point2 5 5]

trajB = map ext [Point2 0 1, Point2 5 6, Point2 6 6]

test = discreteFrechetDistance trajA trajB
