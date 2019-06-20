module Algorithms.Geometry.FrechetDistance.Discrete( discreteFrechetDistance
                                                   , discreteFrechetDistanceWith
                                                   ) where

import           Control.Lens ((^.))
import           Control.Monad.ST (ST,runST)
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

--------------------------------------------------------------------------------


-- | Returns the discrete frechet distance between two point sequences
-- using the squared Euclidean distance. In other words, returns the
-- square of the (Euclidean) frechet distance.
--
-- running time: \(O((nm))\), where \(n\) and \(m\) are the lengths of
-- the sequences.
discreteFrechetDistance :: (Foldable f, Foldable g,  Functor f, Functor g, Ord r, Num r)
                        => f (Point 2 r :+ p) -> g (Point 2 r :+ q) -> r
discreteFrechetDistance = discreteFrechetDistanceWith squaredEuclideanDist

-- | Returns the discrete frechet distance between two point sequences
-- using the given distance measure.
--
-- running time: \(O((nm))\), where \(n\) and \(m\) are the lengths of
-- the sequences (and assuming that a distance calculation takes
-- constant time).
discreteFrechetDistanceWith         :: ( Foldable f, Functor f, Functor g, Foldable g, Ord r)
                                    => (Point 2 r -> Point 2 r -> r) -- ^ distance function
                                    -> f (Point 2 r :+ p)
                                    -> g (Point 2 r :+ q) -> r
discreteFrechetDistanceWith d ta tb = runST $ do
                                                 v <- MV.replicate (n*m) Nothing
                                                 let dpTable = DPTable m v
                                                     z       = Loc 0 0
                                                 -- initializes (0,0) with the appropriate distance
                                                 storeT dpTable z (dist z)
                                                 evalTable dist dpTable (Loc (n-1) (m-1))
  where
    ta' = Builder.build . Builder.foldable . fmap (^.core) $ ta
    tb' = Builder.build . Builder.foldable . fmap (^.core) $ tb
    n = V.length ta'
    m = V.length tb'

    dist (Loc r c) = d (ta' V.! r) (tb' V.! c)

data Loc = Loc !Int !Int deriving (Show,Eq)

data DPTable s r = DPTable !Int (MV.MVector s (Maybe r))

-- | compute the discrete frechet distance between the subtrajectories
-- up to the given Loc using dpTable for memoization memoization
evalTable              :: Ord r => (Loc -> r) -> DPTable s r -> Loc -> ST s r
evalTable dist dpTable = go
  where
    go p = lookupT dpTable p >>= \case
             Just d  -> pure d
             Nothing -> do
                          fd <- minimum <$> mapM go (prevs p)
                          let d = dist p `max` fd
                          storeT dpTable p d
                          pure d

-- | Look up a value in the DP Table
lookupT                           :: DPTable s r -> Loc -> ST s (Maybe r)
lookupT (DPTable m v) (Loc r c) = MV.read v (r*m+c)

-- | Stoer a value in the DP table
storeT                             :: DPTable s r -> Loc -> r -> ST s ()
storeT (DPTable m v) (Loc r c) d = MV.write v (r*m+c) (Just d)

-- | Candidate previous locations
prevs           :: Loc -> [Loc]
prevs (Loc r c) = filter (\(Loc x y) -> x >= 0 && y >= 0)
                    [Loc (r-1) c, Loc (r-1) (c-1), Loc r (c-1)]
