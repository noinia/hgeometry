--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.Melkman
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup, Frank Staals
--
--
-- Compute the Convex hull of a polygon in linear time.
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.Melkman
  ( convexHull
  ) where

import           Control.Lens ((^..))
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as Mut
import qualified Data.Vector.NonEmpty as NonEmptyV
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


type M s v a = StateT (Mut.MVector s v, Int) (ST s) a

runM :: Int -> M s v () -> ST s (Mut.MVector s v)
runM s action = do
  v <- Mut.new (2*s)
  (v', f) <- execStateT action (Mut.drop s v, 0)
  return $ Mut.tail $ Mut.take f v'

dequeRemove :: M s a ()
dequeRemove = do
  modify $ \(Mut.MVector offset len arr, f) -> (Mut.MVector (offset+1) (len-1) arr, f-1)

dequeInsert :: a -> M s a ()
dequeInsert a = do
  modify $ \(Mut.MVector offset len arr, f) -> (Mut.MVector (offset-1) (len+1) arr, f+1)
  (v,_) <- get
  Mut.write v 0 a

dequePush :: a -> M s a ()
dequePush a = do
  (v, f) <- get
  Mut.write v f a
  put (v,f+1)

dequePop :: M s a ()
dequePop = do
  modify $ \(v,f) -> (v,f-1)

dequeBottom :: Int -> M s a a
dequeBottom idx = do
  (v,_) <- get
  Mut.read v idx

dequeTop :: Int -> M s a a
dequeTop idx = do
  (v,f) <- get
  Mut.read v (f-idx-1)

-- Melkman's algorithm: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.512.9681&rep=rep1&type=pdf

-- | \( O(n) \) Convex hull of a simple polygon.
--
--   For algorithmic details see: <https://en.wikipedia.org/wiki/Convex_hull_of_a_simple_polygon>
convexHull   :: forall polygon point r. (Polygon_ polygon point r, Ord r, Num r)
             => polygon -> ConvexPolygon point
convexHull pg = uncheckedFromCCWPoints . NonEmptyV.unsafeFromVector
              $ V.create $ runM (length vs) $ findStartingPoint 2
  where
    -- Find the first spot where 0,n-1,n is not colinear.
    findStartingPoint     :: Int -> M s point ()
    findStartingPoint nth = do
      let vPrev = vs V.! (nth-1)
          vNth = vs V.! nth
      case ccw v1 vPrev vNth of
        CoLinear -> findStartingPoint (nth+1)
        CCW -> do
          dequePush v1 >> dequePush vPrev
          dequePush vNth; dequeInsert vNth
          V.mapM_ build (V.drop (nth+1) vs)
        CW -> do
          dequePush vPrev >> dequePush v1
          dequePush vNth; dequeInsert vNth
          V.mapM_ build (V.drop (nth+1) vs)

    v1 = vs V.! 0
    vs = V.fromList $ pg^..outerBoundary



    build v = do
      botTurn <- ccw <$> pure v     <*> dequeBottom 0 <*> dequeBottom 1
      topTurn <- ccw <$> dequeTop 1 <*> dequeTop 0    <*> pure v
      when (botTurn == CW || topTurn == CW) $ do
        backtrackTop v; dequePush v
        backtrackBot v; dequeInsert v
    backtrackTop v = do
      turn <- ccw <$> dequeTop 1 <*> dequeTop 0 <*> pure v
      unless (turn == CCW) $ do
        dequePop
        backtrackTop v
    backtrackBot v = do
      turn <- ccw <$> pure v <*> dequeBottom 0 <*> dequeBottom 1
      unless (turn == CCW) $ do
        dequeRemove
        backtrackBot v
