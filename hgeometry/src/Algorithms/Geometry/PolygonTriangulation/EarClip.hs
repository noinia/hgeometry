--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.PolygonTriangulation.EarClip
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- Ear clipping triangulation algorithms. The baseline algorithm runs in \( O(n^2) \)
-- but has a low constant factor overhead. The z-order hashed variant runs in
-- \( O(n \log n) \).
--
-- References:
--
--  1. https://en.wikipedia.org/wiki/Polygon_triangulation#Ear_clipping_method
--  2. https://en.wikipedia.org/wiki/Z-order_curve
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.PolygonTriangulation.EarClip
  ( earClip
  ) where

import           Control.Lens                ((^.))
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe     (unsafeInterleaveST)
import           Data.Ext
import           Data.Geometry.Boundary
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Triangle
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Circular        as CV
import qualified Data.Vector.NonEmpty        as NE
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Exts                    (build)

data MutList s a = MutList
  { mutListVector  :: Vector a
  , mutListNextVec :: MU.MVector s Int
  , mutListPrevVec :: MU.MVector s Int
  }

-- O(n)
mutListFromVector :: Vector a -> ST s (MutList s a)
mutListFromVector vec = MutList vec
  <$> do
    arr <- U.unsafeThaw (U.enumFromN 1 (V.length vec))
    MU.unsafeWrite arr (V.length vec-1) 0
    pure arr
  <*> do
    arr <- U.unsafeThaw (U.enumFromN (-1) (V.length vec))
    MU.unsafeWrite arr 0 (V.length vec-1)
    pure arr

mutListClone :: MutList s a -> ST s (MutList s a)
mutListClone (MutList vec nextVec prevVec) = MutList vec
  <$> MU.clone nextVec
  <*> MU.clone prevVec

mutListNext :: MutList s a -> Int -> ST s Int
mutListNext m idx = MU.unsafeRead (mutListNextVec m) idx

mutListPrev :: MutList s a -> Int -> ST s Int
mutListPrev m idx = MU.unsafeRead (mutListPrevVec m) idx

mutListDelete :: MutList s a -> Int -> Int -> ST s ()
mutListDelete m prev next = do
  MU.unsafeWrite (mutListNextVec m) prev next
  MU.unsafeWrite (mutListPrevVec m) next prev

mutListInsert :: MutList s a -> Int -> Int -> Int -> ST s ()
mutListInsert m before after elt = do
  MU.unsafeWrite (mutListNextVec m) before elt  -- before.next = elt
  MU.unsafeWrite (mutListNextVec m) elt after   -- elt.next = after
  MU.unsafeWrite (mutListPrevVec m) after elt   -- after.prev = elt
  MU.unsafeWrite (mutListPrevVec m) elt before  -- elt.prev = before

earCheck :: (Num r, Ord r) => MutList s (Point 2 r :+ p) -> Int -> Int -> Int -> ST s Bool
earCheck vertices a b c = do
  let vs = mutListVector vertices
      pointA = V.unsafeIndex vs a
      pointB = V.unsafeIndex vs b
      pointC = V.unsafeIndex vs c
      trig = Triangle pointA pointB pointC

  let loop elt | elt == a = pure True
      loop elt = do
        let point = V.unsafeIndex vs elt ^. core
        case inTriangleRelaxed point trig of
          Outside -> loop =<< mutListNext vertices elt
          _       -> pure False
  if ccw' pointA pointB pointC == CCW
    then loop =<< mutListNext vertices c
    else pure False

{-
  We can check if a vertex is an ear in O(n) time. Checking all vertices will definitely
  yield at least one ear in O(n^2) time. So, finding N ears will take O(n^3) if done naively.

  Keeping a separate list of possible ears will improve matters. For each possible ear,
  we check if the vertex really is an ear or not. If it isn't, it is deleted from the
  list of possible ears. If it /is/ an ear, the vertex is cut and the neighbours are
  added back to the list of possible ears (if they aren't in the list already).

  So, start with a list of N possible ears, and we might add two vertices to the list
  ever time we find an ear. Since there are only N ears to be found, only 2*N vertices
  can be added to the list of possible ears in the worst case scenario. The list is
  therefore bounded to 3*N and finding all ears is therefore O(n^2).

  Note: When checking if a vertex is an ear, it is sufficient to check against
        reflex vertices. Some implementations keep a separate list of reflex
        vertices for this reason but it does increase the constant factor
        overhead. I think it's better to keep the constant factor low for small values
        of N and use the hashed algorithm for larger values of N.
-}
-- | \( O(n^2) \)
--
--   Returns triangular faces using absolute polygon point indices.
earClip :: (Num r, Ord r) => SimplePolygon p r -> [(Int,Int,Int)]
earClip poly = build gen
  where
    vs = NE.toVector $ CV.vector $ poly^.outerBoundaryVector
    gen :: ((Int,Int,Int) -> b -> b) -> b -> b
    gen cons nil = runST $ do
      vertices <- mutListFromVector vs
      possibleEars <- mutListClone vertices
      let worker len focus = do
            prev <- mutListPrev vertices focus
            next <- mutListNext vertices focus
            if len == 3
              then
                return $ cons (prev, focus, next) nil
              else do
                prevEar <- mutListPrev possibleEars focus
                nextEar <- mutListNext possibleEars focus
                isEar <- earCheck vertices prev focus next
                if isEar
                  then do
                    mutListDelete possibleEars prevEar nextEar
                    mutListDelete vertices prev next -- remove ear

                    case (prevEar /= prev, nextEar /= next) of
                      (True, True)  -> do
                        mutListInsert possibleEars prevEar nextEar prev
                        mutListInsert possibleEars prev nextEar next
                      (True, False) -> do
                        mutListInsert possibleEars prevEar nextEar prev
                      (False, True) -> do
                        mutListInsert possibleEars prevEar nextEar next
                      (False, False) -> return ()

                    rest <- unsafeInterleaveST $ worker (len-1) nextEar
                    return $ cons (prev, focus, next) rest
                  else do
                    mutListDelete possibleEars prevEar nextEar -- remove ear
                    worker len nextEar
      worker (V.length vs) 0

-- earClipRandom :: SimplePolygon p r -> [(Int,Int,Int)]
-- earClipRandom = undefined

-- earClipHashed :: SimplePolygon p r -> [(Int,Int,Int)]
-- earClipHashed = undefined

-- earClipRandomHashed :: SimplePolygon p r -> [(Int,Int,Int)]
-- earClipRandomHashed = undefined
