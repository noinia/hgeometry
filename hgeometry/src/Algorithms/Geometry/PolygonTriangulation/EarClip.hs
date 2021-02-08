{-# LANGUAGE BinaryLiterals #-}
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
  , earClipRandom
  ) where

import           Control.Lens                ((^.))
import           Control.Monad.ST            (ST, runST)
import           Control.Monad.Identity
import           Control.Monad.ST.Unsafe     (unsafeInterleaveST)
import           Data.Ext
import           Data.Bits
import           Data.Geometry.Boundary      (PointLocationResult (Outside))
import           Data.Geometry.Point         (Point, ccw', pattern CCW)
import           Data.Geometry.Polygon       (SimplePolygon, outerBoundaryVector)
import           Data.Geometry.Triangle      (Triangle (Triangle), inTriangleRelaxed)
import           Data.STRef
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import qualified Data.Vector.Circular        as CV
import qualified Data.Vector.NonEmpty        as NE
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           GHC.Exts                    (build)
import           System.Random
import Linear.V2

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

                    cons (prev, focus, next)
                      <$> unsafeInterleaveST (worker (len-1) nextEar)
                  else do -- not an ear
                    mutListDelete possibleEars prevEar nextEar -- remove vertex
                    worker len nextEar
      worker (V.length vs) 0

-- | \( O(n^2) \)
--
--   Returns triangular faces using absolute polygon point indices.
earClipRandom :: (Num r, Ord r) => SimplePolygon p r -> [(Int,Int,Int)]
earClipRandom poly = build gen
  where
    vs = NE.toVector $ CV.vector $ poly^.outerBoundaryVector
    gen :: ((Int,Int,Int) -> b -> b) -> b -> b
    gen cons nil = runST $ do
      vertices <- mutListFromVector vs
      possibleEars <- mutListClone vertices
      shuffled <- newShuffled (V.length vs)
      let worker len = do
            focus <- popShuffled shuffled
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
                        pushShuffled shuffled prev
                        pushShuffled shuffled next
                        mutListInsert possibleEars prevEar nextEar prev
                        mutListInsert possibleEars prev nextEar next
                      (True, False) -> do
                        pushShuffled shuffled prev
                        mutListInsert possibleEars prevEar nextEar prev
                      (False, True) -> do
                        pushShuffled shuffled next
                        mutListInsert possibleEars prevEar nextEar next
                      (False, False) -> return ()

                    cons (prev, focus, next)
                      <$> unsafeInterleaveST (worker (len-1))
                  else do -- not an ear
                    mutListDelete possibleEars prevEar nextEar -- remove vertex
                    worker len
      worker (V.length vs)

-- earClipHashed :: SimplePolygon p r -> [(Int,Int,Int)]
-- earClipHashed = undefined

-- earClipRandomHashed :: SimplePolygon p r -> [(Int,Int,Int)]
-- earClipRandomHashed = undefined


-------------------------------------------------------------------------------
-- Z-Order
-- https://en.wikipedia.org/wiki/Z-order_curve

zHash :: V2 Word -> Word
zHash (V2 a b) = zHashSingle a .|. (unsafeShiftL (zHashSingle b) 1)

zHashSingle :: Word -> Word
zHashSingle w
  | finiteBitSize w == 32 = zHashSingle32 w
  | otherwise             = zHashSingle64 w

zHashSingle32 :: Word -> Word
zHashSingle32 w = runIdentity $ do
    w <- pure $ w .&. 0x0000FFFF
    w <- pure $ (w .|. unsafeShiftL w 8)  .&. 0x00FF00FF
    w <- pure $ (w .|. unsafeShiftL w 4)  .&. 0x0F0F0F0F
    w <- pure $ (w .|. unsafeShiftL w 2)  .&. 0x33333333
    w <- pure $ (w .|. unsafeShiftL w 1)  .&. 0x55555555
    pure w

zHashSingle64 :: Word -> Word
zHashSingle64 w = runIdentity $ do
    w <- pure $ w .&. 0x00000000FFFFFFFF
    w <- pure $ (w .|. unsafeShiftL w 16) .&. 0x0000FFFF0000FFFF
    w <- pure $ (w .|. unsafeShiftL w 8)  .&. 0x00FF00FF00FF00FF
    w <- pure $ (w .|. unsafeShiftL w 4)  .&. 0x0F0F0F0F0F0F0F0F
    w <- pure $ (w .|. unsafeShiftL w 2)  .&. 0x3333333333333333
    w <- pure $ (w .|. unsafeShiftL w 1)  .&. 0x5555555555555555
    pure w

-------------------------------------------------------------------------------
-- Shuffled

data Shuffled s = Shuffled
  { shuffleCount  :: STRef s Int
  , shuffleVector :: MU.MVector s Int }

newShuffled :: Int -> ST s (Shuffled s)
newShuffled len = Shuffled <$> newSTRef len <*> U.unsafeThaw (U.enumFromN 0 len)

popShuffled :: Shuffled s -> ST s Int
popShuffled (Shuffled ref vector) = do
  count <- readSTRef ref
  writeSTRef ref (count-1)
  let idx = fst $ randomR (0, count-1) (mkStdGen count)
  val <- MU.unsafeRead vector idx
  MU.unsafeWrite vector idx =<< MU.unsafeRead vector (count-1)
  pure val

pushShuffled :: Shuffled s -> Int -> ST s ()
pushShuffled (Shuffled ref vector) val = do
  count <- readSTRef ref
  writeSTRef ref (count+1)
  MU.unsafeWrite vector count val

-------------------------------------------------------------------------------
-- MutList

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

-------------------------------------------------------------------------------
-- Ear checking

-- O(n)
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
