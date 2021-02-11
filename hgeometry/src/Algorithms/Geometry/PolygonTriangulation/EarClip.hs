{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
  , earClipHashed
  , earClipRandomHashed
  , zHash
  , zUnHash
  ) where

import           Control.Lens                 ((^.))
import           Control.Monad.Identity
import           Control.Monad.ST             (ST, runST)
import           Control.Monad.ST.Unsafe      (unsafeInterleaveST)
import           Data.Bits
import           Data.Ext
import           Data.Geometry.Boundary       (PointLocationResult (Outside))
import           Data.Geometry.Point          (Point (Point2), ccw', pattern CCW, xCoord, yCoord)
import           Data.Geometry.Polygon
import           Data.Geometry.Triangle       (Triangle (Triangle), inTriangleRelaxed)
import           Data.STRef
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as Algo
import qualified Data.Vector.Circular         as CV
import qualified Data.Vector.NonEmpty         as NE
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable  as MU
import           GHC.Exts                     (build)
import           Linear.V2
import           System.Random                (mkStdGen, randomR)

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

-- | \( O(n \log n) \) expected time.
--
--   Returns triangular faces using absolute polygon point indices.
earClipHashed :: Real r => SimplePolygon p r -> [(Int,Int,Int)]
earClipHashed poly = build gen
  where
    vs = NE.toVector $ CV.vector $ poly^.outerBoundaryVector
    n = V.length vs
    bb = boundingBox vs
    zHashVec = U.generate n $ \i -> zHashPoint bb (V.unsafeIndex vs i ^. core)
    gen :: ((Int,Int,Int) -> b -> b) -> b -> b
    gen cons nil = runST $ do
      vertices <- mutListFromVector vs
      zHashes <- mutListSort zHashVec
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
                isEar <- earCheckHashed bb vertices zHashes prev focus next
                if isEar
                  then do
                    mutListDelete possibleEars prevEar nextEar
                    mutListDelete vertices prev next -- remove ear
                    mutListDeleteFocus zHashes focus

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
      worker n 0

-- | \( O(n \log n) \) expected time.
--
--   Returns triangular faces using absolute polygon point indices.
earClipRandomHashed :: Real r => SimplePolygon p r -> [(Int,Int,Int)]
earClipRandomHashed poly = build gen
  where
    vs = NE.toVector $ CV.vector $ poly^.outerBoundaryVector
    n = V.length vs
    bb = boundingBox vs
    zHashVec = U.generate n $ \i -> zHashPoint bb (V.unsafeIndex vs i ^. core)
    gen :: ((Int,Int,Int) -> b -> b) -> b -> b
    gen cons nil = runST $ do
      vertices <- mutListFromVector vs
      zHashes <- mutListSort zHashVec
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
                isEar <- earCheckHashed bb vertices zHashes prev focus next
                if isEar
                  then do
                    mutListDelete possibleEars prevEar nextEar
                    mutListDelete vertices prev next -- remove ear
                    mutListDeleteFocus zHashes focus

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
      worker n

-------------------------------------------------------------------------------
-- Bounding box

-- Returns (minX, widthX, minY, heightY)
boundingBox :: Real r => V.Vector (Point 2 r :+ p) -> (r, Double, r, Double)
boundingBox v =
    case V.foldl' fn (initX,initX,initY,initY) (V.unsafeTail v) of
      (minX, maxX, minY, maxY) -> (minX, realToFrac (maxX-minX), minY, realToFrac (maxY-minY))
  where
    fn (minX, maxX, minY, maxY) (Point2 x y :+ _) =
      (min minX x, max maxX x, min minY y, max maxY y)
    Point2 initX initY = V.unsafeHead v ^. core

triangleBoundingBox :: Ord r => Triangle 2 p r -> (Point 2 r, Point 2 r)
triangleBoundingBox (Triangle a b c) =
    (Point2 minTX minTY, Point2 maxTX maxTY)
  where
    min3 v1 v2 v3
        | v1 < v2   = if v1 < v3 then v1 else v3
        | otherwise = if v2 < v3 then v2 else v3
    max3 v1 v2 v3
      | v1 > v2   = if v1 > v3 then v1 else v3
      | otherwise = if v2 > v3 then v2 else v3
    minTX = min3 (a^.core.xCoord) (b^.core.xCoord) (c^.core.xCoord)
    minTY = min3 (a^.core.yCoord) (b^.core.yCoord) (c^.core.yCoord)
    maxTX = max3 (a^.core.xCoord) (b^.core.xCoord) (c^.core.xCoord)
    maxTY = max3 (a^.core.yCoord) (b^.core.yCoord) (c^.core.yCoord)

-------------------------------------------------------------------------------
-- Z-Order
-- https://en.wikipedia.org/wiki/Z-order_curve

zHashPoint :: Real r => (r,Double,r,Double) -> Point 2 r -> Word
zHashPoint (minX, widthX, minY, heightY) (Point2 x y) =
    zHash (V2 x' y')
  where
    x' = round (realToFrac (x-minX) / widthX * zHashMax)
    y' = round (realToFrac (y-minY) / heightY * zHashMax)

zHashMax :: Double
zHashMax = realToFrac zHashMaxW

zHashMaxW :: Word
zHashMaxW = if finiteBitSize zHashMaxW == 32 then 0xFFFF else 0xFFFFFFFF

zHash :: V2 Word -> Word
zHash (V2 a b) = zHashSingle a .|. (unsafeShiftL (zHashSingle b) 1)

zUnHash :: Word -> V2 Word
zUnHash z =
  V2 (zUnHashSingle z) (zUnHashSingle (unsafeShiftR z 1))

zHashSingle :: Word -> Word
zHashSingle w
  | finiteBitSize w == 32 = zHashSingle32 w
  | otherwise             = zHashSingle64 w

zUnHashSingle :: Word -> Word
zUnHashSingle w
  | finiteBitSize w == 32 = zUnHashSingle32 w
  | otherwise             = zUnHashSingle64 w

zHashSingle32 :: Word -> Word
zHashSingle32 w = runIdentity $ do
    w <- pure $ w .&. 0x0000FFFF
    w <- pure $ (w .|. unsafeShiftL w 8)  .&. 0x00FF00FF
    w <- pure $ (w .|. unsafeShiftL w 4)  .&. 0x0F0F0F0F
    w <- pure $ (w .|. unsafeShiftL w 2)  .&. 0x33333333
    w <- pure $ (w .|. unsafeShiftL w 1)  .&. 0x55555555
    pure w

zUnHashSingle32 :: Word -> Word
zUnHashSingle32 w = runIdentity $ do
    w <- pure $ w .&. 0x55555555
    w <- pure $ (w .|. unsafeShiftR w 1)  .&. 0x33333333
    w <- pure $ (w .|. unsafeShiftR w 2)  .&. 0x0F0F0F0F
    w <- pure $ (w .|. unsafeShiftR w 4)  .&. 0x00FF00FF
    w <- pure $ (w .|. unsafeShiftR w 8)  .&. 0x0000FFFF
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

zUnHashSingle64 :: Word -> Word
zUnHashSingle64 w = runIdentity $ do
    w <- pure $ w .&. 0x5555555555555555
    w <- pure $ (w .|. unsafeShiftR w 1) .&. 0x3333333333333333
    w <- pure $ (w .|. unsafeShiftR w 2)  .&. 0x0F0F0F0F0F0F0F0F
    w <- pure $ (w .|. unsafeShiftR w 4)  .&. 0x00FF00FF00FF00FF
    w <- pure $ (w .|. unsafeShiftR w 8)  .&. 0x0000FFFF0000FFFF
    w <- pure $ (w .|. unsafeShiftR w 16)  .&. 0x00000000FFFFFFFF
    pure w

-------------------------------------------------------------------------------
-- Shuffled

data Shuffled s = Shuffled
  { shuffleCount  :: STRef s Int
  , shuffleVector :: MU.MVector s Int }

newShuffled :: Int -> ST s (Shuffled s)
newShuffled len = Shuffled <$> newSTRef len <*> U.unsafeThaw (U.enumFromN 0 len)

popShuffled :: Shuffled s -> ST s Int
popShuffled Shuffled{..} = do
  count <- readSTRef shuffleCount
  writeSTRef shuffleCount (count-1)
  let idx = fst $ randomR (0, count-1) (mkStdGen count)
  val <- MU.unsafeRead shuffleVector idx
  MU.unsafeWrite shuffleVector idx =<< MU.unsafeRead shuffleVector (count-1)
  pure val

pushShuffled :: Shuffled s -> Int -> ST s ()
pushShuffled (Shuffled ref vector) val = do
  count <- readSTRef ref
  writeSTRef ref (count+1)
  MU.unsafeWrite vector count val

-------------------------------------------------------------------------------
-- MutList

data MutList s a = MutList
  { mutListIndex   :: (Int -> a)
  , mutListNextVec :: MU.MVector s Int
  , mutListPrevVec :: MU.MVector s Int
  }

-- O(n)
mutListFromVector :: Vector a -> ST s (MutList s a)
mutListFromVector vec = MutList (V.unsafeIndex vec)
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

mutListDeleteFocus :: MutList s a -> Int -> ST s ()
mutListDeleteFocus m focus = do
  prev <- mutListPrev m focus
  next <- mutListNext m focus
  unless (prev == -1) $
    MU.unsafeWrite (mutListNextVec m) prev next
  unless (next == -1) $
    MU.unsafeWrite (mutListPrevVec m) next prev

mutListInsert :: MutList s a -> Int -> Int -> Int -> ST s ()
mutListInsert m before after elt = do
  MU.unsafeWrite (mutListNextVec m) before elt  -- before.next = elt
  MU.unsafeWrite (mutListNextVec m) elt after   -- elt.next = after
  MU.unsafeWrite (mutListPrevVec m) after elt   -- after.prev = elt
  MU.unsafeWrite (mutListPrevVec m) elt before  -- elt.prev = before

mutListSort :: (Ord a, MU.Unbox a) => U.Vector a -> ST s (MutList s a)
mutListSort vec = do
    sorted <- do
      arr <- U.unsafeThaw $ (U.enumFromN 0 n :: U.Vector Int)
      Algo.sortBy (\a b -> compare (U.unsafeIndex vec a) (U.unsafeIndex vec b)) arr
      U.unsafeFreeze arr

    next <- MU.new n
    prev <- MU.new n
    MU.write next
      (U.unsafeIndex sorted (n-1))
      (-1)
    forM_ [0..n-2] $ \i -> do
      MU.write next
        (U.unsafeIndex sorted i)
        (U.unsafeIndex sorted (i+1))
    MU.write prev
      (U.unsafeIndex sorted 0)
      (-1)
    forM_ [1..n-1] $ \i -> do
      MU.write prev
        (U.unsafeIndex sorted i)
        (U.unsafeIndex sorted (i-1))
    pure $ MutList (U.unsafeIndex vec) next prev
  where
    n = U.length vec

-------------------------------------------------------------------------------
-- Ear checking

-- O(n)
earCheck :: (Num r, Ord r) => MutList s (Point 2 r :+ p) -> Int -> Int -> Int -> ST s Bool
earCheck vertices a b c = do
  let pointA = mutListIndex vertices a
      pointB = mutListIndex vertices b
      pointC = mutListIndex vertices c
      trig = Triangle pointA pointB pointC

  let loop elt | elt == a = pure True
      loop elt = do
        let point = mutListIndex vertices elt ^. core
        case inTriangleRelaxed point trig of
          Outside -> loop =<< mutListNext vertices elt
          _       -> pure False
  if ccw' pointA pointB pointC == CCW
    then loop =<< mutListNext vertices c
    else pure False

-- showBinary :: (Integral a, Show a) => a -> String
-- showBinary i = showIntAtBase 2 intToDigit i ""

earCheckHashed :: Real r => (r, Double, r, Double) -> MutList s (Point 2 r :+ p) -> MutList s Word -> Int -> Int -> Int -> ST s Bool
earCheckHashed bb vertices zHashes a b c = do
  let pointA = mutListIndex vertices a
      pointB = mutListIndex vertices b
      pointC = mutListIndex vertices c
      trig = Triangle pointA pointB pointC
      (lowPt, highPt) = triangleBoundingBox trig

      minZ = zHashPoint bb lowPt
      maxZ = zHashPoint bb highPt

  let upwards up
        | up == -1 || upZ > maxZ = pure True
        | inTriangleRelaxed pointUp trig /= Outside = pure False
        | otherwise = upwards =<< mutListNext zHashes up
        where
          upZ = mutListIndex zHashes up
          pointUp = mutListIndex vertices up ^. core
      downwards down
        | down == -1 || downZ < minZ = pure True
        | inTriangleRelaxed pointDown trig /= Outside = pure False
        | otherwise = downwards =<< mutListPrev zHashes down
        where
          downZ = mutListIndex zHashes down
          pointDown = mutListIndex vertices down ^. core
      bidirectional up down
        | up == -1   || upZ > maxZ   = downwards down
        | down == -1 || downZ < minZ = upwards up
        | up /= a && up /= b && inTriangleRelaxed pointUp trig /= Outside = pure False
        | down /= a && down /= b && inTriangleRelaxed pointDown trig /= Outside = pure False
        | otherwise = do
          up' <- mutListNext zHashes up
          down' <- mutListPrev zHashes down
          bidirectional up' down'
        where
          upZ = mutListIndex zHashes up
          downZ = mutListIndex zHashes down
          pointUp = mutListIndex vertices up ^. core
          pointDown = mutListIndex vertices down ^. core
  if ccw' pointA pointB pointC == CCW
    then bidirectional b b
    else pure False
