--
-- Implementation of
-- Simulation of Simplicity: A Technique to Cope with Degenerate Cases in Geometric Algorithms
--
-- By
-- Herbert Edelsbrunner and Ernst Peter Mucke
module Algorithms.Geometry.SoS where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as GV
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Vector as V
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

data Sign = Negative | Positive deriving (Show,Eq,Ord,Enum,Bounded)

flipSign :: Sign -> Sign
flipSign = \case
  Negative -> Positive
  Positive -> Negative

class AsPoint p where
  asPoint :: p -> Point (Dimension p) (NumType p)
  --
  indexOf :: p -> Int

instance AsPoint (Point d r :+ Int) where
  asPoint = view core
  indexOf = view extra

-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane.
--
sideTest      :: ( d ~ Dimension p, ToTerms d, SortI (d+1), Arity d, Arity (d+1)
                 , r ~ NumType p, Num r, Eq r
                 , AsPoint p
                 )
              => p -> Vector d p -> Sign
sideTest q ps = case (odd s', signDet m) of
                  (True,  d) -> flipSign d
                  (False, d) -> d
  where
    (s',m) = sort q ps


-- | Returns the number of comparisons and the sorted matrix, in which the rows are
-- sorted in increasing order of the indices.
sort      :: forall p d r
          . (Arity d, Arity (d+1), SortI (d + 1), AsPoint p, d ~ Dimension p, r ~ NumType p)
          => p -> Vector d p -> (Int, Vector (d + 1) (Vector d r))
sort q ps = second (fmap asVec) . sortI . fmap (with indexOf) $ q `GV.cons` ps
  where
    asVec (With _ p) = toVec . asPoint $ p

data With p = With {-# UNPACK #-} !Int p deriving (Show)
with     :: (p -> Int) -> p -> With p
with f p = With (f p) p
instance Eq (With p) where
  (With i _) == (With j _) = i == j
instance Ord (With p) where
  (With i _) `compare` (With j _) = i `compare` j

--------------------------------------------------------------------------------

class SortI d where
  sortI :: Vector d (With p) -> (Int, Vector d (With p))



instance SortI 2 where
  sortI v@(Vector2 i j) | i <= j    = (0, v)
                        | otherwise = (1, Vector2 j i)

instance SortI 3 where
  sortI v@(Vector3 i j k) =
    case i <= j of
      True  -> case j <= k of
                 True  -> (0,v)
                 False -> case k <= i of
                            True  -> (2, Vector3 k i j)
                            False -> (1, Vector3 i k j)
      False -> case j <= k of
                 True  -> case k <= i of
                            True  -> case j <= k of
                                       True  -> (2, Vector3 j k i)
                                       False -> (2, Vector3 k j i)
                            False -> (1,Vector3 j i k)
                 False -> case k <= i of
                            True  -> (1, Vector3 k j i)
                            False -> (1, Vector3 j i k)


instance SortI 4 where
  sortI v@(Vector4 i j k l) =
    case i <= j of
      True  -> case j <= k of
                 True  -> case k <= l of
                            True  -> (0, v)
                            False -> case l <= j of
                                       True -> case l <= i of
                                                 True  -> (4, Vector4 l i j k)
                                                 False -> (3, Vector4 i l j k)
                                       False -> undefined
                 False -> undefined
      False -> undefined



-- | Determines the sign of the Determinant.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDet :: (Num r, Eq r, ToTerms d) => Vector (d + 1) (Vector d r) -> Sign
signDet = List.head . mapMaybe signum' . toTerms
  where
    signum' x = case signum x of
                  -1    -> Just Negative
                  0     -> Nothing
                  1     -> Just Positive
                  _     -> error "signum': absurd"


-- | Determines the sign of the Determinant.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDetHom :: (Num r, Eq r, ToTermsHom d) => Matrix d d r -> Sign
signDetHom = List.head . mapMaybe signum' . toTermsHom
  where
    signum' x = case signum x of
                  -1    -> Just Negative
                  0     -> Nothing
                  1     -> Just Positive
                  _     -> error "signum': absurd"


class ToTerms d where
  toTerms :: Num r => Vector (d + 1) (Vector d r) -> [r]


instance ToTerms 1 where
  -- note this is Lambda_2 from the paper
  toTerms (Vector2 (Vector1 i)
                   (Vector1 j)) = [ i - j -- det22 [i 1, j 1] = i*1 - j*1
                                  , 1
                                  ]

instance ToTerms 2 where
  -- note this is Lambda_3 from the paper
  toTerms (Vector3 (Vector2 i1 i2)
                   (Vector2 j1 j2)
                   (Vector2 k1 k2)) = [ det33 $ V3 (V3 i1 i2 1)
                                                   (V3 j1 j2 1)
                                                   (V3 k1 k2 1)
                                      , -(j1 - k1) --

                                      , j2 - k2    -- det22 [[j2, 1], [k2, 1]] = j2*1 - k2*1
                                      , i1 - k1
                                      , 1
                                      ]


instance ToTerms 3 where
  -- note this is Lambda_4 from the paper
  toTerms (Vector4 (Vector3 i1 i2 i3)
                   (Vector3 j1 j2 j3)
                   (Vector3 k1 k2 k3)
                   (Vector3 l1 l2 l3)
          ) = [ det44 $ V4 (V4 i1 i2 i3 1)   -- 0
                           (V4 j1 j2 j3 1)
                           (V4 k1 k2 k3 1)
                           (V4 l1 l2 l3 1)
              , det33 $ V3 (V3 j1 j2 1)      -- 1
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , ((-1) *) .                   -- 2
                det33 $ V3 (V3 j1 j3 1)
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , det33 $ V3 (V3 j2 j3 1)      -- 3
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , ((-1) *) .                   -- 4
                det33 $ V3 (V3 i1 i2 1)
                           (V3 k1 k2 1)
                           (V3 l1 l2 1)
              , k1 - l1                      -- 5
              , -(k2 - l2)                   -- 6
              , det33 $ V3 (V3 i1 i3 1)      -- 7
                           (V3 k1 k3 1)
                           (V3 l1 l3 1)
              , k3 - l3                      -- 8
              , ((-1) *) .                   -- 9
                det33 $ V3 (V3 i2 i3 1)
                           (V3 k2 k3 1)
                           (V3 l2 l3 1)
              , det33 $ V3 (V3 i1 i2 1)      -- 10
                           (V3 j1 j2 1)
                           (V3 l1 l2 1)
              , -(j1 - l1)                   -- 11
              , j2 - l2                      -- 12
              , i1 - l1                      -- 13
              , 1                            -- 14
              ]

class ToTermsHom d where
  toTermsHom :: Matrix d d r -> [r]




-- class ToTermsHom 2 where
--   toTerms m@(Matrix (Vector2 i1 i2)
--                     (Vector2 j1 j2)
--             )                     = [ det22 m
--                                     , -i1
--                                     , j2
--                                     ,
--                                     ]


-- toTerms   :: Matrix d d r -> [r]
-- toTerms m = undefined



--------------------------------------------------------------------------------


test1 :: Sign
test1 = sideTest (Point1 1 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)

test2 :: Sign
test2 = sideTest (Point1 5 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)


test3 :: Sign
test3 = sideTest (Point2 (-1) 5 :+ 0 :: Point 2 Int :+ Int) (Vector2 (Point2 0 0  :+ 1)
                                                                     (Point2 0 10 :+ 2)
                                                            )


pattern Point1 x = Point (Vector1 x)
