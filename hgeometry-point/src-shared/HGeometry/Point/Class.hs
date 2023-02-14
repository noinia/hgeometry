{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types that can act as \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module HGeometry.Point.Class
  ( HasVector(..)
  , Affine_(..)
  , Point_(..) -- , pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  -- , origin
  -- , pointFromPoint, pointFromList
  , coord
  -- , xCoord, yCoord, zCoord, wCoord

  -- , projectPoint
  -- , PointFor
  -- , HasPoints(..), HasPoints'
  ) where

import           Control.Lens
-- import           Data.Ext
import           Data.Function (on)
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Class
-- import           HGeometry.Vector
import qualified Linear.Affine as Linear

--------------------------------------------------------------------------------

-- | Types that have a 'vector' field lens
class HasVector point where

  -- | Lens to access the vector corresponding to this point.
  --
  -- >>> myPoint ^. vector
  -- Vector3 1 2 3
  -- >>> ( myPoint & vector .~ Vector3 3 2 1  ) :: Point 3 Int
  -- Point3 3 2 1
  -- >>> (myPoint & coordinates %~ show ) :: Point 3 String
  -- Point3 "1" "2" "3"
  vector :: ( Dimension point ~ d
            , NumType point ~ r
            )
         => Lens' point (Vector d r)

type instance Dimension (Linear.Point v r) = Dimension (v r)
type instance NumType (Linear.Point v r)   = r


instance ( Vector_ (v r)
         , IxValue (v r) ~ r
         ) => HasVector (Linear.Point v r) where
  vector = Linear._Point._Vector
  {-# INLINE vector #-}

-- | Affine space; essentially the same as Linear.Affine, but for
-- points of kind Type rather than (Type -> Type).
class ( Additive_ (Vector d r)
      , d ~ Dimension point, r ~ NumType point
      ) => Affine_ point d r | point -> d
                             , point -> r where
  {-# MINIMAL (.-.), (.+^) #-}

  -- | p .-. q represents the vector from q to p
  (.-.) :: Num r => point -> point -> Vector d r

  -- | add a vector to a point
  --
  -- >>> myPoint .+^ Vector3 100 200 300
  -- Point3 101 202 303
  (.+^) :: Num r => point -> Vector d r -> point

  -- | subtract a vector from a point
  --
  -- >>> myPoint .-^ Vector3 100 200 300
  -- Point3 (-99) (-198) (-297)
  (.-^) :: Num r => point -> Vector d r -> point
  p .-^ v = p .+^ negated v
  {-# INLINE (.-^) #-}

instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r)
         , Additive_ (Vector d r)
         ) => Affine_ (Linear.Point v r) d r where
  p .-. q = (p^.vector) ^-^ (q^.vector)
  {-# INLINE (.-.) #-}
  p .+^ v = p&vector %~ (^+^ v)
  {-# INLINE (.+^) #-}

--------------------------------------------------------------------------------

-- | A class representing points in d-dimensional space.
class ( Dimension point ~ d
      , NumType point   ~ r
      , HasVector point
      , Affine_ point d r
      -- , Vector_ (VectorFor point) d r
      ) => Point_ point d r | point -> d
                            , point -> r where
  {-# MINIMAL fromVector #-}

  -- | Construct a point from a vector
  --
  -- >>> fromVector (Vector4 1 2 3 4) :: Point 4 Int
  -- Point4 1 2 3 4
  fromVector :: Vector d r -> point

  -- | Traversal over *all* coordinates of the points. Coordinates are 1-indexed.
  --
  -- >>> imapMOf_ coordinates (\i x -> print (i,x)) (Point2 10 20 :: Point 2 Int)
  -- (1,10)
  -- (2,20)
  -- >>> itraverseOf coordinates (\i x -> print (i,x)) (Point2 10 20) :: IO (Point 2 ())
  -- (1,10)
  -- (2,20)
  -- Point2 () ()
  -- >>> over coordinates (+1) $ Point2 10 20 :: Point 2 Int
  -- Point2 11 21
  coordinates :: IndexedTraversal1 Int point point r r
  coordinates = vector  . reindexed (+1) components
    -- where
      -- tr :: IndexedTraversal Int (Vector d ) (VectorFor point') r s
      -- tr =
  {-# INLINE coordinates #-}

  -- | Get the coordinate in a given dimension. This operation is unsafe in the
  -- sense that no bounds are checked. Consider using `coord` instead.
  --
  -- >>> myPoint ^.. coord' 2
  -- [2]
  coord'   :: Int -> IndexedTraversal' Int point r
  coord' i = vector . elem'
    where
      -- elem' :: IndexedTraversal' Int (VectorFor point) r
      elem' = component' (i - 1)
                -- vectors are 0 indexed, whereas we are 1 indexed.
  {-# INLINE coord' #-}



-- | Get the coordinate in a given dimension
--
-- >>> myPoint ^. coord @2
-- 2
-- >>> myPoint & coord @1 .~ 10
-- Point3 10 2 3
-- >>> myPoint & coord @3 %~ (+1)
-- Point3 1 2 4
coord :: forall i point d r. (1 <= i, i <= d, KnownNat i, Point_ point d r)
      => IndexedLens' Int point r
coord = singular $ coord' (fromIntegral . natVal $ Proxy @i)
{-# INLINE coord #-}


instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r)
         , Additive_ (Vector d r)
         ) => Point_ (Linear.Point v r) d r where
  fromVector = Linear.P . review _Vector
