{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines a data type for representing intersections. Mostly useful
-- for the more geometric types.
--
--------------------------------------------------------------------------------
module Data.Intersection where

import Data.Maybe (isNothing)
import Data.Vinyl.CoRec
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Lens

-------------------------------------------------------------------------------

-- | A simple data type expressing that there are no intersections
data NoIntersection = NoIntersection deriving (Show,Read,Eq,Ord)

-- | The result of interesecting two geometries is a CoRec,
type Intersection g h = CoRec Identity (IntersectionOf g h)

-- | The type family specifying the list of possible result types of an
-- intersection.
type family IntersectionOf g h :: [*]

-- | Helper to produce a corec
coRec :: (a ∈ as) => a -> CoRec Identity as
coRec = CoRec . Identity

class IsIntersectableWith g h where
  intersect :: g -> h -> Intersection g h

  -- | g `intersects` h  <=> The intersection of g and h is non-empty.
  --
  -- The default implementation computes the intersection of g and h,
  -- and uses nonEmptyIntersection to determine if the intersection is
  -- non-empty.
  intersects :: g -> h -> Bool
  g `intersects` h = nonEmptyIntersection (Identity g) (Identity h) $ g `intersect` h

  -- | Helper to implement `intersects`.
  nonEmptyIntersection :: proxy g -> proxy h -> Intersection g h -> Bool
  {-# MINIMAL intersect, nonEmptyIntersection #-}

  default nonEmptyIntersection :: ( NoIntersection ∈ IntersectionOf g h
                                  , RecApplicative (IntersectionOf g h)
                                  )
                                  => proxy g -> proxy h -> Intersection g h -> Bool
  nonEmptyIntersection = defaultNonEmptyIntersection


-- | When using IntersectionOf we may need some constraints that are always
-- true anyway.
type AlwaysTrueIntersection g h = RecApplicative (IntersectionOf g h)


-- | Returns True iff the result is *not* a NoIntersection
defaultNonEmptyIntersection :: forall g h proxy.
                            ( NoIntersection ∈ IntersectionOf g h
                            , RecApplicative (IntersectionOf g h)
                            )
                            => proxy g -> proxy h -> Intersection g h -> Bool
defaultNonEmptyIntersection _ _ = isNothing . asA @NoIntersection
