{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Geometry.Properties where

import Data.Maybe (isNothing)
import Data.Proxy
import Data.Vinyl.CoRec
import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Lens
import GHC.TypeLits

-------------------------------------------------------------------------------

-- | A type family for types that are associated with a dimension. The
-- dimension is the dimension of the geometry they are embedded in.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: *

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
defaultNonEmptyIntersection _ _ = isNothing . asA (Proxy :: Proxy NoIntersection)


-- type IsAlwaysTrueFromEither a b = (VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z)))
--                                   -- VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))(b  ∈ [a,b])

-- -- | Convert an either to a CoRec. The type class constraint is silly, and is
-- -- triviall true. Somehow GHC does not see that though.
-- fromEither           :: IsAlwaysTrueFromEither a b => Either a b -> CoRec Identity [a,b]
-- fromEither (Left x)  = coRec x
-- fromEither (Right x) = coRec x


-- -- fromEither'           :: ( RElem b [a,b] ((VTL.S VTL.Z))
-- --                          ) => Either a b -> CoRec Identity [a,b]

-- fromEither'           :: ( VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))
-- --                           VTL.RIndex b '[b] ~ VTL.Z
--                          ) => Either a b -> CoRec Identity [a,b]
-- fromEither' (Left x)  = coRec x
-- fromEither' (Right x) = coRec x

type family Union g h :: *

class IsUnionableWith g h where
  union :: g -> h -> Union g h
