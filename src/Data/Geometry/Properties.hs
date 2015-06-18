{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Geometry.Properties where

import           Control.Applicative
import           Data.Maybe(isJust, mapMaybe, listToMaybe, fromJust)
import           Data.Proxy
import           Data.Vinyl.Core
import           Data.Vinyl.Functor
import           Data.Vinyl.Lens
import           Frames.CoRec
import           GHC.TypeLits

import qualified Data.Vinyl.TypeLevel as VTL

--------------------------------------------------------------------------------

-- | A type family for types that are associated with a dimension.
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
coRec = Col . Identity


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
  {-# MINIMAL intersect , nonEmptyIntersection #-}

  default nonEmptyIntersection :: ( NoIntersection ∈ IntersectionOf g h
                                  , RecApplicative (IntersectionOf g h)
                                  )
                                  => proxy g -> proxy h -> Intersection g h -> Bool
  nonEmptyIntersection = defaultNonEmptyIntersection


-- | When using IntersectionOf we may need some constraints that are always
-- true anyway.
type AlwaysTrueIntersection g h = RecApplicative (IntersectionOf g h)


-- | Returns True iff the resultt is a NoIntersection
defaultNonEmptyIntersection :: forall g h proxy.
                            ( NoIntersection ∈ IntersectionOf g h
                            , RecApplicative (IntersectionOf g h)
                            )
                            => proxy g -> proxy h -> Intersection g h -> Bool
defaultNonEmptyIntersection _ _ = isJust . asA (Proxy :: Proxy NoIntersection)


-- | Given a proxy of type t and a corec, test if the corec is a t.
asA             :: (t ∈ ts, RecApplicative ts) => proxy t -> CoRec Identity ts -> Maybe t
asA p c@(Col _) = rget p $ corecToRec' c


-- | Newtype around functions for a to b
newtype Handler b a = H (a -> b)

type Handlers ts b = Rec (Handler b) ts


-- | Pattern match on a CoRec by specifying handlers for each case. If the
-- CoRec is non-empty this function is total.
match      :: RecApplicative (t ': ts)
           => CoRec Identity (t ': ts) -> Handlers (t ': ts) b -> b
match c hs = fromJust $ match' c hs
           -- Since we require 'ts' both for the Handlers and the CoRec, Handlers
           -- effectively defines a total function. Hence, we can safely use fromJust

-- | Pattern match on a CoRec by specifying handlers for each case. The only case
-- in which this can produce a Nothing is if the list ts is empty.
match'      :: RecApplicative ts => CoRec Identity ts -> Handlers ts b -> Maybe b
match' c hs = match'' hs $ corecToRec' c
  where
    match''                            :: Handlers ts b -> Rec Maybe ts -> Maybe b
    match'' RNil         RNil          = Nothing
    match'' (H f :& _)  (Just x :& _)  = Just $ f x
    match'' (H _ :& fs) (Nothing :& c) = match'' fs c


-- foo :: CoRec Identity [NoIntersection, Int]
-- foo = Col (Identity NoIntersection)

bar :: CoRec Identity [NoIntersection, Int]
bar = Col (Identity (5 :: Int))

bar' :: CoRec Identity [NoIntersection, Int]
bar' = coRec (5 :: Int)


type IsAlwaysTrueFromEither a b = (VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z)))
                                  -- VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))(b  ∈ [a,b])

-- | Convert an either to a CoRec. The type class constraint is silly, and is
-- triviall true. Somehow GHC does not see that though.
fromEither           :: IsAlwaysTrueFromEither a b => Either a b -> CoRec Identity [a,b]
fromEither (Left x)  = coRec x
fromEither (Right x) = coRec x


-- fromEither'           :: ( RElem b [a,b] ((VTL.S VTL.Z))
--                          ) => Either a b -> CoRec Identity [a,b]

fromEither'           :: ( VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))
--                           VTL.RIndex b '[b] ~ VTL.Z
                         ) => Either a b -> CoRec Identity [a,b]
fromEither' (Left x)  = coRec x
fromEither' (Right x) = coRec x


-- handlers =  H (\NoIntersection -> "No Intersection")
--          :& H (\(x :: Int)     -> show x)
--          :& RNil


-- testMatch = match handlers bar


type family Union g h :: *

class IsUnionableWith g h where
  union :: g -> h -> Union g h
