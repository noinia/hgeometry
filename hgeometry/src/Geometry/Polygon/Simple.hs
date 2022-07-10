{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Simple
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Simple
  ( SimplePolygon_(..)
  , SimplePolygon
  , SimplePolygonF
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           GHC.Generics
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Simple.Class
import           Geometry.Properties

import           Data.Maybe
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as GV
-- import           Data.Vector.Circular (CircularVector)
-- import qualified Data.Vector.Circular as CV
-- import qualified Data.Vector.Circular.Util as CV

--------------------------------------------------------------------------------

newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable)

instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v
instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

--------------------------------------------------------------------------------


-- | Simple polygons just store their vertices in CCCW order
newtype SimplePolygonF f point r = MkSimplePolygon (f (point 2 r))
  deriving (Generic)

-- | By default we store simple polygons as circular vectors.
type SimplePolygon = SimplePolygonF (Cyclic Vector.Vector)

type instance Dimension (SimplePolygonF f point r) = 2
type instance NumType   (SimplePolygonF f point r) = r

deriving instance Eq (f (point 2 r)) => Eq (SimplePolygonF f point r)

-- instance Wrapped   (SimplePolygonF f point r)
-- instance Rewrapped (SimplePolygonF f point r) (f (point 2 r))

-- | Access the container
_SimplePolygonF :: Iso (SimplePolygonF f point r) (SimplePolygonF f' point' r')
                       (f (point 2 r))              (f' (point' 2 r'))
_SimplePolygonF = iso (\(MkSimplePolygon vs) -> vs) MkSimplePolygon

instance TraversableWithIndex Int f
      => HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r') where
  type Vertex   (SimplePolygonF f point r) = point 2 r
  type VertexIx (SimplePolygonF f point r) = Int
  vertices = _SimplePolygonF . itraversed

-- instance HasVertices (SimplePolygon point r) (SimplePolygon point' r') where
--   type Vertex   (SimplePolygon point r) = point 2 r
--   type VertexIx (SimplePolygon point r) = Int
--   -- vertices = _SimplePolygonF . CV.itraversedRight

class HasFromFoldable f where
  fromFoldable :: Foldable g => g a -> f a
  fromFoldable = fromList . F.toList

  fromList :: [a] -> f a
  {-# MINIAL fromList #-}


instance HasFromFoldable Vector.Vector  where
  fromList = Vector.fromList

instance ( TraversableWithIndex Int f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         )
      => HasVertices' (SimplePolygonF f point r) where
  vertexAt i = _SimplePolygonF . iix i


instance ( TraversableWithIndex Int f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         )
      => HasOuterBoundary (SimplePolygonF f point r) where
  outerBoundary = vertices
  outerBoundaryVertexAt i = singular (vertexAt i)

instance ( Point_ point 2 r
         , TraversableWithIndex Int f
         , HasFromFoldable f
         , Ixed (f (point 2 r))
         , IxValue (f (point 2 r)) ~ point 2 r
         , Index (f (point 2 r)) ~ Int
         ) => SimplePolygon_ (SimplePolygonF f) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable


testPoly :: SimplePolygon Point Int
testPoly = uncheckedFromCCWPoints [Point2 10 20, origin, Point2 0 100]


myVertices :: IndexedFold Int (SimplePolygon point r) (point 2 r)
myVertices = vertices


-- withSuccessors :: forall polygon. ( HasVertices polygon polygon)
--                => IndexedFold (VertexIx polygon) polygon ( Vertex polygon
--                                                          , Vertex polygon
--                                                          )

-- successors :: ( HasVertices polygon polygon
--               , VertexIx polygon ~ Int
--               )
--            => IndexedTraversal' (VertexIx polygon) polygon (Vertex polygon)
-- successors = \f pg -> let n = numVertices pg
--                       in undefined
--                         -- vertices . imap (\i -> )

                        -- (reindexed (\i -> (i-1) `mod` n) vertices) f pg

-- sucessors'               :: Int -> IndexedTraversal' (VertexIx polygon) polygon (Vertex polygon)
--                          -> IndexedTraversal' (VertexIx polygon) polygon (Vertex polygon)
-- sucessors' n theVertices = theVertices . imap (\i _ -> )

-- | gets the ith vertex on the outer boundary
-- outerBoundaryVertex   :: ( HasVertices polygon polygon
--                          , IxValue (Vertex polygon) ~ Vertex polygon
--                          , Index (Vertex polygon) ~ VertexIx polygon
--                          )
--                       => VertexIx polygon
--                       -> IndexedGetter (VertexIx polygon) polygon (Vertex polygon)
-- outerBoundaryVertex i = vertices . iix i

-- myVertices' :: IndexedFold Int (SimplePolygon point r) (Int, point 2 r)
-- myVertices' = myVertices . withIndex .
--   where
--     f = undefined
    -- f (i,u) = (i+1,u)
  --   f = undefined
--     f i u = (i,u)



-- withSuccessors :: forall polygon. ( HasVertices polygon polygon)
--                => IndexedFold (VertexIx polygon) polygon ( Vertex polygon
--                                                          , Vertex polygon
--                                                          )
-- withSuccessors = myVertices . imap f
--   where
--     f     :: Int -> Vertex polygon -> (Vertex polygon, Vertex polygon)
--     f i u = undefined -- (u, undefined )


  -- combine vertices (reindexed (+1) vertices)
  -- where
  --   combine origs nexts = undefined




  -- vertices <> successors
  -- where
  --   successors :: IndexedFold (VertexIx polygon) polygon ( Vertex polygon)
  --   successors = undefined
  -- -- reindex (\i -> i `mod` n) vertices

fold1 :: Fold [Int] [String]
fold1 = undefined

test :: Num a => IndexedFold Int [a] a
test = ifolding sum'

sum' :: Num a => [a] -> Identity (Int,a)
sum' = Identity . (20,) . sum

-- test = toListOf myFold [1,2,3,4]
--   where
--     myFold :: Fold [Int] Int
--     myFold = runIdentity . folding (\xs -> Identity $ sum xs)


      -- \aToFa -- :: Int -> f Int
      --         xs     -- :: [Int]
      --        -> f <$> xs

      --          foldr (\x a -> (+) <$> aToFa x <*> a
      --                 ) (pure 0) xs
      --           (\x -> aToFa x)

      --           xs

      --          contramap fs aToFa -- to construct a thing of type :: f s ~ f [Int]


  -- f is contravariant, so given : (a -> b) -> f b -> f a
  -- and applicative

--------------------------------------------------------------------------------

-- | Convex polygons
newtype ConvexPolygonF f point r =
  ConvexPolygon { toSimplePolygon :: SimplePolygonF f point r }

type ConvexPolygon = ConvexPolygonF (Cyclic Vector.Vector)

-- | ConvexPolygons are isomorphic to SimplePolygons with the added
--   constraint that they have no reflex vertices.
--
-- Note that this is unchecked; i.e. one can turn an arbitrary simple polygon
-- into a suposedly convex one.
_UncheckedConvexPolygon :: Iso (ConvexPolygonF f point r) (ConvexPolygonF f' point' s)
                               (SimplePolygonF f point r) (SimplePolygonF f' point' s)
_UncheckedConvexPolygon = iso toSimplePolygon ConvexPolygon

-- | Prism that can forget that the polygon is convex
--
_ConvexPolygon :: forall f point r. (Num r, Ord r)
               => Prism' (SimplePolygonF f point r) (ConvexPolygonF f point r)
_ConvexPolygon = prism' toSimplePolygon fromPolygon
  where
    fromPolygon                          :: SimplePolygonF f point r
                                         -> Maybe (ConvexPolygonF f point r)
    fromPolygon pg | isStrictlyConvex pg = Just (ConvexPolygon pg)
                   | otherwise           = Nothing

-- deriving instance Eq (ConvexPolygonF f point r)
-- | Polygons are per definition 2 dimensional
type instance Dimension (ConvexPolygonF f point r) = 2
type instance NumType   (ConvexPolygonF f point r) = r

instance ( HasVertices (SimplePolygonF f point r) (SimplePolygonF f point' r')
         )
      => HasVertices (ConvexPolygonF f point r) (ConvexPolygonF f point' r') where
  type Vertex   (ConvexPolygonF f point r) = Vertex   (SimplePolygonF f point r)
  type VertexIx (ConvexPolygonF f point r) = VertexIx (SimplePolygonF f point r)
  vertices = _UncheckedConvexPolygon . vertices

instance HasVertices' (SimplePolygonF f point r) => HasVertices' (ConvexPolygonF f point r) where
  vertexAt i = _UncheckedConvexPolygon . vertexAt i

instance ( HasOuterBoundary (SimplePolygonF f point r)
         , VertexIx (SimplePolygonF f point r) ~ Int
         ) =>
         HasOuterBoundary (ConvexPolygonF f point r) where
  outerBoundary = _UncheckedConvexPolygon . outerBoundary
  outerBoundaryVertexAt i = _UncheckedConvexPolygon . outerBoundaryVertexAt i

instance ( SimplePolygon_ (SimplePolygonF f) point r
         , Point_ point 2 r
         ) => SimplePolygon_ (ConvexPolygonF f) point r where
  -- | Additional precondition: the points actually form a convex polygon
  uncheckedFromCCWPoints = ConvexPolygon . uncheckedFromCCWPoints
  fromPoints = fmap ConvexPolygon . fromPoints
    -- FIXME: here we could actually test if the thing is convex

-- -- | Smart constructor to construct a convex polygon from a simple polygon.
-- fromSimplePolygon :: SimplePolygonF f point r -> Maybe (ConvexPolygonF f point r)
-- fromSimplePolygon pg
--   | isConvex pg = Just (ConvexPolygon pg)
--   | otherwise   = Nothing



-- | \( O(n) \) Check if a polygon is strictly convex.
isStrictlyConvex   :: (Ord r, Num r) => SimplePolygonF f point r -> Bool
isStrictlyConvex s = undefined
  --   CV.and (CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs))
  -- where
  --   f a b c = ccw' a b c == CCW
  --   vs = s ^. outerBoundaryVector

-- | \( O(n) \) Check if a polygon is convex.
isConvex :: (Ord r, Num r) => SimplePolygonF f point r -> Bool
isConvex s = undefined

-- | \( O(n) \) Verify that a convex polygon is strictly convex.
verifyConvex :: (Ord r, Num r) => ConvexPolygonF f point r -> Bool
verifyConvex = isStrictlyConvex . toSimplePolygon
