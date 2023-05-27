module HGeometry.LowerEnvelope.Triangulate
  ( triangulate
  , triangulateFace
  ) where

import           Control.Lens
import           Data.Function (on)
import           Data.Ord (comparing)
import qualified Data.Vector as Boxed
import           HGeometry.Foldable.Sort
import           HGeometry.LowerEnvelope.Type

--------------------------------------------------------------------------------

-- | Triangulates all faces.
--
-- \(O(n \log n)\)

triangulate :: ( Foldable g, Applicative g
               , Ord r, Num r
               , Monoid (g (HalfEdge r))
               , Cons (g (HalfEdge r)) (g (HalfEdge r)) (HalfEdge r) (HalfEdge r)
               ) => LowerEnvelope f g r -> LowerEnvelope f Boxed.Vector r
triangulate = over halfEdges triangulateFaces'

-- | Given some
--
-- \(O(n \log n)\)
triangulateFaces' :: ( Foldable f, Applicative f
                     , Ord r, Num r
                     , Monoid (f (HalfEdge r))
                     , Cons (f (HalfEdge r)) (f (HalfEdge r)) (HalfEdge r) (HalfEdge r)
                     ) => f (HalfEdge r) -> Boxed.Vector (HalfEdge r)
triangulateFaces' = sortBy aroundOrigins . foldMap triangulateFace . edgesByFace


-- | triangulate a convex face
triangulateFace      :: ( Foldable f
                        , Applicative f
                        , Monoid (f (HalfEdge r))
                        , Cons (f (HalfEdge r)) (f (HalfEdge r)) (HalfEdge r) (HalfEdge r)
                        )
                     => f (HalfEdge r) -- ^ the edges in CCW order along the face
                     -> f (HalfEdge r)
triangulateFace face = foldMap mkEdge rest' <> face
  where
    Just (e0,rest)  = uncons face --
    Just (_ ,rest') = uncons rest -- the face has at least 2 vertices, so this should be safe
    u = e0^.origin
    h = e0^.leftPlane
    mkEdge e = pure (HalfEdge u (e^.origin) h) <> pure (HalfEdge (e^.origin) u h)

-- we may want to make sure that the f we use here is a VectorBuilder,
-- so that it is cheap to produce the output f.

-- | collect all edges by the plane defining them.
edgesByFace :: ( Foldable f, Ord r)
            => f (HalfEdge r) -> [Boxed.Vector (HalfEdge r)]
edgesByFace = groupOn (^.leftPlane) . sortBy (comparing (^.leftPlane))

-- | Group a bunch of consecutive elements b
groupOn   :: Ord b => (a -> b) -> Boxed.Vector a -> [Boxed.Vector a]
groupOn f = Boxed.groupBy ((==) `on` f)
