module Polygon.Convex.UnboundedSpec
  ( spec
  , allOriginal
  ) where

import           HGeometry.Intersection
import           Control.Lens
import           Control.Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Boundary
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Instances ()
import           HGeometry.Kernel.Test.Box
import           HGeometry.Kernel
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Polygon.Convex.Instances ()
import           HGeometry.Polygon.Instances ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           HGeometry.Polygon.Simple.PossiblyDegenerate

--------------------------------------------------------------------------------


-- TODO: move these tests to some module about Unbounded
spec :: Spec
spec = describe "Polygon.Convex.Unbounded" $
         specUnbounded

specUnbounded :: Spec
specUnbounded = describe "fromUnbounded correct" $ do
         prop "boundedFromVertices that intersect triangle are orig vertices " $
           verifyBoundedFromVertices

         prop "bug, boundedFromVertices that intersect triangle are orig vertices " $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               reg = Unbounded (Vector2 (-1) (-0.5))
                               (NonEmpty.singleton (Point2 0 0.5))
                               (Vector2 1 (-1.5))
           verifyBoundedFromVertices tri reg

         prop "bug, intersection" $ do
           let tri = Triangle (Point2 2 2) (Point2 1 (-1)) (Point2 2 0)
               reg = Unbounded (Vector2 (-1) (-0.5))
                               (NonEmpty.singleton (Point2 0 0.5))
                               (Vector2 1 (-1.5))
           allOriginal @(UnboundedConvexRegion (Point 2 R)) reg tri

         prop "original vertices are really original vertices (bounded)" $
           allOriginal @(ConvexPolygon (Point 2 R))

         prop "original vertices are really original vertices (unbounded)" $
           allOriginal @(UnboundedConvexRegion (Point 2 R))

-- | Check if all original vertices of a triangle and a polygon are really original
allOriginal          :: ( IsIntersectableWith (Triangle (Point 2 R)) poly
                          , Intersection (Triangle (Point 2 R)) poly ~
                             Maybe (PossiblyDegenerateSimplePolygon
                               (OriginalOrExtra (Point 2 R) (Point 2 R))
                               (poly' (OriginalOrExtra (Point 2 R) (Point 2 R))))
                        , Foldable poly'
                        , Show (poly' (OriginalOrExtra (Point 2 R) (Point 2 R)))
                        , HasVertices poly poly
                        , Vertex poly ~ Point 2 R
                        ) => poly -> Triangle (Point 2 R) -> Every
allOriginal poly tri = case tri `intersect` poly of
    Nothing  -> discard
    Just res -> bifoldMap f (foldMap f) res
      where
        f :: OriginalOrExtra (Point 2 R) (Point 2 R) -> Every
        f = Every . \case
          Extra    _ -> property True
          Original v -> counterexample (show res) . counterexample (show v) . property
                      $ v `elem` origVertices

        origVertices = tri^..vertices <> poly^..vertices


-- | Make sure that the vertices of the bounded convex region we create that
-- intersect the triagnle are actually original vertices of the convex region as well.
verifyBoundedFromVertices         :: Triangle (Point 2 R) -> UnboundedConvexRegion (Point 2 R)
                                  -> Every
verifyBoundedFromVertices tri reg = foldMapOf vertices (Every . check) $ toBoundedFrom tri reg
  where
    check v | v `intersects` tri = counterexample (show v) -- if v lies inside it should be
                                 $ isOrigVertex v
             | otherwise         = property True
    isOrigVertex v = elemOf vertices v reg
