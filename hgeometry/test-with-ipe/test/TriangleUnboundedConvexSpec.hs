module TriangleUnboundedConvexSpec where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Triangle
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type UnboundedConvexRegion point = UnboundedConvexRegionF (NumType point) NonEmpty point

-- | An unbounded ConvexRegion
data UnboundedConvexRegionF r nonEmpty point =
  Unbounded (Vector 2 r)
            -- ^ vector indicating the direction of the unbounded edge
            -- incident to the first vertex. Note that this vector
            -- thus points INTO vertex v.
            (nonEmpty point)
            -- ^ the vertices in CCW order,
            (Vector 2 r)
            -- ^ the vector indicating the direction of the unbounded
            -- edge incident to the last vertex. The vector points
            -- away from the vertex (i.e. towards +infty).
  deriving stock (Show,Eq,Functor,Foldable,Traversable)


--------------------------------------------------------------------------------

type R = RealNumber R

spec :: Spec
spec = describe "triangle x unbounde convex polygon intersection" $ do
         pure ()
