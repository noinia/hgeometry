{-# LANGUAGE UndecidableInstances #-}
-- | Triangles in \(d\)-dimensional space.
module Geometry.Triangle.Boxed
  ( Triangle(Triangle)

  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Lens
import           Data.Bifoldable (Bifoldable (bifoldMap))
import           Data.Bifunctor (Bifunctor (first))
import           Data.Bitraversable
import           Data.Either (partitionEithers)
import           Data.Ext
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Semigroup.Foldable
import           Data.Util (Three, pattern Three)
import           Data.Vinyl (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec (Handler (H), match)
import           GHC.Generics (Generic)
import           GHC.TypeLits (type (+))
import           Geometry.Ball (Disk, disk)
import           Geometry.Boundary (PointLocationResult (..))
import           Geometry.Box (IsBoxable (..))
import           Geometry.HalfSpace
import           Geometry.HyperPlane
import           Geometry.Line (Line (Line))
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Properties
import           Geometry.Transformation
import           Geometry.Vector
import qualified Geometry.Vector as V
import           Test.QuickCheck (Arbitrary(..), suchThat)

--------------------------------------------------------------------------------

data Triangle d point r = Triangle !(point d r)
                                   !(point d r)
                                   !(point d r)
                          deriving (Show,Read,Eq,Functor,Foldable,Traversable,Generic)

type instance NumType   (Triangle d p r) = r
type instance Dimension (Triangle d p r) = d

instance (NFData (point d r)) => NFData (Triangle d point r)

instance Field1 (Triangle d point r) (Triangle d point r) (point d r) (point d r) where
  _1 = lens (\(Triangle p _ _) -> p) (\(Triangle _ q r) p -> Triangle p q r)
instance Field2 (Triangle d point r) (Triangle d point r) (point d r) (point d r) where
  _2 = lens (\(Triangle _ q _) -> q) (\(Triangle p _ r) q -> Triangle p q r)
instance Field3 (Triangle d point r) (Triangle d point r) (point d r) (point d r) where
  _3 = lens (\(Triangle _ _ r) -> r) (\(Triangle p q _) r -> Triangle p q r)


instance HasVertices' (Triangle d point r) where
  type Vertex   (Triangle d point r) = point d r
  type VertexIx (Triangle d point r) = Int

instance HasVertices (Triangle d point r) (Triangle d point' s) where
  vertices = conjoined traverseTri (itraverseTri . indexed)
    where
      traverseTri f  (Triangle a b c) = Triangle <$> f            a <*> f   b <*> f   c
      itraverseTri f (Triangle a b c) = Triangle <$> f (1 :: Int) a <*> f 2 b <*> f 3 c


instance HasPoints (Triangle d point r) (Triangle d point' s) point point' where
  allPoints = vertices

instance ( Arbitrary r, Num r, Ord r
         , Eq (point 2 r), Point_ point 2 r
         , Arbitrary (point 2 r)) => Arbitrary (Triangle 2 point r) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary `suchThat` (/= a)
                 c <- arbitrary `suchThat` (\c' -> c' /= a && c' /= b && ccw a b c' /= CoLinear)
                 pure $ Triangle a b c

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (Triangle d point r)
