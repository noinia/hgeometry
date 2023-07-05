{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Type
  ( Plane
  , Vertex(Vertex), location, definers
  , asVertex
  , HalfEdge(HalfEdge)
  , HasOrigin(..), HasLeftPlane(..), destination
  , UnboundedHalfEdge(UnboundedHalfEdge)
  , asHalfEdge, aroundOrigins
  , LowerEnvelope(LowerEnvelope), halfEdges

  , intersectionPoint

  , (^^^)
  ) where

import           Control.Lens
import           Data.Functor.Classes
import           Data.Kind (Type)
import           Data.Ord (comparing)
import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import qualified HGeometry.LowerEnvelope.AtMostThree as AtMostThree
import           HGeometry.Point hiding (origin)
import           HGeometry.Properties
import           HGeometry.Vector
import qualified Hiraffe.Graph as Graph

--------------------------------------------------------------------------------

-- | The Lower envelope whose vertices are stored in a structure of
-- type f, and whose half-edges are stored in a structure of type g.
--
--
type LowerEnvelope :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data LowerEnvelope f g r =
  LowerEnvelope { _vertices           :: f (Vertex r)
                , _halfEdges          :: g (HalfEdge r)
                  -- ^ sorted in CCW order around their origins
                }
-- TODO: this is still just an intermediate type; in the end we will need to convert to a proper
-- planar subdivision anyway


verticesLens :: Lens (LowerEnvelope f g r) (LowerEnvelope f' g r) (f (Vertex r)) (f' (Vertex r))
verticesLens = lens _vertices (\env vs -> env { _vertices = vs })

halfEdges :: Lens (LowerEnvelope f g r) (LowerEnvelope f g' r) (g (HalfEdge r)) (g' (HalfEdge r))
halfEdges = lens _halfEdges (\env es -> env { _halfEdges = es })

deriving instance (Show r, Show1 f, Show1 g) => Show (LowerEnvelope f g r)
deriving instance (Read r, Read1 f, Read1 g) => Read (LowerEnvelope f g r)
deriving instance (Ord r, Eq1 f, Eq1 g)      => Eq   (LowerEnvelope f g r)


instance ( TraversableWithIndex i f, Ixed (f (Vertex r))
         , i ~ Index (f  (Vertex r)), IxValue (f (Vertex r)) ~ Vertex r
         ) => Graph.HasVertices' (LowerEnvelope f g r) where
  type VertexIx (LowerEnvelope f g r) = Index (f (Vertex r))
  type Vertex   (LowerEnvelope f g r) = Vertex r
  vertexAt u = verticesLens .> iix u

instance ( TraversableWithIndex i f, Ixed (f (Vertex r))
         , i ~ Index (f  (Vertex r)), IxValue (f (Vertex r)) ~ Vertex r
         ) => Graph.HasVertices (LowerEnvelope f g r) (LowerEnvelope f g r) where
  vertices = verticesLens .> itraversed


-- todo; report faces somehow I guess that before re-sorting the
-- half-edges after triangulation we know have the edges per face.

--------------------------------------------------------------------------------

-- | A vertex in the lower envelope
data Vertex r = Vertex { _location :: Point 3 r
                       , _definers :: Vector 3 (Plane r)
                       } deriving (Show,Read)

instance Ord r => Eq (Vertex r) where
  u == v = u `compare` v == EQ
instance Ord r => Ord (Vertex r) where
  (Vertex p defs) `compare` (Vertex q defs') = p `compare` q
                                            <> sort @Boxed.Vector defs `compare` sort defs'


-- | The location of the vertex
location :: Lens' (Vertex r) (Point 3 r)
location = lens _location (\v l -> v { _location = l })

-- | Projected 2d location of the point
location2 :: Getter (Vertex r) (Point 2 r)
location2 = location . to projectPoint

-- | The three planes defining the vertex
definers :: Lens' (Vertex r) (Vector 3 (Plane r))
definers = lens _definers (\v ds -> v { _definers = ds })

-- | Construct the vertex representing the intersection of the three
-- given planes (if such an intersection point exists).
asVertex                  :: (Fractional r, Eq r)
                          => Three (Plane r) -> Maybe (Vertex r)
asVertex (Three h1 h2 h3) = (\l -> Vertex l (Vector3 h1 h2 h3)) <$> intersectionPoint h1 h2 h3

--------------------------------------------------------------------------------

-- | A Half-edge in the envelope
data HalfEdge r = HalfEdge { _origin      :: !(Vertex r)
                           , _destination :: !(Vertex r)
                           , _leftPlane   :: !(Plane r)
                           } deriving (Show,Read,Eq)

type instance NumType (HalfEdge r) = r

-- | Unbounded half edges
data UnboundedHalfEdge r = UnboundedHalfEdge { _uOrigin    :: !(Vertex r)
                                             , _uLeftPlane :: !(Plane r)
                                             } deriving (Show,Read,Eq)

type instance NumType (UnboundedHalfEdge r) = r

-- instance Ord r => Ord (HalfEdge r) where
--   compare = aroundOrigins

class HasOrigin t where
  origin :: Lens' t (Vertex (NumType t))

class HasLeftPlane t where
  leftPlane :: Lens' t (Plane (NumType t))

destination :: Lens' (HalfEdge r) (Vertex r)
destination = lens _destination (\he d -> he { _destination = d })


instance HasOrigin (HalfEdge r) where
  origin = lens _origin (\he o -> he { _origin = o })
instance HasOrigin (UnboundedHalfEdge r) where
  origin = lens _uOrigin (\he o -> he { _uOrigin = o })

instance HasLeftPlane (HalfEdge r) where
  leftPlane = lens _leftPlane (\he lp -> he { _leftPlane = lp })
instance HasLeftPlane (UnboundedHalfEdge r) where
  leftPlane = lens _uLeftPlane (\he lp -> he { _uLeftPlane = lp })



--------------------------------------------------------------------------------

-- | Given two vertices, try to construct a half-edge fr<om them; this
-- requires that two of the three defining planes are shared.
asHalfEdge           :: (Ord r, Num r) => Two (Vertex r) -> Maybe (HalfEdge r)
asHalfEdge (Two u v) = case AtMostThree.commonElems (u^.definers) (v^.definers) of
                         AtMostThree.Two h1 h2 -> Just $ HalfEdge u v (leftHalfPlane h1 h2)
                         _                     -> Nothing
                         -- | there can be at most two planes in common; if
                         -- there are three u and v are the same vertex.
  where
    leftHalfPlane h1 h2 | isLeftHalfPlane h1 = h1
                        | otherwise          = h2

    isLeftHalfPlane = undefined -- take a vector into the left halfplane with say origin u, take any point in this halfplane, test if h1 is lower than h2 if so return True.

-- | An ordering that lexicographically orders the half-edges first
-- based on their origin location, and then ccw around that origin
-- location.
aroundOrigins       :: (Ord r, Num r) => HalfEdge r -> HalfEdge r -> Ordering
aroundOrigins e1 e2 = comparing    (^.origin.location) e1 e2
                   <> ccwCmpAround (e1^.origin.location2)
                                   (e1^.destination.location2) (e2^.destination.location2)
                   <> comparing (^.leftPlane) e1 e2

--------------------------------------------------------------------------------

-- | Compute the common intersection point of three non-vertical
-- hyperplanes in R^3 (if such a point exists).
intersectionPoint :: (Fractional r, Eq r) => Plane r -> Plane r -> Plane r -> Maybe (Point 3 r)
intersectionPoint h1@(Plane a1 b1 c1)
                     (Plane a2 b2 c2)
                     (Plane a3 b3 c3)
  | a1 == a2  = Nothing -- FIXME: this can't be right
  | otherwise = do y <- my
                   x <- xf y
                   pure $ Point3 x y (z x y)
  where
    xf y' = (b2-b1)*y' + c2 - c1
            ///
            a1 - a2

    my = c3 - c1 - (a1-a3)*(c2-c1)/(a1-a2)
         ///
         (a1-a3)*(b2-b1)/(a1-a2) + b1 - b3

    z x' y' = evalAt (Point2 x' y') h1

infixl 0 ///
-- | safe division testing for zero
(///)         :: (Eq a, Fractional a) => a -> a -> Maybe a
num /// denom = if denom /= 0 then Just (num / denom) else Nothing

--------------------------------------------------------------------------------


-- | Raise to some fractional power
(^^^)   :: Int -> Rational -> Int
n ^^^ p = floor $ fromIntegral n ** realToFrac @Rational @Double p
{-# INLINE (^^^) #-}
