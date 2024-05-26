{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Type
  ( LowerEnvelope'(LowerEnvelope)
  , theUnboundedVertex, boundedVertices
  , traverseLowerEnvelope

  , singleton

  , BoundedVertex
  , BoundedVertexF(Vertex)
  , location, definers, location2

  , UnboundedVertex(UnboundedVertex)
  , unboundedVertexId
  , HasUnboundedEdges(..)

  , EdgeGeometry
  , projectedEdgeGeometries, projectedEdgeGeometry


  , outgoingUnboundedEdge
  , edgeIntersectionLine
  , intersectionLine'
  , intersectionLineWithDefiners
  , EdgeDefiners(..)
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>))
import qualified Data.Functor.Apply as Apply
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Traversable(traverse1Maybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import           HGeometry.Box
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Plane.LowerEnvelope.Type
import           HGeometry.Plane.LowerEnvelope.VertexForm (IntersectionLine(..),intersectionLine)
import qualified HGeometry.Plane.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph
import           Witherable


--------------------------------------------------------------------------------
-- * Data type defining a connected lower envelope

-- | A connected lower envelope in adjacencylist form.
--
-- invariant: there is always at least one bounded vertex.
data LowerEnvelope' plane =
  LowerEnvelope !(UnboundedVertex plane) (Seq.Seq (BoundedVertex plane))

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope' plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope' plane)

type instance NumType   (LowerEnvelope' plane) = NumType plane
type instance Dimension (LowerEnvelope' plane) = 3

instance (Ord (NumType plane), Num (NumType plane)) => IsBoxable (LowerEnvelope' plane) where
  -- ^ the bounding box contains all bounded vertices
  boundingBox env = boundingBox . NonEmpty.fromList $ env^..boundedVertices.traverse.location
    -- the fromList is safe since there is alwasy at least one vertex

-- | Traversal of the planes in the lower envelope. Since we need an Ord constraint we
-- can't make LowerEnvelope' an instance of Traversable.
--
-- Be aware that this may destroy some of the invariants. So use this function with care.
traverseLowerEnvelope                         :: ( Applicative f, NumType plane ~ NumType plane'
                                                 , Ord plane'
                                                 )
                                              => (plane -> f plane')
                                              -> LowerEnvelope' plane
                                              -> f (LowerEnvelope' plane')
traverseLowerEnvelope f (LowerEnvelope v0 vs) =
    LowerEnvelope <$> traverse f v0 <*> traverse (traverseBoundedV f) vs

-- | lens to access the unbounded vertex
theUnboundedVertex :: Lens' (LowerEnvelope' plane) (UnboundedVertex plane)
theUnboundedVertex = lens (\(LowerEnvelope v _) -> v)
                          (\(LowerEnvelope _ vs) v -> LowerEnvelope v vs)

-- | Lens to access the sequence of bounded vertices.
boundedVertices :: Lens' (LowerEnvelope' plane) (Seq.Seq (BoundedVertex plane))
boundedVertices = lens (\(LowerEnvelope _ vs)    -> vs)
                       (\(LowerEnvelope u _ ) vs -> LowerEnvelope u vs)

--------------------------------------------------------------------------------

-- | Given a single Vertex, construct a LowerEnvelope' out of it.
singleton   :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
            => VertexForm.LEVertex plane -> LowerEnvelope' plane
singleton v = LowerEnvelope v0 (Seq.singleton v')
  where
    i  = 1 -- the vertexID we are using for this vertex.
    v' = fromLEVertex v
    v0 = UnboundedVertex $ flipEdge i <$> view incidentEdgesB v'
    -- do we need to reverse the sequenceo f edges?

--------------------------------------------------------------------------------

-- | The unbounded vertex.
newtype UnboundedVertex plane = UnboundedVertex { _incidentEdgesU :: Seq.Seq (LEEdge plane) }
                              deriving (Show,Eq,Functor,Foldable,Traversable)

-- | access the incidentEdges of an unbounded vertex
incidentEdgesU :: Iso (UnboundedVertex plane) (UnboundedVertex plane')
                      (Seq.Seq (LEEdge plane)) (Seq.Seq (LEEdge plane'))
incidentEdgesU = coerced

-- | The vertexID of the unbounded vertex
unboundedVertexId :: VertexID
unboundedVertexId = 0

--------------------------------------------------------------------------------

-- | Vertices in of the lower envelope in adjacencylist form.
type BoundedVertex = BoundedVertexF Seq.Seq

-- | Given a vertex in vertex form, construct A BoundedVertex from
-- it. Note that this essentially assumes the vertex is connected to
-- the unbounded vertex with all its outgoing edgse.
--
-- \(O(k \log k)\), where \(k\) is the number of definers.
fromLEVertex                              :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                                          => VertexForm.LEVertex plane
                                          -> BoundedVertex plane
fromLEVertex (VertexForm.LEVertex v defs) = Vertex v defs es
  where
    es  = (\(_ :+ EdgeDefiners hl hr) -> Edge unboundedVertexId hl hr) <$> sortAroundStart es'
    es' = mapMaybe (\t@(Two h1 h2) -> let defs' = toNonEmpty' $ Set.delete h1 $ Set.delete h2 defs
                                      in outgoingUnboundedEdge v t defs'
                   ) $ uniquePairs defs
    toNonEmpty' s = case NonEmpty.nonEmpty $ F.toList s of
                      Just xs -> xs
                      _       -> error "fromLEVertex: absurd, there should be at least 3 definers"


-- | Given a bunch of halflines that all share their starting point v,
-- sort them cyclically around the starting point v.
sortAroundStart :: (Foldable f, Ord r, Num r)
                => f (HalfLine (Point 2 r) :+ e) -> Seq.Seq (HalfLine (Point 2 r) :+ e)
sortAroundStart = fromFoldable . sortBy @V.Vector compareAroundStart
  where
    compareAroundStart (HalfLine v d  :+ _)
                       (HalfLine _ d' :+ _) = ccwCmpAround v (v .+^ d) (v .+^ d')



-- | Given a location of a vertex v, a pair of planes h1,h2 and the
-- remaining defining planes of v, computes the outgoing half-line
-- from v on which h1,h2 are the lowest (if such an halfline exists).
outgoingUnboundedEdge                   :: ( Plane_ plane r, Ord r, Fractional r
                                           , Foldable1 f
                                           )
                                        => Point 3 r -- ^ the location of the vertex v
                                        -> Two plane -- ^ the pair of planes for which to compute
                                        -- the halfine
                                        -> f plane -- ^ the other planes intersecting at v
                                        -> Maybe (HalfLine (Point 2 r) :+ EdgeDefiners plane)
outgoingUnboundedEdge v (Two h1 h2) h3s =
  intersectionLineWithDefiners h1 h2 >>= toHalfLineFrom (projectPoint v) h3s
  -- todo, if there are more planes, I guess we should check if the hl is not dominated by the other
  -- planes either.

-- | Given :
--
-- v : the projected location of the vertex
-- hs : the remaining planes defining v (typically just one plane h3)
-- l   : the (projection of the) line l in which planes h1 and h2 intersect (containing v)
--
-- we compute the half-line eminating from v in which h1 and h2 define
-- an edge incident to v.
toHalfLineFrom                  :: (Plane_ plane r, Foldable1 f, Fractional r, Ord r)
                                => Point 2 r -- ^ vertex v
                                -> f plane     -- ^ the remaining plane(s) hs
                                -> LinePV 2 r :+ EdgeDefiners plane -- ^ the line l
                                -> Maybe (HalfLine (Point 2 r) :+ EdgeDefiners plane)
toHalfLineFrom v hs ((LinePV _ w) :+ defs@(EdgeDefiners h1 h2)) =
    validate w defs <|> validate ((-1) *^ w) (EdgeDefiners h2 h1)
    -- We try both directions. Note that if we reverse the direction
    -- of the line, what the left/right plane is changes.
    --
    -- If neither direction works, then h1,h2 do not define a good
    -- direction. This should happen only when there are more than 3
    -- planes intersecting in v, i.e. in degernate sitatuions
  where
    -- | test if direction d is a good direction, i.e. if towards direction d
    -- h1 (and thus also h2) is actually lower than all remaining defining planes.
    validate d defs' = let zVal = evalAt (v .+^ d)
                       in if all (\h3 -> zVal h1 < zVal h3) hs
                          then Just (HalfLine v d :+ defs') else Nothing




-- | The planes left (above) and right (below) their intersection
-- line.
data EdgeDefiners plane = EdgeDefiners { _leftPlane  :: plane -- ^ above plane
                                       , _rightPlane :: plane -- ^ below plane
                                       }

-- | Computes the line in which the two planes intersect, and labels
-- the halfplanes with the lowest plane.
--
-- The returned line will have h to its left and h' to its right.
intersectionLineWithDefiners      :: ( Plane_ plane r, Ord r, Fractional r)
                                  => plane -> plane -> Maybe (LinePV 2 r :+ EdgeDefiners plane)
intersectionLineWithDefiners h h' = (:+ EdgeDefiners h h') <$> intersectionLine' h h'


-- | Computes the line in which the two planes intersect. The returned line will have h to
-- its left and h' to its right.
--
--
intersectionLine'      :: ( Plane_ plane r, Ord r, Fractional r)
                       => plane -> plane -> Maybe (LinePV 2 r)
intersectionLine' h h' = intersectionLine h h' <&> \case
    Vertical x    -> reorient (LinePV (Point2 x 0) (Vector2 0 1)) (Point2 (x-1) 0)
    NonVertical l -> let l'@(LinePV p _) = fromLineEQ l
                     in reorient l' (p&yCoord %~ (+1))
  where
    -- make sure h is to the left of the line
    reorient l q = let f = evalAt q
                   in if f h <= f h' then l else l&direction %~ negated
    fromLineEQ (LineEQ a b) = fromLinearFunction a b

-- | Computes the supportingline of the (downward projection of) the edge.
edgeIntersectionLine   :: ( Plane_ plane r, Ord r, Fractional r)
                       => LEEdge plane -> LinePV 2 r
edgeIntersectionLine e = case intersectionLine' (e^.leftPlane) (e^.rightPlane) of
    Just l  -> l
    Nothing -> error "edgeIntersectionLine: absurd. no intersection line !?"



--------------------------------------------------------------------------------

-- | Types that have an IncidentEdges' field.
class HasIncidentEdges t  where
  -- | Lens to access the incident edges field.
  incidentEdges' :: Lens' (t plane) (Seq.Seq (LEEdge plane))

instance HasIncidentEdges UnboundedVertex where
  incidentEdges' = incidentEdgesU

instance HasIncidentEdges BoundedVertex where
  incidentEdges' = lens (view incidentEdgesB) (\(Vertex p d _) es -> Vertex p d es)

-- instance HasIncidentEdges (Vertex (LowerEnvelope' plane)) plane where
--   incidentEdges' = undefined -- pick either incidentEdges on the left or on the right thing.


--------------------------------------------------------------------------------
-- * The Instances for HasVertices, HasDarts, HasFaces etc

instance HasVertices' (LowerEnvelope' plane) where
  type Vertex   (LowerEnvelope' plane) = Either (UnboundedVertex plane) (BoundedVertex plane)
  type VertexIx (LowerEnvelope' plane) = VertexID

  -- | note, trying to assign the unbounded vertex to something with index /=
  -- unboundedVertexId is an error.
  vertexAt        :: VertexID
                  -> IndexedTraversal' VertexID (LowerEnvelope' plane)
                                                (Vertex (LowerEnvelope' plane))
  vertexAt i
      | i == unboundedVertexId = conjoined trav0  (itrav0 .indexed)
      | otherwise              = conjoined travVs (itravVs.indexed)
    where
      trav0  f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f   (Left v0)

      itrav0                          :: Applicative f
                                      => (VertexID -> Vertex (LowerEnvelope' plane)
                                         -> f (Vertex (LowerEnvelope' plane)))
                                      -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      itrav0 f (LowerEnvelope v0 vs)  = flip LowerEnvelope vs . unLeft <$> f 0 (Left v0)

      travVs                          :: Applicative f
                                      => (Vertex   (LowerEnvelope' plane)
                                         -> f (Vertex   (LowerEnvelope' plane)))
                                      -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      travVs f (LowerEnvelope v0 vs)  =
        LowerEnvelope v0 <$> (vs&ix (i-1) %%~ fmap unRight . f . Right)
      itravVs f (LowerEnvelope v0 vs) =
        LowerEnvelope v0 <$> (vs&ix (i-1) %%~ fmap unRight . f i . Right)

      unLeft  = either id (error "LowerEnvelope'.vertexAt: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope'.vertexAt: trying to convert a regular bounded vertex into v_infty is not allowed") id
  {-# INLINE vertexAt #-}


instance HasVertices (LowerEnvelope' plane) (LowerEnvelope' plane') where

  vertices = conjoined traverse' (itraverse' . indexed)
    where
      traverse' :: (Apply f)
                => (Vertex (LowerEnvelope' plane) -> f (Vertex (LowerEnvelope' plane')))
                -> LowerEnvelope' plane -> f (LowerEnvelope' plane')
      traverse'  f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f .Left) v0
                      <.*> traverse1Maybe (fmap unRight . f . Right) vs

      unLeft  = either id (error "LowerEnvelope'.vertices: trying to convert v_infty into a normal vertex is not allowed")
      unRight = either (error "LowerEnvelope'.vertices: trying to convert a regular bounded vertex into v_infty is not allowed") id

      itraverse' :: (Apply f)
                 => (VertexID -> Vertex (LowerEnvelope' plane) -> f (Vertex (LowerEnvelope' plane')))
                -> LowerEnvelope' plane -> f (LowerEnvelope' plane')
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (fmap unLeft . f unboundedVertexId .Left) v0
                      <.*> itraverse1Maybe (\i -> fmap unRight . f (i+1) . Right) vs
  {-# INLINE vertices #-}

-- | Indexed version of 'traverse1Maybe'.
itraverse1Maybe   :: (TraversableWithIndex i t, Apply f)
                  => (i -> a -> f b) -> t a -> Apply.MaybeApply f (t b)
itraverse1Maybe f = itraverse (\i -> Apply.MaybeApply . Left . f i)

----------------------------------------

instance HasDarts' (LowerEnvelope' plane) where
  type Dart   (LowerEnvelope' plane) = LEEdge plane
  type DartIx (LowerEnvelope' plane) = ( VertexIx (LowerEnvelope' plane)
                                       , VertexIx (LowerEnvelope' plane)
                                       )
  dartAt (u,v) = vertexAt u <.> beside l l
    where
      l :: HasIncidentEdges t => IndexedTraversal' VertexID (t plane) (LEEdge plane)
      l = incidentEdges' .> first' v (view destination)
  {-# INLINE dartAt #-}

-- | Helper function to access the an edge
first'              :: Eq i => i -> (a -> i)
                    -> IndexedTraversal' i (Seq.Seq a) a
first' i getIdx f s = case Seq.findIndexL ((== i) . getIdx) s of
                        Nothing -> pure s
                        Just j  -> s&ix j %%~ indexed f i


instance HasDarts (LowerEnvelope' plane) (LowerEnvelope' plane) where
  darts = itraverse' . indexed
    -- the traverse' variant that does not use the indices does not really look all that
    -- useful. So I didn't bother writing the faster version.
    where
      itraverse' :: (Applicative f)
                 => (DartIx (LowerEnvelope' plane) -> LEEdge plane -> f (LEEdge plane))
                 -> LowerEnvelope' plane -> f (LowerEnvelope' plane)
      itraverse' f (LowerEnvelope v0 vs) =
        LowerEnvelope <$> (v0&incidentEdgesU.traverse %%~ liftF f unboundedVertexId)
                      <*> itraverse (\i vtx -> vtx&incidentEdgesB.traverse %%~ liftF f (i+1)) vs

      liftF       :: (DartIx (LowerEnvelope' plane) -> LEEdge plane -> f (LEEdge plane))
                  -> VertexIx (LowerEnvelope' plane)
                  -> LEEdge plane -> f (LEEdge plane)
      liftF f u e = f (u,e^.destination) e
  {-# INLINE darts #-}

--------------------------------------------------------------------------------


instance HasEdges' (LowerEnvelope' plane) where
  type Edge   (LowerEnvelope' plane) = LEEdge plane
  type EdgeIx (LowerEnvelope' plane) = ( VertexIx (LowerEnvelope' plane)
                                       , VertexIx (LowerEnvelope' plane)
                                       )
  edgeAt = dartAt
  {-# INLINE edgeAt #-}

instance HasEdges (LowerEnvelope' plane) (LowerEnvelope' plane) where
  -- | Traversal of all edges in the graph; i.e. we traverse the edges only in the
  -- direction from lower vertex id to higher vertexId.
  edges = darts . ifiltered (\(u,v) _ -> u < v)
  {-# INLINE edges #-}

-- instance Graph_ (LowerEnvelope' plane) where

class HasUnboundedEdges t e | t -> e where
  -- | Lens to access the unbounded edges.
  unboundedEdges :: Lens' t (Seq.Seq e)

instance HasUnboundedEdges (LowerEnvelope' plane) (LEEdge plane) where
  unboundedEdges = theUnboundedVertex.incidentEdgesU


-- | Edges are either halflines or Linesegments
type EdgeGeometry point = Either (HalfLine point) (ClosedLineSegment point)

-- | Get the projected geometries (halflines and edges line segments ) representing the
-- edges.
projectedEdgeGeometries :: (Plane_ plane r, Ord r, Fractional r
                                       , Show plane, Show r

                           )
                        => IndexedFold (EdgeIx (LowerEnvelope' plane))
                                       (LowerEnvelope' plane) (EdgeGeometry (Point 2 r))
projectedEdgeGeometries = ifolding $ \env ->
  (\(i,e) -> (i, projectedEdgeGeometry env i e)) <$> env^..edges.withIndex
  -- TODO; this is kind of horrible, there must be a nicer way of writing this.

-- | Computes the (projected) LineSegment or HalfLine representing a given edge.
projectedEdgeGeometry               :: (Plane_ plane r, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                                    => LowerEnvelope' plane
                                    -> EdgeIx (LowerEnvelope' plane)
                                    -> Edge (LowerEnvelope' plane)
                                    -> EdgeGeometry (Point 2 r)
projectedEdgeGeometry env (ui,vi) e = case env^?!vertexAt ui of
    Left _unboundedVtx -> case env^?!vertexAt vi of
      Left _  -> error "projectedEdgeGeometry: absurd edge from vInfty to vInfty"
      Right v -> unboundedEdge v
    Right u           -> case env^?!vertexAt vi of
      Left _unboundedVtx -> unboundedEdge u
      Right v            -> Right $ ClosedLineSegment (u^.location2) (v^.location2)
  where
    unboundedEdge v = let dir = edgeIntersectionLine e ^.direction.to negated
                      in Left $ HalfLine (v^.location2) dir
    -- edge e is oriented from the lowerId towards the higherId, so in particular *from*
    -- the unbounded vertex into the bounded vertex that means that to construct the
    -- halfline we actually wish to flip the orientation of the line

--------------------------------------------------------------------------------
