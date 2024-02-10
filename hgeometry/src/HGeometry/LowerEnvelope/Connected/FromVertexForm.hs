{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LowerEnvelope.FromVertexForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.LowerEnvelope.Connected.FromVertexForm
  ( fromVertexForm'
  ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LowerEnvelope.Connected.Type
import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.VertexForm (IntersectionLine(..),intersectionLine)
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import qualified HGeometry.Vector as Vec
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph
import           Witherable

import           Debug.Trace

--------------------------------------------------------------------------------

-- | Given a Lower envelope in vertex form, construct the AdjacencyList representation out
-- of it.
--
-- pre: The set of vertices is non-empty
--
-- \(O(n\log n)\)
fromVertexForm'      :: forall plane r. (Plane_ plane r, Ord plane, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                     => VertexForm.VertexForm plane -> LowerEnvelope' plane
fromVertexForm' lEnv = LowerEnvelope v0 boundedVs
  where
    v0 = UnboundedVertex unboundedEdges'

    -- the bounded vertices.
    boundedVs = Seq.zipWith ( \(_,v,_) outs -> v&incidentEdgesB .~ toSeq outs
                            ) boundedVs' boundedEsByOrigin

    toSeq = foldMap (Seq.singleton . snd)

    -- all edges leaving from bounded vertices, ordered by their VertexIds
    boundedEsByOrigin = Seq.fromList . groupOnCheap fst . foldMap F.toList
                      $ allEdges

    -- computes all outgoing edges of all bounded vertices, they are grouped by face
    allEdges :: [FaceEdges plane]
    allEdges = traceShowWith ("allEdges",) .
      fmap (faceToEdges . sortAlongBoundary)
             . groupOnCheap definingPlane
             $ foldMap (^._3) boundedVs'

    -- | construct the unbounded edges, by simply filtering and flipping the
    -- edges towards infinity.
    unboundedEdges' :: Seq.Seq (LEEdge plane)
    unboundedEdges' = mapMaybe (\(u, e) -> if e^.destination == unboundedVertexId
                                           then Just (flipEdge u e) else Nothing
                               -- flip every unbounded edge
                               )
                    $ foldMap fromFoldable allEdges
    -- FIXME: we should probably sort those right?


    -- | A sequence of bounded vertices, ordered by vertexID, together with a bunch of
    -- copies; one for each face it will appear on
    --
    -- TODO: i guess that in degenerate sitations it could be that there are copies that
    -- wont actually proudce an edge (i.e. if there is some definer/plane containing the
    -- vertex, that does not appear on the lower envelope).
    --
    boundedVs' :: Seq.Seq ( VertexID, BoundedVertex plane, [IntermediateVertex plane])
    boundedVs' = ifoldMapOf (indexing (vertices.withIndex)) mkVtx lEnv

    mkVtx i (v,defs) = let j = i+1
                       in Seq.singleton ( j
                                        , Vertex v defs mempty
                                        , [ Vtx h j v defs | h <- F.toList defs ]
                                        -- TODO: conceivably, we could delete the h from the
                                        -- defs here already? instead of in the various delete h
                                        -- cases
                                        )

-- | For each bounded vertex (represented by their ID) the outgoing halfedge.
type FaceEdges plane = NonEmpty (VertexID, LEEdge plane)


-- | Helper type for edgedefs
data EdgeDefs plane = EdgeDefs { common :: plane
                               , uNeigh :: plane
                               , vNeigh :: plane
                               } deriving (Show,Eq)

-- | Given a plane h, and vertices u (with its definers), and v (with its definers) that
-- define an edge of h, computes:
--
-- - plane h' that is on the other side of the edge from u to v,
-- - the plane hu incident only to u that is adjacent to h, and
-- - the plane hv incident only to v that is adjacent to h.
extractEdgeDefs                   :: (Ord plane
                                     , Show plane, Show r
                                     )
                                  => plane
                                  -> Point 3 r -> VertexForm.Definers plane
                                  -> Point 3 r -> VertexForm.Definers plane
                                  -> Maybe (EdgeDefs plane)
extractEdgeDefs h u uDefs v vDefs
  -- | traceShow ("extractEdgeDefs",h,u,uDefs,v,vDefs) False = undefined
  -- | otherwise
  = case traceShowWith ("commons",) commons of
    []   -> Nothing
    [h'] -> Just $ EdgeDefs h' hu hv
    _    -> error "extractEdgeDefs: unhandled degeneracy. u and v have >2 planes in common."
  where
    commons = F.toList $ Set.delete h --- $ traceShowWith ("udefs intersect vDefs",)
              (uDefs `Set.intersection` vDefs)
    uOnlies = F.toList $ uDefs Set.\\ vDefs
    vOnlies = F.toList $ vDefs Set.\\ uDefs

    hu = from' u uOnlies
    hv = from' v vOnlies

    from' x hss
      |traceShow ("from'",x,hss) False = undefined
      | otherwise
      = case hss of
      []   -> error "extractEdgeDefs: absurd, too few definers"
      [h'] -> h'
      hs    -> error $ "extractEdgeDefs: unhandled degeneracy. More than 3 planes at a vertex. "
                     <> show ("extractEdgeDefs",h,u,v,uOnlies,vOnlies)
             -- TODO we should either the neighbor of h in the order around the given
             -- vertex here.

--------------------------------------------------------------------------------

-- TODO: move to the definition below
data IntermediateVertex plane = Vtx { definingPlane :: !plane
                                    , ivId          :: {-# UNPACK #-} !VertexID
                                    , ivLoc         :: !(Point 3 (NumType plane))
                                    , _ivDefs       :: VertexForm.Definers plane
                                    -- , ivEdges :: Seq.Seq (LEEdge plane)
                                    }

--------------------------------------------------------------------------------

-- | Sort the vertices of a (convex) face in counter clockwise order around its
-- boundary.
--
-- running time: \(O(n \log n)\)
sortAlongBoundary      :: forall plane r. (Plane_ plane r, Ord r, Num r
                                          , Show plane, Show r
                                          )
                       => NonEmptyVector (IntermediateVertex plane)
                       -> NonEmptyVector (IntermediateVertex plane)
sortAlongBoundary face = case mv0 of
    Nothing             -> face -- already sorted, since there is only one vertex
    Just (Vtx _ _ v0 _) -> NonEmptyV.unsafeFromVector $ sortBy (cmpAround $ v1 .-. v0) face
    -- TODO: I guess that in degenerate situations, there now may be consecutive
    -- points (vertices) at the same location. We should group those and get rid of them?
  where
    -- we find the leftmost vertex v1 of the face (pick the lexicographically smallest
    -- when there are multiple), and then compute its predecessor v0 in the order along
    -- the boundary of the face (i.e. we find the point v for which the line through v1
    -- and v has maximum slope).

    -- To get the list of vertices in CCW order along the face, we then sort the vertices
    -- around v1
    Vtx _ iv1 v1 _ = minimumBy (comparing ivLoc) face
    v1' = projectPoint v1
    -- its predecessor v0, if it exists
    mv0 :: Maybe (IntermediateVertex plane)
    mv0 = foldr findPred Nothing face
    findPred v acc
      | ivId v == iv1 = acc -- skip v1 itself
      | otherwise = Just $ case acc of
                             Nothing -> v -- anything is better than nothing
                             Just u  -> maxBy cmpSlope' u v

    cmpSlope'     :: IntermediateVertex plane -> IntermediateVertex plane -> Ordering
    cmpSlope' u v = case ccw v1' (projectPoint $ ivLoc u) (projectPoint $ ivLoc v) of
                      CCW      -> GT
                      CW       -> LT
                      CoLinear -> EQ

    -- | sort the vertices around the direction wrt. (the downward projection of) w
    cmpAround :: Vector 3 r -> IntermediateVertex plane -> IntermediateVertex plane -> Ordering
    cmpAround w u v =
        ccwCmpAroundWith (Vec.prefix w) v1' u' v' <> cmpByDistanceTo v1' u' v'
      where
        u' = projectPoint $ ivLoc u
        v' = projectPoint $ ivLoc v

-- | given an edge (u,v) that has h to its left, and all remaining vertices of the face,
-- sorted in CCW order around the face starting with *v*, compute all its edgesf
faceToEdges       :: forall plane r. (Plane_ plane r, Ord r, Fractional r, Ord plane
                                     , Show plane, Show r
                                     )
                   => NonEmptyVector (IntermediateVertex plane) -> FaceEdges plane
faceToEdges faceV = case toNonEmpty faceV of
                      u :| []  -> oneVertex u
                      u :| [v] -> twoVertices u v
                      _        -> toNonEmpty $ manyVertices
  where
    manyVertices = NonEmptyV.zipWith3 mkEdge (shiftR (-1) faceV) faceV (shiftR 1 faceV)

    mkEdge (Vtx _ uIdx up uDefs) (Vtx h vIdx vp vDefs) (Vtx _ wIdx wp wDefs) =
      case extractEdgeDefs h vp vDefs wp wDefs of
        Nothing -> -- vw are separated by the unbounded vertex
                    case extractEdgeDefs h up uDefs vp vDefs of
                      Nothing                    ->
                        error "mkEdge: absurd, u and v don't share a plane!?"
                      Just (EdgeDefs _hUV _hu hv) -> (vIdx, Edge unboundedVertexId h hv)
        Just (EdgeDefs hVW _ _) -> (vIdx, Edge wIdx h hVW)
      -- the main idea is that every pair of subsequent bounded vertices necessarily has
      -- exactly one defining plane, except for when two bounded vertices are separted by
      -- the vertex at infinity.


-- | compute the face edges of the face of h, when v is the only vertex incident to h.
oneVertex                  :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                              , Show plane, Show r
                              )
                           => IntermediateVertex plane -> FaceEdges plane
oneVertex (Vtx h i v defs) = case List.sort outgoingEdges of
    [ Left _hPred, Right hSucc ] -> NonEmpty.singleton (i, Edge unboundedVertexId h hSucc)
    _ -> error "oneVertex. absurd. Other than a single Left, and Right"
  where
    otherPlanes = Set.delete h defs
    -- | Tries to compute all outgoing edges involving h. Since every plane h appears
    -- exactly once in the order around v, this should produce exactly two edges; one
    -- left and one right.
    outgoingEdges = foldMap (\h' -> let rest = toNonEmpty' $ Set.delete h' otherPlanes
                                    in case outgoingUnboundedEdge v (Two h h') rest of
                                         Just e  -> [ tagOtherPlane $ e^.extra ]
                                         Nothing -> []
                            ) otherPlanes
    tagOtherPlane (EdgeDefiners hl hr) = if h == hl then Right hr else Left hl
    toNonEmpty' s = case NonEmpty.nonEmpty $ F.toList s of
                      Just xs -> xs
                      _       -> error "oneVertex. Absurd, there should be at least 3 definers"

-- | Compute the edges of the face incident to h, when there are only two vertices
-- incident to that face.
--
-- more or less a special case of the manyVertices scenario, in which we don't know
-- the if edge should be oriented from u to v or from v to u.
twoVertices  :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                , Show plane, Show r
                )
             => IntermediateVertex plane -> IntermediateVertex plane -> FaceEdges plane
twoVertices (Vtx h ui up uDefs) (Vtx _ vi vp vDefs) =
  case extractEdgeDefs h up uDefs vp vDefs >>= withUVOrder of
      Nothing  -> error "twoVertices. absurd, h and h' don't define an edge!?"
      Just (EdgeDefs hr hu hv, uBeforeV) -- -> traceShowWith ("twoVertices",u,v,uBeforeV,) $
        | uBeforeV -> NonEmpty.fromList [ (vi, Edge ui                h hr)
                                        , (ui, Edge unboundedVertexId h hu)
                                        ]
                      -- the edge must be oriented from v to u so that h is on the left
        | otherwise  -> NonEmpty.fromList [ (ui, Edge vi                h hr)
                                          , (vi, Edge unboundedVertexId h hv)
                                          ]
                        -- the edge must be oriented from u to v
  where
    -- determine if u lies before v in the order of u and v along the intersection line of
    -- h and hr.
    withUVOrder e@(EdgeDefs hr _ _) =
      ( \l -> let m = perpendicularTo l &anchorPoint .~ projectPoint up
              in (e, projectPoint vp `onSide` m /= LeftSide)
              -- if v lies on the left it lies further along commonLine. So u lies before
              -- v if v does not lie in the left haflpalne
      ) <$> intersectionLine' h hr


--------------------------------------------------------------------------------

-- | Vertices in of the lower envelope in adjacencylist form.
type BoundedVertex = BoundedVertexF Seq.Seq


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

--------------------------------------------------------------------------------
-- * Convenience functions

-- | Group consecutive elements into non-empty groups.
--
--
-- running time: \(O(n\log n)\)
--
-- >>> groupOnCheap fst [ (1,"foo"), (2,"bar"), (2,"blaa"), (1,"boeez"), (1,"blap"), (4,"blax"), (4,"bleh"), (100,"floep")]
-- [[(1,"foo"),(1,"boeez"),(1,"blap")],[(2,"bar"),(2,"blaa")],[(4,"blax"),(4,"bleh")],[(100,"floep")]]
groupOnCheap  :: (Foldable f, Ord b)
              => (a -> b) -> f a -> [NonEmptyV.NonEmptyVector a]
groupOnCheap f = fmap NonEmptyV.unsafeFromVector . V.groupBy ((==) `on` f) . sortOnCheap f

-- | returns the maximum using the given comparison function
maxBy         :: (t -> t -> Ordering) -> t -> t -> t
maxBy cmp a b = case cmp a b of
                  LT -> b
                  _  -> a


-- | shift the vector by d
shiftR     :: Int -> NonEmptyVector v -> NonEmptyVector v
shiftR d v = let n = length v
             in NonEmptyV.generate1 n $ \i -> v NonEmptyV.! ((i+n+d) `mod` n)
