module Hatching
  ( hatching

  ) where

import           Hiraffe.Graph.Class
import           HGeometry.Intersection
import           Data.Maybe
import           Control.Lens
import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.Polygon
import           HGeometry.PlaneGraph.Class
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Ext
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified HGeometry.Set.Util as Set
import qualified Data.Set as Set

--------------------------------------------------------------------------------


data EventKind r = Delete (ClosedLineSegment (Point 2 r))
                 | Insert (ClosedLineSegment (Point 2 r))
                 | Hatch
                 deriving (Show,Eq,Ord)
                 -- The order of the constructors is on purpose!

type Event r = Point 2 r :+ EventKind r

type CurrentEdge r = ClosedLineSegment (Point 2 r)

-- | Given a vector v and a polygon, computes hatchings for the
-- polygon in the direction given by v. In particular: we produce line
-- segments that are *perpendicular* to v, and so that any consecutive
-- hatchings are s and s+v. All line segments/hatchings produced
-- intersect the input polygon.
--
-- \(O(n\log n + k)\), where k is the output size.
hatching          :: forall polygon vertex r.
                     ( BidirGraph_ polygon
                     , Polygon_ polygon vertex r
                     , HasEdges polygon polygon
                     , Ord r, Fractional r)
                  => Vector 2 r
                     -- ^ direction perpendicular to the hatchings;
                      -- i.e. the output line segments will be
                      -- perpendicular to this vector (and separated by this vector)
                  -> polygon -> [ClosedLineSegment (Point 2 r)]
hatching dir poly = snd $ foldl' handle (mempty, []) events
  where

    -- | vector perpendicular to the hatching direction
    perpDir = view direction . perpendicularTo $ LinePV v0 dir

    cmp = cmpInDirection2 dir
    cmpEvent (p :+ ep) (q :+ eq) = cmp p q <> compare ep eq

    cmpAt p' segA segB = cmpInDirection2 perpDir (f p' segA)           (f p' segB)
                      <> cmpInDirection2 perpDir (f (p' .+^ dir) segA) (f (p' .+^ dir) segB)
       -- if segA and segB intersect exactly at the same point v on the line through p'
       -- also compute their intersection a bit further along the order; and use that
       -- to determine their ordering.
      where
        f p seg = case LinePV p perpDir `intersect` supportingLine seg of
                    Just (Line_x_Line_Point q) -> q
                    Just _                     -> seg^.start
                    _                          -> error "absurd: no intersection point"

    toEvents e = let e'@(LineSegment_ s t) = orient e
                 in [ s :+ Insert e',  t :+ Delete e' ]

    orient seg = case cmp (seg^.start) (seg^.end) of
                   GT -> seg&start .~ (seg^.end)
                            &end   .~ (seg^.start)
                   _ -> seg

    -- | The vertex events
    vertexEvents@((v0 :+ _) :| _) = NonEmpty.sortBy cmpEvent . NonEmpty.fromList
                                  $ foldMapOf edgeSegments (toEvents . fmap (^.asPoint)) poly
    lastVtx :+ _ = NonEmpty.last vertexEvents

    -- | All our events
    events :: NonEmpty (Event r)
    events = case NonEmpty.nonEmpty hatchEvents of
               Just evts -> mergeSortedBy cmpEvent vertexEvents evts
               Nothing   -> vertexEvents

    -- | Produce the hatch events
    hatchEvents = fmap (:+ Hatch)
                . takeWhile (\q -> cmp q lastVtx == LT)
                . drop 1 -- drop v0 itself
                $ iterate (.+^ dir) v0

    handle :: (Set.Set (CurrentEdge r), [ClosedLineSegment (Point 2 r)])
           -> Event r
           -> (Set.Set (CurrentEdge r), [ClosedLineSegment (Point 2 r)])
    handle (status, output) = \case
      v :+ Delete seg -> (Set.deleteAllBy (cmpAt v) seg status, output)
      v :+ Insert seg -> (Set.insertBy    (cmpAt v) seg status, output)
      p :+ Hatch      -> (status, hatch p status <> output)

    hatch p status = evens $ zipWith ClosedLineSegment xs (drop 1 xs)
      where
        xs = mapMaybe intersectionPoint $ Set.toAscList status

        intersectionPoint seg = case LinePV p perpDir `intersect` supportingLine seg of
                                  Just (Line_x_Line_Point q) -> Just q
                                  _                          -> Nothing




-- | Take all elements on even indices
--
-- >>> events [0..10]
-- [0,2,4,6,8,10]
evens :: [a] -> [a]
evens = \case
  (y:_:ys) -> y : evens ys
  ys       -> ys
