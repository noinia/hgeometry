module Ipe.FromIpe.UnboundedConvexChain where

import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.PolyLine (PolyLine, polyLineFromPoints)
import           HGeometry.Polygon.Convex.Unbounded
import           HGeometry.Vector
import           Ipe.Attributes
import           Ipe.Path
import           Ipe.Types

-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Ipe.Attributes
-- >>> import Ipe.Color(IpeColor(..))
-- >>> import qualified HGeometry.PolyLine as PolyLine
-- >>> :{
-- let testPath :: Path Int
--     testPath = Path . fromSingleton  . PolyLineSegment
--              . PolyLine.polyLineFromPoints . NonEmpty.fromList
--              $ [ origin, Point2 10 10, Point2 200 100 ]
--     testPathAttrs :: IpeAttributes Path Int
--     testPathAttrs = attr SStroke (IpeColor "red")
--     testObject :: IpeObject Int
--     testObject = IpePath (testPath :+ testPathAttrs)
-- :}


-- | Renders an unbounded convex region as a Path
renderChain :: (Foldable1 nonEmpty, Point_ vertex 2 r, Num r)
            => UnboundedConvexRegionF r nonEmpty vertex :+ IpeAttributes Path r
            -> IpeObject' Path r
renderChain (reg@(Unbounded v pts w) :+ ats) =
    (poly^.re _asPolyLine) :+     attr SArrow  normalArrow
                               <> attr SRArrow normalArrow
                               <> ats
  where
    poly = case extremalVertices (mapChain toNonEmpty reg) of
             Left p              -> f p p
             Right (Vector2 p q) -> f p q
    f p q = polyLineFromPoints . fmap (^.asPoint)
          $ p .-^ v NonEmpty.<| toNonEmpty pts <> NonEmpty.singleton (q .+^ w)


-- | Convert to a polyline. Ignores all non-polyline parts
--
-- >>> testPath ^? _asPolyLine
-- Just (PolyLine [Point2 0 0,Point2 10 10,Point2 200 100])
_asPolyLine :: Prism' (Path r) (PolyLine (Point 2 r))
_asPolyLine = prism' poly2path path2poly
  where
    poly2path = Path . fromSingleton  . PolyLineSegment
    path2poly = preview (pathSegments.folded._PolyLineSegment)
    -- TODO: Check that the path actually is a polyline, rather
    -- than ignoring everything that does not fit

-- | Helper to produce a singleton sequence
fromSingleton :: a -> Seq.Seq a
fromSingleton = Seq.singleton
