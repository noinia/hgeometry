{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec(spec) where

import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, catMaybes)
import           Data.Traversable
import           Golden
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.PointAndVector
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           Ipe
import           System.OsPath
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5


--------------------------------------------------------------------------------
-- to ipe
instance (Fractional r, Ord r, Show r) => HasDefaultFromIpe (HalfLine (Point 2 r)) where
  type DefaultFromIpe (HalfLine (Point 2 r)) = Path
  defaultFromIpe = _asHalfLine

-- | Try to parse an Line segment with an arrow head as a HalfLine
_asHalfLine :: (Fractional r, Ord r, Show r)
            => Prism' (IpeObject r) (HalfLine (Point 2 r) :+ IpeAttributes Path r)
_asHalfLine = prism' (\(hl :+ ats) -> IpePath (ipeHalfLine hl ! ats)) objToHalfLine
  where
    objToHalfLine = \case
      IpePath (path' :+ ats) -> case path'^?_asClosedLineSegment  of
        Just (ClosedLineSegment s t) -> case (hasAttr SArrow ats, hasAttr SRArrow ats) of
                                          (True,False) -> Just $ HalfLine s (t .-. s) :+ ats
                                          (False,True) -> Just $ HalfLine s (s .-. t) :+ ats
                                          _            -> Nothing
        Nothing                      -> Nothing
      _                    -> Nothing

    hasAttr a = isJust . lookupAttr a

--------------------------------------------------------------------------------

type HalfPlane line = HalfSpaceF line



--------------------------------------------------------------------------------

-- | A single component of a HalfPlane SimplePolygon intersection
data HalfPlanePolygonIntersection f r vertex =
    HalfPlane_x_SimplePolygon_Vertex vertex
  | HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment vertex)
  | HalfPlane_x_SimplePolygon_Polygon (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))
  -- deriving (Show,Eq,Functor)

type instance Intersection (HalfPlane line) (SimplePolygonF f point) =
  [HalfPlanePolygonIntersection f (NumType point) point]

type instance Intersection (HalfPlane line) (ConvexPolygonF f point) =
  Maybe (HalfPlanePolygonIntersection f (NumType point) point)


instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (SimplePolygonF f point) where
  halfPlane `intersects` poly = anyOf (vertices.asPoint) (`intersects` halfPlane) poly


data CurrentState f r vertex =
    First (NonEmpty vertex)
  | Later { _lastVertex          :: vertex
          , _currentComponent    :: Maybe (CurrentComponent r vertex)
          , _completedComponents :: [HalfPlanePolygonIntersection f r vertex]
          , _firstComponent      :: Maybe (FirstComponent r vertex)
          }


data CurrentComponent r vertex = Current { _currentVertices :: NonEmpty vertex
                                         -- ^ vertices of the current component
                                         -- most recent one first
                                         , _entryPoint :: Maybe (Point 2 r)
                                         -- ^ the first point in our component
                                         }

currentVertices = lens _currentVertices (\(Current _ p) vs -> Current vs p)

                                 -- Complete (NonEmpty vertex)
  --                              -- ^ current vertices of the component
  --                              | Partial { _currentVertices :: NonEmpty vertex
  --                                        -- ^ vertices of the current component
  --                                        -- most recent one first
  --                                        , _entryPoint :: Point 2 r
  --                                        -- ^ the first point in our component
  --                                        }

-- push v = \case
--   Complete vs  -> Complete $ v NonEmpty.<| vs
--   Partial vs p -> Partial (v NonEmpty.<| vs) p

data FirstComponent r vertex = FirstComponent { _exitPoint        :: Maybe (Point 2 r)
                                              -- ^ the point where we exited the component
                                              -- if it is not a vertex
                                              , _interiorVertices :: NonEmpty vertex
                                              }

instance ( Point_ vertex 2 r, Num r, Ord r, VertexContainer f vertex, HyperPlane_ line 2 r
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , Intersection (LinePV 2 r) line ~ Maybe (LineLineIntersection line')
         , NumType line' ~ r
         , IsIntersectableWith (LinePV 2 r) line
         , HasFromFoldable1 f
         ) => IsIntersectableWith (HalfPlane line) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly = case foldrMap1 initialize compute $ toNonEmptyOf vertices poly of
      First _                         -> [HalfPlane_x_SimplePolygon_Polygon $ Original <$> poly]
      Later v current completed first -> combine v current first <> completed
    where
      initialize v
        | (v^.asPoint) `intersects` halfPlane = First (NonEmpty.singleton v)
        | otherwise                           = Later v Nothing [] Nothing

      -- we maintain the current component, if it exists and the completed components.
      -- we have to handle the first component separately, as we may have to combine the first
      -- and last component into one.
      compute   :: vertex -> CurrentState f r vertex -> CurrentState f r vertex
      compute v = \case
        First vs@(u :| _)
          | (v^.asPoint) `intersects` halfPlane -> First (v NonEmpty.<| vs)
          | otherwise                           ->
              Later v Nothing [] (Just $ FirstComponent exitPoint vs)
              where
                exitPoint = intersectionPoint u v

        Later u current completed first
          | (v^.asPoint) `intersects` halfPlane -> Later v (Just current') completed  first
          | otherwise                           -> Later v Nothing         completed' first
           where
             current'   = case current of
               Nothing -> Current (NonEmpty.singleton v) entryPoint
                 where
                   entryPoint = intersectionPoint u v
               Just current'' -> current''&currentVertices %~ (v NonEmpty.<|)

             completed' = case current of
               Nothing        -> completed
               Just current'' -> complete (Just v) current'' : completed

      -- | Given a points u and v that do not both lie on the same side of the line l
      -- bounding the halfplane, computes the intersection point of the *open* edge (u,v)
      -- with l (if such a point exists).
      intersectionPoint u v = case LinePV (u^.asPoint) (v .-. u)
                                     `intersect` (halfPlane^.boundingHyperPlane) of
        Just (Line_x_Line_Point p) | p /= (u^.asPoint) -> Just p
        _                          -> Nothing


      -- | Given a vertex v that does *not* lie in the halfplane, and the current state.
      -- finish up the current component.
      complete   :: Maybe vertex -> CurrentComponent r vertex
                 -> HalfPlanePolygonIntersection f r vertex
      complete v = \case
        Current (u :| [])  Nothing -> HalfPlane_x_SimplePolygon_Vertex u
        Current (u :| [w]) Nothing -> HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment w u)
        Current (u :| ws)  mEntry  -> HalfPlane_x_SimplePolygon_Polygon
                                    $ case catMaybes [mEntry, intersectionPoint u =<< v] of
           extras -> uncheckedFromCCWPoints $ (Extra <$> extras) <<> (Original <$> (u :| ws))

      combine v mCurrent = \case
        Nothing    -> mCurrent >>= complete Nothing
        Just first -> case mCurrent of
          Nothing      -> _
          Just current -> _



xs <<> ys = case NonEmpty.nonEmpty xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys





                         -- (HalfSpaceF (LinePV 2 (RealNumber 5)))
                         -- (SimplePolygonF
                         --    (HGeometry.Cyclic.Cyclic
                         --       Data.Vector.NonEmpty.Internal.NonEmptyVector)
                         --    (Point 2 R))


instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (ConvexPolygonF f point) where
  halfPlane `intersects` poly = halfPlane `intersects` (toSimplePolygon poly)
    -- TODO there is a better, O(log n) time implementation. use that instead ...


--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]
                 [osp|polygonHalfspaceIntersection.out.ipe|]

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (halfPlanes, polygons) <-  runIO $ do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (SimplePolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfpalne of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, pgs)

    for polygons $ \polygon ->
      for halfPlanes $ \halfPlane ->
        it ("intersects halfplane and polygon") $
          (halfPlane `intersects` polygon) `shouldBe` True -- TODO; fix
    pure ()
