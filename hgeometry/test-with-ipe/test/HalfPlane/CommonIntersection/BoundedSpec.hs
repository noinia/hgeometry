{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module HalfPlane.CommonIntersection.BoundedSpec
  where

import           GHC.Generics (Generic)
import           Test.Util
import           HGeometry.Sign
import           HGeometry.Box (Rectangle, LineBoxIntersection(..))
import           Data.Bifunctor (first)
import           HGeometry.Foldable.Sort
import           HGeometry.HalfSpace
import           HGeometry.Point
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Ext
import           HGeometry.Line
import           HGeometry.HyperPlane
import           HGeometry.Polygon
import           HGeometry.Intersection
import           HGeometry.HalfPlane.CommonIntersection.Chain as Chain
import           HGeometry.HalfPlane.CommonIntersection.Bounded
import           HGeometry.HalfPlane.CommonIntersection( partitionHalfPlanes
                                                       , classifyHalfPlane
                                                       )
import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           Prelude hiding (zipWith)
import           Test.Hspec.QuickCheck
import qualified Data.Set.NonEmpty as NESet
import           R
import           Debug.Trace
import           Data.Foldable
import           Test.QuickCheck hiding (Negative, Positive)
import           Ipe.Color
import           Ipe
import           Ipe.Draw
import           System.OsPath
import           HGeometry.Kernel.Instances ()
import           Test.Hspec

--------------------------------------------------------------------------------

type HalfPlane = HalfPlaneF (LinePV 2 R)


myTriangle :: Triangle (Point 2 R)
myTriangle = Triangle (Point2 0 0) (Point2 1010 0) (Point2 0 1010)

myHalfPlanes :: NonEmpty HalfPlane
myHalfPlanes = NonEmpty.fromList
               [
                 leftHalfPlane $ LinePV (Point2 10 20) (Vector2 10 3)
               , leftHalfPlane $ LinePV (Point2 40 3) (Vector2 1 5)
               , leftHalfPlane $ LinePV (Point2 0  200) (Vector2 1 (-1))
               , rightHalfPlane $ LinePV (Point2 0 1000) (Vector2 1000 (-1))
               , rightHalfPlane $ LinePV (Point2 0 900) (Vector2 1 20)
               ]


spec :: Spec
spec = describe "Bounded Halfspace intersection tests" $ do
         drawHalfspaceCorrect
         prop "is convex polygon" $
           \(t :: Triangle (Point 2 R)) (hs :: NonEmpty HalfPlane) ->
             case boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes t) <> hs) of
               Nothing   -> property True -- this test may be less useful than ideal
               Just poly -> counterexample (show poly) $ verifyConvex poly
         voronoiCell
         voronoiBug

--------------------------------------------------------------------------------

-- | Given a triangle T, a point site s inside the triangle, and a bunch of other points
--  compute the voronoi cell of s restricted to T.
--
-- this tests makes sure that we produce a (non-empty) convex polygon in this case.
voronoiCell :: Spec
voronoiCell = prop "bounded voronoi cell" $
                \(PointInTriangle t s) (rest :: NESet.NESet (Point 2 R)) ->
                  let input = assignColors rest
                      hs    = NESet.map (\(t' :+ color) ->
                                            closerHalfPlane s t' :+ Just color) input
                      triHs = (:+ Nothing) <$> intersectingHalfPlanes t
                      allHs = foldr NESet.insert hs triHs
                  in case boundedCommonIntersection allHs of
                       Nothing    -> ipeCounterExample (t, allHs, s :+ orange @R
                                                       , input) $ property False
                       Just _cell -> property True


-- | closerHalfPlane s t is the halfplane closer to s than to t.
closerHalfPlane     :: (Point_ point 2 r, Ord r, Fractional r)
                    => point -> point -> HalfSpaceF (VerticalOrLineEQ r)
closerHalfPlane s t = half&halfSpaceSign %~ \sign ->
                        if (dist q s < dist q t) then sign else flipSign sign
  where
    l    = let LinePV p v = bisector s t in fromPointAndVec p v
    half = HalfSpace Positive l
    q    = pointInteriorTo half
    dist = squaredEuclideanDist

----------------------------------------

-- for debugging purposes
voronoiBug :: Spec
voronoiBug = prop "bounded voronoi cell" $
               let
                 t :: Triangle (Point 2 R)
                 t = Triangle (Point2 14.63157 16.3) (Point2 (-0.83334) (-12.70371)) (Point2 7.1 (-8.52632))
                 s = Point2 7.78184 2.73599

                 rest :: NESet.NESet (Point 2 R)
                 rest = NESet.fromList (Point2 13 (-24) :| [])

                 input = assignColors rest
                 hs    = NESet.map (\(t' :+ color) ->
                                       closerHalfPlane s t' :+ Just color) input
                 triHs = (:+ Nothing) <$> intersectingHalfPlanes t
                 allHs = foldr NESet.insert hs triHs
               in case boundedCommonIntersection allHs of
                       Nothing    -> ipeCounterExample (t, allHs, s :+ orange @R
                                                       , input) $ property False
                       Just _cell -> property True

--------------------------------------------------------------------------------


testz = partitionHalfPlanes $ halfplanes



foo = traverse_ (\h -> print (h, classifyHalfPlane h)) halfplanes
halfplanes =
  toNonEmpty (intersectingHalfPlanes myTriangle) <> myHalfPlanes

test :: Maybe (ConvexPolygon (Point 2 R :+ HalfPlane))
test = boundedCommonIntersection $
       toNonEmpty (intersectingHalfPlanes myTriangle) <> myHalfPlanes


mainx = writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent
                    . mconcat   $
                    [ draw @(Ipe R) [fill ?~ gray] myHalfPlanes
                    , draw @(Ipe R) [fill ?~ blue] myTriangle
                    , draw @(Ipe R) [ fill ?~ red
                                    , stroke ?~ green
                                    ] inters
                    ]
  where
    Just inters = test


theBug = prop "theBug " $ do
           let t  :: Triangle (Point 2 R)
               t  = Triangle (Point2 0 2) (Point2 1 0) (Point2 0 0.5)
               hs = NonEmpty.fromList
                    [ leftHalfPlane (LinePV (Point2 0.5 0) (Vector2 0 1))
                    , rightHalfPlane $ lineThrough (Point2 (-5.99353) 3.33474)
                                                   (Point2 4.9484 0.377459)

                    ]
           -- case boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes @(LinePV 2 R) t)) of
           --    Nothing   -> property False
           --    Just poly -> ipeCounterExample poly $
           --                 ipeCounterExample (toNonEmpty (intersectingHalfPlanes  @(LinePV 2 R)  t)) $
           --                 property False
           case boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes t) <> hs) of
              Nothing   -> property False
              Just poly -> ipeCounterExample poly $
                           ipeCounterExample (toNonEmpty (intersectingHalfPlanes t) <> hs) $
                           property True



blah = do
    print $ partitionHalfPlanes allHs
    writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent
                    . mconcat   $
                    [ draw @(Ipe R) [fill ?~ gray] hs
                    , draw @(Ipe R) [fill ?~ blue] t
                    , draw @(Ipe R) [ fill ?~ red
                                    , stroke ?~ green
                                    ] inters
                    , draw @(Ipe R) [ fill ?~ orange
                                    , stroke ?~ green
                                    ] tri
                    ]
  where
    allHs = toNonEmpty (intersectingHalfPlanes t) <> hs
    Just inters = boundedCommonIntersection allHs
    Just tri    = boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes @(VerticalOrLineEQ R) t))
    t  :: Triangle (Point 2 R)
    t  = Triangle (Point2 0 2) (Point2 1 0) (Point2 0 0.5)
    hs = NonEmpty.fromList
         [ leftHalfPlane (LinePV (Point2 0.5 0) (Vector2 0 1))
         , rightHalfPlane $ lineThrough (Point2 (-5.99353) 3.33474)
                                        (Point2 4.9484 0.377459)

         ]

blah2 = let h :: HalfPlaneF (LinePV 2 R)
            h = HalfSpace Negative ( LinePV ( Point2 0 2 ) ( Vector2 0 ( -1.5 ) ) )
        in prop "h should be a right hsalfplane" $
             Point2 10 0 `intersects` h





bug = do
    print $ boundedCommonIntersection (triHs <> hs)
    writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent
                    . mconcat   $
                    [ draw @(Ipe R) [fill ?~ gray] (view core <$> hs)
                    , draw @(Ipe R) [fill ?~ orange
                                    , layer ?~ "triHs"
                                    ] (view core <$> triHs)
                    , draw @(Ipe R) [fill ?~ blue
                                    , layer ?~ "theTriangle"
                                    ] t
                    -- , draw @(Ipe R) [ fill ?~ red
                    --                 , stroke ?~ green
                    --                 ] inters
                    -- , draw @(Ipe R) [ fill ?~ orange
                    --                 , stroke ?~ green
                    --                 ] tri
                    ]

  where
    t  = Triangle (Point2 7.75 (-4.5)) (Point2 (-8.85715) 9) (Point2 7.4 (-4.42858))
    -- Just inters = boundedCommonIntersection (triHs <> hs)

    triHs =

      NonEmpty.fromList
      [ HalfSpace Negative ( NonVertical ( LineEQ ( -0.81291 ) 1.79999 ) ) :+ Nothing
      , HalfSpace Positive ( NonVertical ( LineEQ ( -0.82602 ) 1.68389 ) ) :+ Nothing
      , HalfSpace Positive ( NonVertical ( LineEQ ( -0.20406 ) ( -2.91856 ) ) ) :+ Nothing
      ]


    hs :: NonEmpty (HalfPlaneF (VerticalOrLineEQ R) :+ Maybe String)
    hs = NonEmpty.fromList
      [ HalfSpace Positive( NonVertical ( LineEQ ( -0.5 ) 1.25 ) ) :+ Just "black"
        -- ( Plane 0 0 0 :+ ( Point2 0 0 :+ IpeColor ( Named "black" ) ) )
      , HalfSpace Positive  ( NonVertical ( LineEQ ( -1 ) 2 ) ) :+ Just "white"
      ]
                 -- ( Plane 0 ( -2 ) 1 :+ ( Point2 0 1 :+ IpeColor ( Named "white" ) ) ) ] )











oldBug = do
    print $ boundedCommonIntersection (triHs <> hs)
    writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent
                    . mconcat   $
                    [ draw @(Ipe R) [fill ?~ gray] (view core <$> hs)
                    , draw @(Ipe R) [fill ?~ orange
                                    , layer ?~ "triHs"
                                    ] (view core <$> triHs)
                    , draw @(Ipe R) [fill ?~ blue
                                    , layer ?~ "theTriangle"
                                    ] t
                    , draw @(Ipe R) [ fill ?~ red
                                    , stroke ?~ green
                                    ] inters
                    -- , draw @(Ipe R) [ fill ?~ orange
                    --                 , stroke ?~ green
                    --                 ] tri
                    ]

  where
    t  = Triangle ( Point2 5 6 ) ( Point2 ( -1 ) 2 ) ( Point2 0 ( -6 ) )
    Just inters = boundedCommonIntersection (triHs <> hs)

    triHs = NonEmpty.fromList
      [HalfSpace Negative ( NonVertical ( LineEQ 0.66666 2.66666 ) ) :+ Nothing
      , HalfSpace Positive ( NonVertical ( LineEQ ( -8 ) ( -6 ) ) ) :+ Nothing
      , HalfSpace Positive ( NonVertical ( LineEQ 2.4 ( -6 ) ) ) :+ Nothing]

    hs :: NonEmpty (HalfPlaneF (VerticalOrLineEQ R) :+ Maybe String)
    hs = NonEmpty.fromList
      [
        HalfSpace Positive ( VerticalLineThrough ( -0.5 ) ) :+ Just "white"
        -- ( Plane 0 0 0 :+ ( Point2 0 0 :+ IpeColor ( Named "white" ) ) )
      -- , HalfSpace Negative ( NonVertical ( LineEQ ( -0.27028 ) 1.71486 ) ) :+ Just "red"
        -- ( Plane 0 ( -7.4 ) 13.69 :+ ( Point2 0 3.7 :+ IpeColor ( Named "red" ) ) )
      ]










-- floep =

          -- it "thebug" $ do
          --   let t  = Triangle (Point2 0 0) (Point2 1 (-1)) (Point2 1 (-2) :: Point 2 R)
          --       hs = HalfSpace Negative (LinePV (Point2 0 0) (Vector2 0 (-1))) :| []
          --       res :: Maybe (ConvexPolygonF ViewL1 (Point 2 R :+ HalfPlane))
          --       res = boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes t) <> hs)
          --   res `shouldBe`
          --     Just (uncheckedFromCCWPoints . NonEmpty.fromList $
          --            [Point2 0 0     :+  HalfSpace Negative (LinePV (Point2 0 0) (Vector2 0 (-1)))
          --            ,Point2 1 (-2)  :+  HalfSpace Positive (LinePV (Point2 0 0) (Vector2 0 (-1)))
          --            ,Point2 1 (-1)  :+  HalfSpace Positive (LinePV (Point2 0 0) (Vector2 0 (-1)))
          --            ]) -- TODO; fix





--             , Plane 2 0 1 :+ ( Point2 ( -1 ) 0 :+ IpeColor ( Named "black" ) )

--             , EnvVertex
--     ( Plane 2 0 1 :+ ( Point2 ( -1 ) 0 :+ IpeColor ( Named "black" ) ) )
--     ( Plane 0 0 0 :+ ( Point2 0 0 :+ IpeColor ( Named "white" ) ) )
--     ( Plane 0 ( -7.4 ) 13.69 :+ ( Point2 0 3.7 :+ IpeColor ( Named "red" ) ) ) []
--     ( Point2 ( -0.5 ) 1.85 ) 0 :| [] )


-- ( "halfPlanes"

--   , Plane 2 0 1 :+ ( Point2 ( -1 ) 0 :+ IpeColor ( Named "black" ) ), "-> ",


drawHalfspaceCorrect :: Spec
drawHalfspaceCorrect = describe "drawing halfspaces is correct" $ do
    prop "draw halfspace correct (LinePV 2 R)" $ do
      \(h :: HalfPlaneF (LinePV 2 R)) -> case h `intersect` defaultBox of
        Nothing -> discard
        Just is -> case is of
          ActualPolygon interior -> let q = pointInteriorTo interior
                                    in ipeCounterExample interior $ q `intersects` h
          _                      -> discard

    prop "draw halfspace correct (VerticalLineEQ R)" $ do
      \(h :: HalfPlaneF (VerticalOrLineEQ R)) -> case h `intersect` defaultBox of
        Nothing -> discard
        Just is -> case is of
          ActualPolygon interior -> let q = pointInteriorTo interior
                                    in counterexample (show q) $
                                       ipeCounterExample (interior) $ q `intersects` h
          _                      -> discard
