{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module HalfPlane.CommonIntersection.BoundedSpec
  where


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
                                                       -- , boundaries
                                                       -- , extremes
                                                       , classifyHalfPlane
                                                       )
import           Control.Lens
import           Control.Monad ((=<<),(<=<))
import           Data.Foldable1
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Sequence.NonEmpty
import           Data.These
import           HGeometry.Sequence.Alternating
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           Prelude hiding (zipWith)
import           Data.Zip
import           Test.Hspec.QuickCheck
import           Data.Coerce

import           R

import           Test.Util
import           Debug.Trace
import           Data.Foldable
import           Test.QuickCheck hiding (Negative)
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


-- blah = case partitionHalfPlanes halfplanes of
--          These _ nonverts -> case boundaries nonverts of
--            These upper lower -> upper
--          x -> error $ "blah" <> show x

testz = partitionHalfPlanes $ halfplanes



foo = traverse_ (\h -> print (h, classifyHalfPlane h)) halfplanes
halfplanes =
  toNonEmpty (intersectingHalfPlanes myTriangle) <> myHalfPlanes

test :: Maybe (ConvexPolygon (Point 2 R :+ HalfPlane))
test = boundedCommonIntersection $
       toNonEmpty (intersectingHalfPlanes myTriangle) <> myHalfPlanes

-- spec = describe "bounded tests" $ do
--          prop ""



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

instance ( Ord r, Fractional r
         , IsDrawable (Ipe r) line, AttrOf (Ipe r) line ~ PathAttributes r
         , HalfSpaceF line `IsIntersectableWith` Rectangle (Point 2 r)
         ) => IsDrawable (Ipe r) (HalfSpaceF line) where
  type AttrOf (Ipe r) (HalfSpaceF line) = PathAttributes r
  draw ats h = case h `intersect` defaultBox of
      Nothing -> []
      Just is -> case is of
        ActualPolygon interior -> mconcat [ draw @(Ipe r)
                                                 (ats <>
                                                   [ opacity ?~ Named "20%"
                                                   , stroke  .~ Nothing
                                                   ]
                                                 ) interior
                                          , boundary
                                          ]
        _                      -> boundary
    where
      boundary = draw @(Ipe r) (ats <> [fill .~ Nothing]) (h^.boundingHyperPlane)

instance ( Ord r, Fractional r
         -- , HalfSpaceF line `IsIntersectableWith` Rectangle (Point 2 r)
         ) => IsDrawable (Ipe r) (LinePV 2 r) where
  type AttrOf (Ipe r) (LinePV 2 r) = PathAttributes r
  draw ats l = case l `intersect` defaultBox of
                 Nothing -> []
                 Just is -> case is of
                   Line_x_Box_LineSegment seg -> draw @(Ipe r) ats seg
                   _                          -> [] -- don't draw singleton points

spec :: Spec
spec = describe "Bounded common intersection spec" $ do
        prop "is convex polygon" $
          \(t :: Triangle (Point 2 R)) (hs :: NonEmpty HalfPlane) ->
            case boundedCommonIntersection (toNonEmpty (intersectingHalfPlanes t) <> hs) of
              Nothing   -> property True -- this test may be less useful than ideal
              Just poly -> counterexample (show poly) $ verifyConvex poly


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
