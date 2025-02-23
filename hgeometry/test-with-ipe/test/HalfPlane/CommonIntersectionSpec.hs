{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module HalfPlane.CommonIntersectionSpec(spec) where

import           Control.Lens hiding (below)
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import           Golden
import           HGeometry.Box
import           HGeometry.HalfPlane.CommonIntersection
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Sequence.Alternating
import           HGeometry.Transformation
import           Ipe
import           Ipe.Color
-- import           Paths_hgeometry
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck (arbitrary, generate)
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

type HalfPlane r = HalfSpaceF (VerticalOrLineEQ r)

spec :: Spec
spec = describe "common halfplane intersection tests" $ do
         it "myHalfplanes test" $
           commonIntersection myHalfPlanes `shouldBe` theAnswer
         it "myHalfplanes2 test" $
           commonIntersection myHalfPlanes2 `shouldBe` theAnswer2
         -- generateGoldenSpec [osp|commonIntersectionAllNegatives1|]
         -- generateGoldenSpec [osp|commonIntersectionAllNegatives|]



generateGoldenSpec theName = do
    halfPlanes <- runIO $ onlyNegatives
    goldenWith [osp|data/test-with-ipe/golden/|]
                    (ipeContentGolden { name = theName  })
                    ( myIpeTest halfPlanes)


myHalfPlanes :: NonEmpty (HalfPlane R)
myHalfPlanes = NonEmpty.fromList
               [ below $ LineEQ 1    1
               , below $ LineEQ (-1) 2
               , leftOf $ 10
               ]

theAnswer :: CommonIntersection (HalfPlane R) R
theAnswer = UnboundedRegion . Chain
          $ Alternating (myHalfPlanes NonEmpty.!! 0)
                        (Seq.fromList $ [ (Point2 (1/2) (3/2), myHalfPlanes NonEmpty.!! 1)
                                        , (Point2 10    (-8),  myHalfPlanes NonEmpty.!! 2)
                                        ]
                        )



myHalfPlanes2 :: NonEmpty (HalfPlane R)
myHalfPlanes2 = NonEmpty.fromList
                [ above $ LineEQ 1    1
                , above $ LineEQ (-1) 2
                , rightOf $ (-2)
                ]

theAnswer2 :: CommonIntersection (HalfPlane R) R
theAnswer2 = UnboundedRegion . Chain
          $ Alternating (myHalfPlanes2 NonEmpty.!! 2)
                        (Seq.fromList $ [ (Point2 (-2) 4,      myHalfPlanes2 NonEmpty.!! 1)
                                        , (Point2 (1/2) (3/2), myHalfPlanes2 NonEmpty.!! 0)
                                        ]
                        )


-- readHalfPlanes         :: OsPath -> IO (NonEmpty (HalfPlane R))
-- readHalfPlanes theName = readAllFrom ([osp|data/test-with-ipe/golden/|] <> theName)
--                          <$> \(lines :: [LineSegment ]) ->
--   do

--     page <- readSinglePageFileThrow $
--      return $ page^..


-- halfspaceGoldenSpec theName = do
--     halfPlanes <- runIO $ readHalfplanes
--     goldenWith  [osp|data/test-with-ipe/golden/|]
--                     (ipeContentGolden { name = theName  })
--                     ( myIpeTest halfPlanes)



--------------------------------------------------------------------------------
-- * some helpers for producing Halfplanes

below :: LineEQ r -> HalfPlane r
below = HalfSpace Negative . NonVertical

above :: LineEQ r -> HalfPlane r
above = HalfSpace Positive . NonVertical

leftOf :: r -> HalfPlane r
leftOf = HalfSpace Negative . VerticalLineThrough

rightOf :: r -> HalfPlane r
rightOf = HalfSpace Positive . VerticalLineThrough



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

generateInput :: IO (NonEmpty (HalfPlane R))
generateInput = generate arbitrary

onlyNegatives :: IO (NonEmpty (HalfPlane R))
onlyNegatives = generateInput <&> fmap (\h -> h&halfSpaceSign .~ Negative)

myIpeTest       :: NonEmpty (HalfPlane R) -> [IpeObject R]
myIpeTest input = [ iO $ draw $ commonIntersection input
                  ] <>
                  [ iO $ asConstraint h
                  | h <- F.toList input
                  ]

asConstraint    :: forall r. (Fractional r, Ord r)
                => IpeOut (HalfPlane r) Group r
asConstraint h = ipeGroup [ iO $ defIO seg
                          , iO $ ipeSimplePolygon poly ! attr SFill gray
                          ]
  where
    l = h^.boundingHyperPlane
    n = normalVector l
    poly = fromMaybe (error "asConstraint: absurd")
         . fromPoints @(SimplePolygon (Point 2 r))
         . NonEmpty.fromList $ [ seg^.start, seg^.end, seg'^.end, seg'^.start ]

    seg' = translateBy n seg

    box :: Rectangle (Point 2 r)
    box = Box (Point2 (-1000) (-1000)) (Point2 1000 1000)
    -- the line segment we will d raw to represent the bounding line
    seg = case l `intersect` box of
            Nothing                             -> undefined
            Just (Line_x_Box_Point _)           -> undefined
            Just (Line_x_Box_LineSegment seg'') -> seg''



draw :: IpeOut (CommonIntersection (HalfPlane r) r) Group r
draw = \case
  EmptyIntersection     -> ipeGroup []
  SingletonPoint _p _hs   -> ipeGroup []
  InSubLine _l _hs _sl     -> ipeGroup []
  Slab _hl _hr            -> ipeGroup []
  BoundedRegion _pg      -> ipeGroup []
  UnboundedRegion _chain -> ipeGroup []
