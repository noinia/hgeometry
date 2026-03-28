{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module HalfPlane.CommonIntersectionSpec(spec) where

import           Data.Bifoldable
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
import           HGeometry.PolyLine
import           HGeometry.LineSegment
import           R
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           HGeometry.Sequence.Alternating
import           HGeometry.Transformation
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck (arbitrary, generate)
import           Test.QuickCheck.Instances ()
import           Data.Sequence (Seq)
import           HGeometry.Ext
import           Data.These

-- import Debug.Trace

--------------------------------------------------------------------------------

type HalfPlane r = HalfSpaceF (VerticalOrLineEQ r)

spec :: Spec
spec = describe "common halfplane intersection tests" $ do
         it "myHalfplanes test" $
           commonIntersection myHalfPlanes `shouldBe` Just theAnswer
         it "myHalfplanes2 test" $
           commonIntersection myHalfPlanes2 `shouldBe` Just theAnswer2
         -- generateGoldenSpec [osp|commonIntersectionAllNegatives1|]
         -- generateGoldenSpec [osp|commonIntersectionAllNegatives|]
         describe "clipUpperByLower manual" $ do
           let (uppers, lowers, res,answer) = testClip
           it "verify" $ res `shouldBe` answer
           ipeClipResult [osp|commonIntersectionClipManual|] uppers lowers
         -- generateClipGolden [osp|commonIntersectionClip|]


generateGoldenSpec theName = do
    halfPlanes <- runIO onlyNegatives
    goldenWith [osp|data/test-with-ipe/golden/|]
                    (ipeContentGolden { name = theName  })
                    ( myIpeTest halfPlanes)

generateClipGolden theName = do
    uppers <- runIO onlyNegatives
    lowers <- runIO onlyPositives
    ipeClipResult theName uppers lowers



--------------------------------------------------------------------------------
-- for now;
-- type IpeAttr r = Attributes' r


-- class IpeDraw geom where
--   -- | Draws some geometry
--   ipeDraw :: (NumType geom ~ r) => [IpeAttr r] -> geom -> [IpeObject r]
--   -- ipedraw ats g = undefined


-- drawHalfPlane       :: [IpeAttr r] -> halfPlane -> [IpeObject r]
-- drawHalfPlane ats g = [ iO ipeHalfPlane

--                       ]

  -- ipeHalfPlane

--   undefined

-- drawUpper ::

--------------------------------------------------------------------------------




ipeClipResult theName uppers lowers = case boundaries' $ These uppers lowers of
  These myUpper myLower -> do
    let result = clipUpperByLower myUpper myLower
    goldenWith [osp|data/test-with-ipe/golden/|]
                    (ipeContentGolden { name = theName  })
                    [ iO $ defIO myUpper ! attr SStroke green
                    , iO $ defIO myLower ! attr SStroke blue
                    , iO $ defIO result  ! attr SStroke purple
                    , iO $ ipeGroup [ iO $ asConstraint lightgreen h
                                    | h <- F.toList uppers
                                    ]  ! attr SLayer "uppers"
                    , iO $ ipeGroup [ iO $ asConstraint lightblue h
                                    | h <- F.toList lowers
                                    ]  ! attr SLayer "lowers"
                    ]
  _ -> error "ipeClipresult: absurd?"

instance ( Foldable f
         , Fractional r, Ord r
         ) => HasDefaultIpeOut (Chain f r halfPlane) where
  type DefaultIpeOut (Chain f r halfPlane) = Path
  defIO  = defIO . toPolyLineIn defaultBox





toPolyLineIn           :: Foldable f
                       => Rectangle (Point 2 r)
                       -> Chain f r halfPlane -> PolyLine (Point 2 r)
toPolyLineIn _box (Chain c) =
  polyLineFromPoints . NonEmpty.fromList . bifoldMap (:[]) (const []) $ c
  -- TODO: use the box somehow


asLineEQ :: Show r => VerticalOrLineEQ r -> LineEQ r
asLineEQ = \case
  NonVertical              l -> l
  VerticalLineThrough x    -> error $ "vertical line at " <> show x  <> " !?"


boundaries' = let f = fmap (\h -> asLineEQ (h^.boundingHyperPlane) :+ h)
              in boundaries . bimap f f

testClip :: ( NonEmpty _, NonEmpty _
            , Chain Seq R (LineEQ R :+ HalfPlane R)
            , Chain Seq R (LineEQ R :+ HalfPlane R))
testClip = (uppers, lowers, clipUpperByLower myUpper myLower, answer)
  where
    These myUpper myLower = boundaries' $ These uppers lowers
    These answer _ = boundaries' $ These answer' lowers
    uppers = NonEmpty.fromList
              [ below $ LineEQ 5 (-20)
              , below $ LineEQ 1    1
              , below $ LineEQ (-1) 2
              , below $ LineEQ (-5) 20
              ]
    lowers = NonEmpty.fromList
              [ above $ LineEQ 1      (-50)
              , above $ LineEQ 0      (-22)
              , above $ LineEQ (-0.5) (-12)
              ]
    answer' = NonEmpty.fromList
              [ below $ LineEQ (-5) 20
              , below $ LineEQ (-1) 2
              , below $ LineEQ 5 (-20)
              ]

myHalfPlanes :: NonEmpty (HalfPlane R)
myHalfPlanes = NonEmpty.fromList
               [ below $ LineEQ 1    1
               , below $ LineEQ (-1) 2
               , leftOf 10
               ]

theAnswer :: CommonIntersection (HalfPlane R) R
theAnswer = UnboundedRegion . Chain
          $ Alternating (NonEmpty.head myHalfPlanes)
                        (Seq.fromList [ (Point2 (1/2) (3/2), myHalfPlanes NonEmpty.!! 1)
                                      , (Point2 10    (-8),  myHalfPlanes NonEmpty.!! 2)
                                      ]
                        )



myHalfPlanes2 :: NonEmpty (HalfPlane R)
myHalfPlanes2 = NonEmpty.fromList
                [ above $ LineEQ 1    1
                , above $ LineEQ (-1) 2
                , rightOf (-2)
                ]

theAnswer2 :: CommonIntersection (HalfPlane R) R
theAnswer2 = UnboundedRegion . Chain
          $ Alternating (myHalfPlanes2 NonEmpty.!! 2)
                        (Seq.fromList [ (Point2 (-2) 4,      myHalfPlanes2 NonEmpty.!! 1)
                                      , (Point2 (1/2) (3/2), NonEmpty.head myHalfPlanes2)
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

onlyPositives :: IO (NonEmpty (HalfPlane R))
onlyPositives = generateInput <&> fmap (\h -> h&halfSpaceSign .~ Positive)

myIpeTest       :: NonEmpty (HalfPlane R) -> [IpeObject R]
myIpeTest input = [ iO $ maybe (ipeGroup []) draw $ commonIntersection input
                  ] <>
                  [ iO $ asConstraint gray h
                  | h <- F.toList input
                  ]

asConstraint         :: forall r. (Fractional r, Ord r)
                     => IpeColor r -> IpeOut (HalfPlane r) Group r
asConstraint color h = ipeGroup [ iO $ defIO seg
                                , iO $ ipeSimplePolygon poly ! attr SFill color
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
  SingletonPoint _p _hs  -> ipeGroup []
  InSubLine _l _hs _sl   -> ipeGroup []
  Slab _hl _hr           -> ipeGroup []
  BoundedRegion _pg      -> ipeGroup []
  UnboundedRegion _chain -> ipeGroup []
