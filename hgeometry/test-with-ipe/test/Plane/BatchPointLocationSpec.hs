{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Plane.BatchPointLocationSpec where

import Data.Set qualified as Set
import Data.Foldable1
import Data.Default
import Plane.BatchedPointLocation
import Test.Hspec
import HGeometry.Kernel
import Ipe
import System.OsPath
import Control.Lens
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph.Connected
import Test.Hspec.WithTempFile
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Foldable
import Golden
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Line.BatchPointLocation qualified as Line
import PlaneGraph.PolygonOverlaySpec
import PlaneGraph.RenderSpec
import Data.Map.NonEmpty qualified as NEMap
import Prelude hiding (lines)


--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Plane.BatchedPointlocation" $ do
         goldenWith [osp|data/test-with-ipe/golden/Plane/|]
           (ipeFileGolden { name      = [osp|batchpointlocate|] }
           )
           ( let myLines'     = (iO . defIO) <$> myLines
                 queryPoints' = (iO . defIO) <$> queryPoints
                 answers      = Line.groupQueries queryPoints myLines
                 lr pts       = (iO . defIO) <$> pts
                 answers'     = [ iO $ ipeGroup (lr group) ! attr SLayer (LayerName (Text.show i))
                                | (i, group) <- zip [0..] (toList answers)
                                ]
                 subdiv = Line.buildPointLocationStructure queryPoints myLines

                 gr :: CPlaneGraph () (Point 2 R) (ClosedLineSegment (Point 2 R) :+ Maybe (VerticalOrLineEQ R)) String
                 gr = (subdiv^.Line.subdivision) & faces %@~ \i _ -> show i
                 faces' = ifoldMapOf interiorFacePolygons (drawFace gr) gr
                 content' = transformBy t $
                   [ iO $ ipeGroup myLines'     ! attr SLayer "lines"
                   , iO $ ipeGroup queryPoints' ! attr SLayer "queries"
                   , iO $ ipeGroup (renderGraph (renderSubdiv subdiv)) ! attr SLayer "subdiv"
                   ] <> faces' <> answers'
                 t    = uniformScaling 10
             in addStyleSheet opacitiesStyle $ singlePageFromContent content'
           )
         it "manual; same as naive" $
             let toSet = foldMap Set.singleton
             in (toSet <$> batchedPointLocation myQueryPoints myPlanes)
                `shouldBe`
                naivePlanesAbove myQueryPoints myPlanes


         -- prop "show /read for plane'" $
         --   \(h :: Plane' Int)  -> (read (show h) :: Plane' Int) === h

         -- prop "same as naive" $
         --   \(Input queries planes) ->
         --     let toSet = foldMap Set.singleton
         --     in (toSet <$> batchedPointLocation queries planes)
         --        ===
         --        naivePlanesAbove queries planes


-- | fit to something that fits in this rectangle
screenBox :: Rectangle (Point 2 R)
screenBox = Rectangle origin (Point2 500 500)


myPlanes :: [MyPlane]
myPlanes = MyPlane <$>
           [ Plane 1 0 5
           , Plane 0 1 20
           , Plane 2 2 3
           ]

myQueryPoints :: NonEmpty (Point 3 R)
myQueryPoints = NonEmpty.fromList
              [
              --   origin
              -- , Point3 1 8 5
               Point3 45 0 10
              , Point3 10 4 20
              ]


myLines :: [VerticalOrLineEQ R]
myLines = [ NonVertical $ LineEQ 0 2
          , NonVertical $ LineEQ 1 3
          , NonVertical $ LineEQ (-1) 6
          -- , VerticalLineThrough 5
          ]

queryPoints :: NonEmpty (Point 2 R)
queryPoints = NonEmpty.fromList
              [ origin
              , Point2 1 8
              , Point2 (-1) 0
              , Point2 10 4
              , Point2 (-30) (-23)
              ]


--------------------------------------------------------------------------------

renderSubdiv    :: forall r line.
                     Line.PointLocationDS' r line
                  -> CPlaneGraph () (Point 2 r)
                                    (Maybe (IpeAttributes Path R))
                                    (Maybe (IpeAttributes Path R))
renderSubdiv ds = gr1&faces ?~ def
  where
    gr = ds^.Line.subdivision
    gr1 :: CPlaneGraph () (Point 2 r)
                          (Maybe (IpeAttributes Path R))
                          ()
    gr1 = gr&edges ?~ def
      -- \(E defs covering) -> do
      --                     _ :+ (_ :+ (z,props)) <- minimumOn (^.extra.extra._1) defs
      --                     _ :+ (zCovering,_)    <- minimumOn (^.extra._1) covering
      --                     if z <= zCovering then props^.edgeAttrs
      --                                       else Nothing



--------------------------------------------------------------------------------

-- groupQueries'               :: ( Point_ queryPoint 2 r
--                                , Line_ line 2 r
--                                , Foldable set
--                                , Ord r, Fractional r


--                                , Show line, Show r -- FIXME: Remoe theese
--                                , Show queryPoint

--                                , Eq line
--                                , IsBoxable queryPoint

--                               , IsIntersectableWith line (Rectangle (Point 2 r))
--                               , Intersection line (Rectangle (Point 2 r)) ~
--                                 Maybe (LineBoxIntersection 2 r)
--                               )
--                             => NonEmpty queryPoint
--                             -> set line
--                             -> NonEmpty (NonEmpty queryPoint)
-- groupQueries' queries lines = toNonEmpty $ Line.groupQueries queries lines

-- -- | Given a set of \(n\) query points, and a set of \(r\) lines H computes for each
-- -- query q the subset of lines above q, in increasing order.
-- --
-- -- running time: \(O(n\log n + r^5\log r)\)
-- naiveGroupQueries               :: ( Point_ queryPoint 2 r
--                               , Line_ line 2 r
--                               , Foldable set
--                               , Ord r, Fractional r


--                               , Show line, Show r -- FIXME: Remoe theese
--                               , Show queryPoint

--                               , Eq line
--                               , IsBoxable queryPoint

--                               , IsIntersectableWith line (Rectangle (Point 2 r))
--                               , Intersection line (Rectangle (Point 2 r)) ~
--                                 Maybe (LineBoxIntersection 2 r)
--                               )
--                            => NonEmpty queryPoint
--                            -> set line
--                            -> NonEmpty (NonEmpty queryPoint)

--                            MonoidalNEMap (FaceIx (PointLocationDS' r line)) (NonEmpty queryPoint)
-- naiveGroupQueries queries lines = foldMap1 (\q -> singleton (pointLocate q pointLocDS)
--                                                             (NonEmpty.singleton q)
--                                            ) queries
--   where
--     pointLocDS = buildPointLocationStructure queries lines


naivePlanesAbove                :: forall queryPoint r plane set.
                                   ( Point_ queryPoint 3 r
                                   , Plane_ plane r
                                   , Foldable set
                                   , Ord r, Fractional r
                                   , Ord plane
                                   , Ord queryPoint

                                   , Show queryPoint, Show r --FIXME: remove
                                   )
                                => NonEmpty queryPoint
                                -> set plane
                                -> NEMap.NEMap queryPoint (Set.Set plane)
naivePlanesAbove queries planes =
    foldMap1 (\q -> NEMap.singleton q (Set.filter (q `liesBelow'`) planes')) queries
  where
    liesBelow'       :: queryPoint -> plane -> Bool
    q `liesBelow'` h = verticalSideTest q h /= GT
    planes' = foldMap Set.singleton planes



--------------------------------------------------------------------------------
squareSize :: R
squareSize = 100


data Input = Input (NonEmpty QueryPoint)
                   (NonEmpty MyPlane)
             deriving (Show,Eq)

instance Arbitrary Input where
  arbitrary = Input <$> (NonEmpty.singleton <$> arbitrary)
                    <*> arbitrary
  shrink (Input qs hs) = concat [ Input qs <$> shrink hs
                                , [Input qs' hs | qs' <- shrink qs ]
                                ]
    -- shrink the planes first


newtype QueryPoint = Query (Point 3 R)
  deriving newtype (Eq,Ord)
  deriving stock (Show,Read)

type instance NumType   QueryPoint = NumType   (Point 3 R)
type instance Dimension QueryPoint = Dimension (Point 3 R)
instance HasVector QueryPoint QueryPoint where
  vector = coerced
instance Affine_ QueryPoint 3 R
instance HasCoordinates QueryPoint QueryPoint
instance Point_ QueryPoint 3 R



instance Arbitrary QueryPoint where
  arbitrary = Query <$> (Point3 <$> choose (0,squareSize)
                                <*> choose (0,squareSize)
                                <*> choose (0,squareSize)
                        )
  shrink (Query (Point3 x y z)) = concat [
        [ Query (Point3 x' y z) | x' <- shrink x ]
     ,  [ Query (Point3 x y' z) | y' <- shrink y ]
     ,  [ Query (Point3 x y z') | z' <- shrink z ]
     ]

newtype MyPlane = MyPlane (Plane R)
  deriving newtype (Eq,Ord)
  deriving stock (Show,Read)

type instance NumType   MyPlane = NumType   (Plane R)
type instance Dimension MyPlane = Dimension (Plane R)
instance HyperPlane_ MyPlane 3 R
instance NonVerticalHyperPlane_ MyPlane 3 R where
  hyperPlaneCoefficients = coerced @_ @_ @(Plane R) . hyperPlaneCoefficients
-- instance HyperPlaneFromPoints MyPlane where
--   hyperPlaneThrough = MyPlane . hyperPlaneThrough

instance Arbitrary MyPlane where
  arbitrary = MyPlane <$> (Plane <$> choose (0,10)
                                 <*> choose (0,10)
                                 <*> choose (1,squareSize)
                          )
  -- TODO; not sure about this instance yet
  shrink (MyPlane (Plane a b c)) = concat
    [ [ MyPlane (Plane a' b  c) | a' <- shrink a ]
    , [ MyPlane (Plane a  b' c) | b' <- shrink b ]
    , [ MyPlane (Plane a  b  c') | c' <- shrink c ]
    ]



-- data Plane' r = Plane' r r r
--   deriving (Eq)

-- instance Arbitrary r => Arbitrary (Plane' r) where
--   arbitrary = Plane' <$> arbitrary <*> arbitrary <*> arbitrary
