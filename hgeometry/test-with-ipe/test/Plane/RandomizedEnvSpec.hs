{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Plane.RandomizedEnvSpec
  where

import           HGeometry.Polygon.Convex.Internal (verifyConvex)
import           GHC.Generics (Generic)
import           Control.Monad
import           HGeometry.Polygon.Simple.Sample
import           Prelude hiding (zipWith)
import           Data.Zip
import           HGeometry.LineSegment
import           HGeometry.HalfLine
import           Data.Maybe
import           Golden
import           Data.Bifunctor
import           Data.Foldable1
import qualified Data.Set as Set
import           Data.Ord
import           Control.Lens hiding (Prism)
import           System.OsPath
import           Ipe
import           System.Random
import           Data.List (sort)
import           Data.Foldable (Foldable(..))
import           HGeometry.Kernel
import           Plane.BruteForce
import           Plane.Sample
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as Original
import           R
import           Plane.LowerEnvelopeSpec (MyPlane(..))
import Data.Set.NonEmpty qualified as NESet
import           Data.Set (Set)
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import           Data.Map.Monoidal (MonoidalMap)
import qualified Plane.Randomized2 as Randomized
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import           HGeometry.Ext
import           HGeometry.Polygon
import           HGeometry.Cone
import           HGeometry.Point.Either
import           Ipe.Color
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Intersection
import           HGeometry.Ipe.Instances
import           Ipe.AllColors
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           Data.Text (Text)
import           Debug.Pretty.Simple
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import           Debug.Trace
import           Ipe.Draw
import           Test.Util
import           Data.Traversable
import           System.Random.Stateful

--------------------------------------------------------------------------------

-- newtype InputPlanes plane = InputPlanes (NESet.NESet plane)
--                           deriving (Show,Eq)

-- instance Arbtirary (InputPlanes MyPlane) where
--   arbitrary = do coeffs <-


    -- arbitrary >>= go (mempty,mempty,mempty)
    -- where
    --   go   :: (Set r, Set r, Set r) -> Int -> Gen (InputPlanes plane)
    --   go n =



instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

instance Arbitrary (IpeColor R) where
  arbitrary = Test.QuickCheck.elements basicNamedColors

----------------------------------------

-- | Generate a triangular domain, and a non-empty list of points
-- strictly inside the domain.
data Queries = Queries (Triangle (Point 2 R)) (NonEmpty (Point 2 R))
             deriving (Show,Eq,Generic)

instance Arbitrary Queries where
  arbitrary = do domain   <- arbitrary
                 queries' <- scale (*100) $
                             fmap NonEmpty.fromList . listOf1 $
                               arbitrary `suchThat` all (> 0)
                 let queries = barrycentric domain <$> queries'
                 pure $ Queries domain queries
  -- shrink = const []
  -- shrink = genericShrink
  shrink (Queries tri qs) = [ Queries tri qs'
                            | qs' <- shrink qs
                            , all (`intersects` tri) qs'
                            ]

-- | Given a triangle and a vector of coefficients, use it to produce a point inside
-- the triangle
barrycentric :: Triangle (Point 2 R) -> Vector 3 R -> Point 2 R
barrycentric (Triangle (Point a) (Point b) (Point c)) (normalize -> Vector3 x y z) =
    Point $ (x *^ a) ^+^ (y *^ b) ^+^ (z *^ c)

-- | Normalize the vector w.r.t the sum of the coefficients.
normalize   :: Vector 3 R -> Vector 3 R
normalize v = let s = sum v in (/s) <$> v


-- | Make sure that we indeed generate points inside the triangle.
testBarrycentric :: Spec
testBarrycentric = prop "test barrycentric" $
                     \(Queries t pts) -> all (`intersects` t) pts

--------------------------------------------------------------------------------

-- | I don't think I really want this one; but just for debugging purposes it seems ok
type instance NumType (a,b) = NumType b
type instance NumType (a,b,c) = NumType c
type instance NumType (a,b,c,d) = NumType d

--------------------------------------------------------------------------------
-- Move to Ipe.Draw

--------------------------------------------------------------------------------

instance (Point_ apex 2 r, Fractional r, Ord r, Show r
         ) => IsDrawable (Ipe r) (Cone r apex edge) where
  type AttrOf (Ipe r) (Cone r apex edge) = PathAttributes r
  draw _ats c = [iO $ defIO c]

instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , NumType a ~ r, NumType b ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         ) => IsDrawable (Ipe r) (a,b) where
  type AttrOf (Ipe r) (a,b) = CommonAttributes r Maybe
  draw ats (a,b) = draw @(Ipe r) [ commonAttributes %~ apply ats ] a
                <> draw @(Ipe r) [ commonAttributes %~ apply ats ] b


instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , IsDrawable (Ipe r) c
         , NumType a ~ r, NumType b ~ r, NumType c ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) c) r Maybe
         ) => IsDrawable (Ipe r) (a,b,c) where
  type AttrOf (Ipe r) (a,b,c) = CommonAttributes r Maybe
  draw ats (a,b,c) = draw @(Ipe r) [ commonAttributes %~ apply ats ] a
                  <> draw @(Ipe r) [ commonAttributes %~ apply ats ] b
                  <> draw @(Ipe r) [ commonAttributes %~ apply ats ] c

instance ( IsDrawable (Ipe r) a
         , IsDrawable (Ipe r) b
         , IsDrawable (Ipe r) c
         , IsDrawable (Ipe r) d
         , NumType a ~ r, NumType b ~ r, NumType c ~ r, NumType c ~ r
         , HasCommonAttributes (AttrOf (Ipe r) a) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) b) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) c) r Maybe
         , HasCommonAttributes (AttrOf (Ipe r) d) r Maybe
         ) => IsDrawable (Ipe r) (a,b,c,d) where
  type AttrOf (Ipe r) (a,b,c,d) = CommonAttributes r Maybe
  draw ats (a,b,c,d) = mconcat
      [ draw @(Ipe r) [ commonAttributes %~ apply ats ] a
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] b
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] c
      , draw @(Ipe r) [ commonAttributes %~ apply ats ] d
      ]


instance (Point_ point 2 r, Fractional r, Ord r, Show r, Show point
         ) => IsDrawable (Ipe r) (HalfLine point) where
  type AttrOf (Ipe r) (HalfLine point) = PathAttributes r
  draw _ats hl = [iO $ defIO hl]

-- | Helper function to apply attributes
apply       :: [at -> at] -> at -> at
apply ats a = foldl' (flip ($)) a ats


--------------------------------------------------------------------------------

-- more numtype instances just for debugging/testing purposes

type instance NumType (NESet.NESet a) = NumType a
type instance NumType (MonoidalMap.MonoidalMap k a) = NumType a

----------------------------------------


instance IsDrawable backend g => IsDrawable backend (NESet.NESet g) where
  type AttrOf backend (NESet.NESet g) = AttrOf backend g
  draw ats = foldMap (draw @backend ats)

instance IsDrawable backend g => IsDrawable backend (MonoidalMap.MonoidalMap k g) where
  -- ^ Draws the values; not the keys
  type AttrOf backend (MonoidalMap.MonoidalMap k g) = AttrOf backend g
  draw ats = foldMap (draw @backend ats)



instance IsDrawable (Ipe R) MyPoint where
  type AttrOf (Ipe R) MyPoint = AttrOf (Ipe R) (Point 2 R)
  draw ats (p :+ c) = draw @(Ipe R) ((stroke ?~ c) : ats) p


--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- * Move to Kernel ; Cone x Triangle intersection stuff ?

testX :: Spec
testX = it "cone triangle boundary intersections" $
  let testT :: Triangle (Point 2 R)
      testT =  Triangle (Point2 1 0) (Point2 1 1) (Point2 0 0)

      testC :: Cone R (Point 2 R) ()
      testC = Cone (Point2 1 0) (Vector2 1 1 :+ ()) (Vector2 0 (-1) :+ ())
  in coneTriangleBoundaryIntersections testC testT
     `shouldBe`
     [Point2 1 0,Point2 1 0,Point2 1 0,Point2 1 0]


-- | Computes the intersection points of the boundary of a cone with
-- the boundary of a triangle.
--
-- this may contain duplicates.
coneTriangleBoundaryIntersections :: forall apex corner r edge.
                                     ( Point_ apex 2 r, Point_ corner 2 r
                                     , Ord r, Fractional r
                                     )
                                  => Cone r apex edge -> Triangle corner
                                  -> [Point 2 r]
coneTriangleBoundaryIntersections cone (fmap (^.asPoint) -> Triangle a b c) =
    intersectionPoints (leftBoundary cone) <> intersectionPoints (rightBoundary cone)
  where
    intersectionPoints (HalfLine o v :+ _) =
      flip foldMap sides $ \side -> case HalfLine (o^.asPoint) v `intersect` side of
        Nothing                                       -> []
        Just (HalfLine_x_LineSegment_Point p)         -> [p]
        Just (HalfLine_x_LineSegment_LineSegment seg) -> [seg^.start,seg^.end]

    sides = [ ClosedLineSegment a b
            , ClosedLineSegment b c
            , ClosedLineSegment c a
            ]


--------------------------------------------------------------------------------

-- | Generates arbitary points in a triangle
arbitraryPointsInTriangle   :: Triangle (Point 2 R) -> Gen [Point 2 R]
arbitraryPointsInTriangle t = do seed <- arbitrary
                                 n    <- arbitrary
                                 let gen = mkStdGen seed
                                     t'  :: Triangle (Point 2 Double)
                                     t'  = t&vertices.coordinates %~ realToFrac

                                     sample g = over coordinates realToFrac
                                             <$> sampleFromTriangle t' g
                                 pure $ runStateGen_ gen $ replicateM n . sample

-- | Generate arbitrary pints in the intersection of a triangle and a an other region.
arbitraryPointInIntersection             :: Point 2 R `HasIntersectionWith` region
                                         => Triangle (Point 2 R) -> region -> Gen [Point 2 R]
arbitraryPointInIntersection domain cone =
  filter (`intersects` cone) <$>  arbitraryPointsInTriangle domain

--------------------------------------------------------------------------------

-- | A clipped cone data type (represented by the two bounding arys)
data ClippedCone apex = ClippedCone { _leftBoundary'  :: HalfLine apex
                                    , _rightBoundary' :: HalfLine apex
                                    }

deriving instance Show (HalfLine apex) => Show (ClippedCone apex)
deriving instance Eq   (HalfLine apex) => Eq   (ClippedCone apex)

instance ( Arbitrary apex, Point_ apex 2 r, Num r, Ord r, Arbitrary r
         ) => Arbitrary (ClippedCone apex) where
  arbitrary = do (cone :: Cone r apex ()) <- arbitrary
                 lambdas                  <- arbitrary `suchThat` (all (> 0))
                 pure $ fromConeAndShifts cone lambdas

-- | Construct a clipped cone from a given cone and some offsets w.r.t the apex.
fromConeAndShifts              :: ( Num r, Point_ apex 2 r)
                               => Cone r apex edge -> Vector 2 r -> ClippedCone apex
fromConeAndShifts cone lambdas = ClippedCone leftRay rightRay
  where
    shift lambda v = HalfLine ((cone^.apex) .+^ (lambda *^ v)) v
    Vector2 leftRay rightRay = zipWith shift lambdas
                             $ Vector2 (cone^.leftBoundaryVector.core)
                                       (cone^.rightBoundaryVector.core)

instance ( Point_ point 2 r, Num r, Ord r
         ) => Point 2 r `HasIntersectionWith` ClippedCone point where
  q `intersects` (ClippedCone (HalfLine al vl) (HalfLine ar vr)) =
    all (q `intersects`) [ rightHalfPlane (LinePV (al^.asPoint) vl) -- right of the left boundary
                         , leftHalfPlane  (LinePV (ar^.asPoint) vr) -- left of the right boundary
                         , leftHalfPlane  (LinePV (al^.asPoint) (ar .-. al))
                         ]
  {-# INLINE intersects #-}

--------------------------------------------------------------------------------

-- | Helper data type to generate tests for our cone and clipped cone cover tests
data ConeInput' cone = ConeInput { _domain  :: Triangle (Point 2 R)
                                 , _cone     :: cone
                                 , _pointsIn :: [Point 2 R]
                                 } deriving (Show,Eq,Functor,Generic)

type ConeInput        = ConeInput' (Cone R (Point 2 R) ())
type ClippedConeInput = ConeInput' (ClippedCone (Point 2 R))

instance ( Arbitrary cone
         , Point 2 R `HasIntersectionWith` cone
         ) => Arbitrary (ConeInput' cone) where
  arbitrary = do domain <- arbitrary
                 cone   <- arbitrary
                 pts    <- resize 300 $ arbitraryPointInIntersection domain cone
                 pure $ ConeInput domain cone pts
  shrink = genericShrink

coneCovers :: Spec
coneCovers = describe "Cone Covers" $ do
    prop "cone cover contains domain" $
      \(ConeInput domain (cone :: Cone R (Point 2 R) ()) pts) ->
        let corners'      = filter (`intersects` cone) (toList domain)
            intersections = coneTriangleBoundaryIntersections cone domain
        in ipeCounterExample (domain,cone,corners',intersections) $
           case coverCone domain cone of
             Nothing
               | null pts  -> discard -- don't test with empty pts
               | otherwise -> counterexample (show pts) $ property False
             Just poly -> counterexample (show
                                           (poly
                                           )
                                         ) $
                          ipeCounterExample ( poly
                                            ) $
                          conjoin [ counterexample (show v) $ Every $ v `intersects` poly
                                  | v <- intersections ++ corners' ++ pts
                                  ]

    prop "clipped cone cover contains domain" $
      \(ConeInput domain (clippedCone :: ClippedCone (Point 2 R)) pts) ->
        let (ClippedCone leftRay rightRay) = clippedCone
            corners'      = filter (`intersects` clippedCone) (toList domain)
            -- intersections = filter (`intersects` clippedCone) $
            --                 coneTriangleBoundaryIntersections (definingCone cone) domain
         in ipeCounterExample (domain,corners') $
            case coverClippedCone domain leftRay rightRay of
              Nothing
                | null pts  -> discard -- don't test with empty clipped cones
                | otherwise -> counterexample (show pts) $ property False
              Just poly -> counterexample (show
                                            (poly, leftRay, rightRay)
                                          ) $
                           ipeCounterExample ( poly
                                             , leftRay
                                             , rightRay
                                             ) $
                           conjoin [ counterexample (show v) $ Every $ v `intersects` poly
                                   | v <- corners' ++ pts
                                   ]


--------------------------------------------------------------------------------

spec :: Spec
spec = describe "RandomizedEnvSpec" $ do
         testX
         testBug
         coneCovers
         findMissingEdgeTest
         lowest
         verifyCellProperties
         testBarrycentric
         -- voronoiSpec -- FIXME: Enable this again

         modifyMaxSize (const 60) $ do
           prop "new brute force same as original" $
             \(planes :: NESet.NESet MyPlane) ->
               verticesOf (bruteForceVertices planes)
               ===
               Map.keys (Original.computeVertexForm planes)

           -- prop "prisms are interiorly disjoint" $
           --   \(planes :: NESet.NESet MyPlane) ->
           --     let input  = Sample (toList planes) (length planes) [] (length planes)
           --         env    = bruteForceTriangulatedEnvelope input
           --         prisms = toList $ foldMap (^.core) env
           --     in
           --       mconcat [ counterexample (show (a,b)) $ interiorlyDisjoint a b
           --               | a <- prisms, b <- prisms, a /= b
           --               ]

           --       show () === "foo"

           prop "brute force vornoi diagram; sites contained in voronoi regions" $
             \(sites' :: NESet.NESet (Point 2 R)) (Queries domain _) ->
               let sites = assignColors sites'
                   vd    = voronoiDiagramIn domain (toNonEmpty sites)
                   p `implies` q = not p || q
                   -- we sdhould not use the ==> in the interior;
                   -- i.e. we don't necessarily want only point sets
                   -- that all are inside the domain.
               in not (null vd) ==>
                    ipeCounterExample (domain, sites, vd) $
                    counterexample (show vd) $
                    conjoin [ (s `intersects` domain) `implies`
                              (s `intersects` cell)
                            | (s,cell) <- MonoidalMap.assocs vd
                            ]

         modifyMaxSize (const 13) $ do
           prop "brute force envelope; indeed lowest at query points" $
             \(planes :: NESet.NESet MyPlane) (Queries domain queries) ->
               let env   = lowerEnvelopeOn domain planes
               in counterexample (show env) $
                  ipeCounterExample (queries, domain, toList env) $
                    not (null env) ==>
                      conjoin [ verifyLowestEnv (toNonEmpty planes) q env
                              | q <- toList queries
                              ]

           xprop "brute force vornoi diagram; covers all points" $
             \(sites' :: NESet.NESet (Point 2 R)) (Queries domain queries) ->
               let sites = assignColors sites'
                   vd = voronoiDiagramIn domain (toNonEmpty sites)
                   verifyClosest sites q vd =
                     let ss  = closestAt q vd
                         ss' = closestAt' q sites
                     in ss === ss'
               in not (null vd) ==>
                    ipeCounterExample (queries, domain, sites, vd) $
                    counterexample (show vd) $
                    conjoin [ verifyClosest sites q vd
                            | q <- toList queries
                            ]


           lowest
           xprop "randomized2 same as (new) brute force" $
             \(planes :: NESet.NESet MyPlane)
              (domain :: Triangle (Point 2 R)) (gen :: StdGen) ->
               verticesOf (Randomized.verticesIn gen domain planes)
               ===
               verticesOf (bruteForceVerticesIn domain planes)


--------------------------------------------------------------------------------


-- | Generate at least three planes
newtype MyPlanes = MyPlanes (NESet.NESet MyPlane)
                 deriving (Show,Eq)

instance Arbitrary MyPlanes where
  arbitrary = MyPlanes <$> arbitrary `suchThat` ((>= 3) . length)


verifyCellProperties :: Spec
verifyCellProperties = describe "verifying cell properties" $ do
  prop "cells convex" $
    \(MyPlanes planes) (domain :: Triangle (Point 2 R)) ->
      let env = lowerEnvelopeOn domain planes
      in not (null env) ==>
           ipeCounterExample env $
           ifoldMap (\h cell -> Every $
                                counterexample (show (h,cell)) $
                                ipeCounterExample (cell, domain) $
                                verifyConvex cell

                    ) env


findMissingEdgeTest :: Spec
findMissingEdgeTest = it "find missing edge" $
                      findMissingEdge (\u v -> u > v) 0 (NonEmpty.fromList [1..5])
                      `shouldBe`
                      Just (0,[1..4],5)

lowest :: Spec
lowest = prop "brute force triangulated envelope; indeed lowest at query points" $
             \(MyPlanes planes) (Queries domain queries) ->
               let env = triangulatedLowerEnvelopeOn domain planes
               in if null env then trace ("discarding empty envelope " <> show planes) discard
                              else counterexample (show env) $
                  ipeCounterExample (queries, domain, toList env) $
                    not (null env) ==> conjoin [ verifyLowest (toNonEmpty planes) q env
                                               | q <- toList queries
                                               ]


{-
           prop "dummy" $
             \(planes :: NESet.NESet MyPlane) ->
               let input = Sample (toList planes) (length planes) [] (length planes)
               in show (bruteForceTriangulatedEnvelope input) === "foo"

-}
         -- runIO test

--------------------------------------------------------------------------------

type Cell = ConvexPolygon (Vertex' (EnvVertex R MyPlane) R MyPlane)

-- | Given the planes and a query; verify that the lower envelope is
-- correct at the given query point.
verifyLowestEnv          :: NonEmpty MyPlane -> Point 2 R
                         -> BoundedLowerEnvelope R MyPlane
                         -> Property
verifyLowestEnv hs q = counterexample (show q)
                     . allAtLowest
                     . ifoldMap findContainingCells
  where
    findContainingCells      :: MyPlane -> Cell -> [(MyPlane, Cell)]
    findContainingCells h pg
      | q `intersects` pg = [(h,pg)]
      | otherwise         = []

    allAtLowest = \case
      []   -> Every $ counterexample "No cell containing the query point!"
                    $ counterexample ("lowest should be: " <> show lowestAtQ) False
      tris -> foldMap (\(h,tri) -> Every $ counterexample (show tri) $ isLowestAtQ h) tris

    isLowestAtQ   :: MyPlane -> Every
    isLowestAtQ h = let z = evalAt q h
                    in foldMap (\h' -> Every $
                                 counterexample (show h') $
                                 counterexample (show (z,evalAt q h')) $
                                 z <= evalAt q h'
                               ) hs

    lowestAtQ = minimumBy (comparing $ evalAt q) hs






-- | Verify that the lower envelope is correct at the query point
verifyLowest          :: NonEmpty MyPlane -> Point 2 R
                      -> TriangulatedLowerEnvelope R MyPlane
                      -> Property
verifyLowest hs q = counterexample (show q)
                  . allAtLowest
                  . ifoldMap findContainingPrisms
  where
    findContainingPrisms    :: MyPlane -> NonEmpty (Prism R MyPlane)
                            -> [(MyPlane, Prism R MyPlane)]
    findContainingPrisms h = foldMap $ \tri -> ([(h, tri) | q `intersects` tri])

    allAtLowest = \case
      []   -> Every $ counterexample "No prism containing the query point!"
                    $ counterexample ("lowest should be: " <> show lowestAtQ) False
      tris -> foldMap (\(h,tri) -> Every $ counterexample (show tri) $ isLowestAtQ h) tris


    -- allPrismsCorrect   :: MyPlane -> NonEmpty (Prism R MyPlane :+ extra) -> Every
    -- allPrismsCorrect h = Every . counterexample (show h) . foldMap (prismIsCorrect h)

    -- prismIsCorrect         :: MyPlane -> Prism R MyPlane :+ extra -> Every
    -- prismIsCorrect h (tri :+ _)
    --   | q `intersects` projectPrism tri = Every $ counterexample (show tri) $ isLowestAtQ h
    --   | otherwise                       = mempty

    isLowestAtQ   :: MyPlane -> Every
    isLowestAtQ h = let z = evalAt q h
                    in foldMap (\h' -> Every $
                                 counterexample (show h') $
                                 counterexample (show (z,evalAt q h')) $
                                 z <= evalAt q h'
                               ) hs

    lowestAtQ = minimumBy (comparing $ evalAt q) hs




verticesOf :: (Plane_ plane r, Ord r, Fractional r)
           => Set (EnvVertex r plane) -> [Point 3 r]
verticesOf = sort . map location . toList


-- {-
-- -- | Test whether two prisms are interiorly disjoint
-- interiorlyDisjoint     :: (Plane_ plane r, Fractional r, Ord plane, Ord r)
--                        => Prism r plane -> Prism r plane -> Bool
-- interiorlyDisjoint a b = case projectPrism a `intersect` projectPrism b  of
--   Just (ActualPolygon _) -> False
--   _                      -> True

-- -}
----------------------------------------------------------------------------------
-- * Some helper utils for firuting out whether the input planes are in general position

isInGeneralPosition        :: (Foldable set, Plane_ plane r, Ord plane, Fractional r, Ord r)
                           => set plane -> Bool
isInGeneralPosition planes = and
  [ -- uniqueOn (\(Plane_ a _ _) -> a) planes
  -- , uniqueOn (\(Plane_ _ b _) -> b) planes
  -- , uniqueOn (\(Plane_ _ _ c) -> c) planes
   all (null . view extraDefiners) $ bruteForceVertices planes
  ]


uniqueOn f = snd . foldr (\x (s,b) -> let y = f x in (Set.insert y s, b && Set.notMember y s))
                         (mempty,True)

--------------------------------------------------------------------------------

colors :: [IpeColor R]
colors = cycle (drop 3 basicNamedColors <> toList (namedSet myColors))

myColors :: Map.Map _ (IpeValue (RGB R))
myColors = nonRepeated allColors

mkCone                     :: Num r => apex -> Vector 2 r -> Vector 2 r -> Cone r apex ()
mkCone a incoming outgoing = Cone a (negated incoming :+ ()) (outgoing :+ ())


--------------------------------------------------------------------------------
-- * Debugging things


data Input plane = Input (Triangle (Point 2 R))
                         (NESet.NESet plane)
                         [Point 2 R] -- possible queries
                 deriving (Show,Read)

-- domain = Triangle (Point2 100 100) (Point2 110 100) (Point2 100 110)

-- test :: IO ()
-- test = do
--           writeIpeFile [osp|tri.ipe|]
--             . addStyleSheet (createIpeStyle "myColors" myColors)
--             . addStyleSheet opacitiesStyle
--             . ipeFile . NonEmpty.fromList . fmap (fromContent . concat) $
--               [ [ let testCoverCone :: ConvexPolygon (OriginalOrExtra (Point 2 R) (Point 2 R))
--                       testCoverCone = coverCone domain (Point2 1 3) (Vector2 (-1) (-1)) (Vector2 1 0)
--                   in
--                   [ iO $ defIO (mkCone (Point2 1 (3 :: R)) (Vector2 (-1) (-1)) (Vector2 1 0))
--                   , iO $ defIO domain  &layer ?~  "domain"
--                   , iO $ ipeSimplePolygon testCoverCone &layer  ?~ "result"
--                                                         &fill   ?~ seagreen
--                                                         &stroke ?~ black
--                   ] ]
--               , [ let leftV  = Vector2 1 (-1)
--                       rightV = Vector2 1 2
--                       al     = Point2 5 3
--                       ar     = Point2 8 (3 :: R)
--                       answer :: ConvexPolygon (Point 2 R)
--                       answer = uncheckedFromCCWPoints $ NonEmpty.fromList
--                                [ al .-^ (20 *^ leftV)
--                                , al, ar
--                                , ar .+^ (20 *^ rightV)
--                                ]
--                       result = coverClippedCone domain al leftV ar rightV
--                   in
--                   [ iO $ defIO answer &layer ?~  "clippedCone"
--                                       &fill ?~ blue
--                   , iO $ defIO domain  &layer ?~  "domain"
--                   , iO $ ipeSimplePolygon result
--                           &layer ?~  "result"
--                           &fill ?~ seagreen
--                           &stroke ?~  black
--                   ] ]
--               , [ let al     :: Point 2 R
--                       al     = Point2 1.80000 0.60000
--                       leftV  = Vector2 1 0.33333
--                       ar     = Point2 0.79091 1.60909
--                       rightV = Vector2 (-1) (-2.66666)
--                       answer :: ConvexPolygon (Point 2 R)
--                       answer = uncheckedFromCCWPoints $ NonEmpty.fromList
--                                [ al .-^ (20 *^ leftV)
--                                , al, ar
--                                , ar .+^ (20 *^ rightV)
--                                ]
--                       result = coverClippedCone domain al leftV ar rightV
--                   in
--                   [ iO $ defIO answer &layer ?~  "clippedCone"
--                                       &fill ?~ blue
--                   , iO $ defIO domain  &layer ?~  "domain"
--                   , iO $ ipeSimplePolygon result
--                           &layer ?~  "result"
--                           &fill ?~ seagreen
--                           &stroke ?~  black
--                   ] ]
--               ]
--   where
--     domain :: Triangle (Point 2 R)
--     domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)




-- test2 = runTest $ Input domain planes []
--   where
--     domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)

--     planes :: NESet.NESet (MyPlane :+ IpeColor R)
--     planes = NESet.fromList
--            . NonEmpty.fromList . fmap (over core MyPlane) . flip (zipWith (:+)) colors $
--             -- [ Plane 0    1    0
--             -- , Plane 0    (-1) 0
--             -- , Plane 1    0    2
--             -- , Plane (-1) (1/100)    2
--             -- ]
--              -- [ Plane (-1) 3 1
--              -- , Plane 1.66666 1.66666 (-3)
--              -- , Plane 2.66666 (-1) 0.5
--              -- , Plane 0 0 1
--              -- , Plane (-2) 2 2
--              -- ]

--              -- [ Plane (-15.9) (-2.83334) (-4.16667)
--              -- , Plane (-14.5) 17.57894 (-5.21053)
--              -- ,Plane (-14.23530) 17.6 2.1
--              -- ,Plane (-5) (-11.6) (-16)
--              -- ,Plane 11 (-7.26667) (-7.23077)]

--              [Plane (-17.10527) 14 15.77777, Plane (-4.3) (-12.93334) 0.28571,Plane (-2.42858) (-3.57143) (-9.92858),Plane (-0.27273) (-21.44445) 8.8,Plane 0.625 (-0.875) (-17.18182),Plane 1.2 0.28571 4.4,Plane 1.73333 (-10.9) 18,Plane 5.28571 4.85714 14,Plane 7.75 (-10.11112) (-16.14286),Plane 8.85714 7.25 (-13.3125),Plane 9.07142 3.5 21.44444,Plane 12.66666 (-5.52942) 17.77272,Plane 17.85714 10.8 (-5),Plane 18.4375 (-9.5) (-6.47620),Plane 20.1 9 14,Plane 21.29411 13.38461 3]







runTest (Input domain planes queries) = do
  print $ isInGeneralPosition planes
{-
          traverse_  print planes
          putStrLn "========================="

          traverse_ print vertices''

          putStrLn "========================="
          writeIpeFile [osp|env.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . ipeFile . NonEmpty.fromList . fmap (fromContent . concat)
            $ [ [ drawEnv env
                , drawVertices vertices''
                , [iO $ defIO domain  &layer ?~  "domain"]
                ]
              , [ drawVertices vertices''
                , [iO $ defIO domain  &layer ?~  "domain"]
                , drawEnv env'
                ]
              ]

-}
          -- print $ intersectionVector orangePlane greenPlane

  -- where

  --   env = lowerEnvelopeOn domain planes
  --   vertices'' = bruteForceVertices planes

  --   subPlanes = NonEmpty.fromList [planes `ix'` 0, planes `ix'` 2, planes `ix'` 3]
  --   env' = lowerEnvelopeOn domain subPlanes
  --   ix' xs i = toList xs List.!! i

    -- greenPlane = planes `ix` 0
    -- orangePlane = planes `ix` 3



    -- test =
--           print lowestAtQ
--   where
--     input = Sample (toList planes) (length planes) [] (length planes)
--     env   = bruteForceTriangulatedEnvelope input


--     q = Point2 (-5) (-1)
--     lowestAtQ = minimumBy (comparing $ evalAt q) planes


--     planes :: NonEmpty (MyPlane :+ IpeColor R)
--     planes = NonEmpty.fromList . fmap (over core MyPlane) . flip (zipWith (:+)) colors $
--     -- planes = NonEmpty.fromList . fmap (over core MyPlane) $
--     --          [ Plane (-1) 3 1           :+ red
--     --          , Plane 1.66666 1.66666 (-3) :+ blue
--     --          , Plane 2.66666 (-1) 0.5      :+ green
--     --          , Plane 0 0 1               :+ orange
--     --          , Plane (-2) 2 2              :+ yellow
--              -- ]
--     -- planes = NonEmpty.fromList . fmap (over core MyPlane) . flip (zipWith (:+)) colors $
--     --          [ Plane (-15.9) (-2.83334) (-4.16667)
--     --          , Plane (-14.5) 17.57894 (-5.21053)
--     --          ,Plane (-14.23530) 17.6 2.1
--     --          ,Plane (-5) (-11.6) (-16)
--     --          ,Plane 11 (-7.26667) (-7.23077)]
--              [ Plane (-52.74419) 21.3 (-34.26924)
--              , Plane (-52.06667) 7 (-43.09091),Plane (-45.09091) 45 (-2.55556),Plane (-40.79311) 0.91891 1.775,Plane (-31.96875) (-49.17648) 26.4,Plane (-29.5) 21.62790 (-38.65385),Plane (-29.24) (-49.81482) 31.2,Plane (-27.75) (-6.63830) (-20),Plane (-27.56522) 26.97560 (-0.81579),Plane (-25.27778) 42.60714 31.15789,Plane (-25.1875) 29.17647 (-51.96667),Plane (-23.70968) (-11.05556) (-11.58537),Plane (-19.14286) 41.40740 32.66666,Plane (-18.82759) 5 (-32.16667),Plane (-17.22223) 21.8125 (-39.43479),Plane (-16.88236) 39.36363 14.35714,Plane (-16.39286) 25.9375 (-32.89796),Plane (-5.14286) 38.66666 26.16666,Plane (-0.46667) (-23.38096) (-46.92453),Plane 1.15686 31.35 (-30.9375),Plane 3.97435 (-48.69231) 40.125,Plane 5.5 (-19.11112) (-27.3125),Plane 7.25 (-9.36112) (-38.83334),Plane 8 2.61538 (-14.5),Plane 17.86666 31.85185 (-28.51613),Plane 24.5 21 49.16129,Plane 37.19047 46.70588 (-43.34616),Plane 37.33333 20.48936 (-48.21740),Plane 41.8 (-8.23334) 31.17948,Plane 42.35135 (-1) 14.75,Plane 45.52380 22.41860 (-32.82759),Plane 46.70370 12.4 38,Plane 48.90909 (-15.63637) 43.87096,Plane 49 (-7.125) (-27.24490),Plane 52 (-32.79311) (-24.53659)]



--     vertices   = bruteForceVertices input



--         draw'' (prism :+ cl) =
--           [ iO $ defIO (projectPrism prism) &fill ?~ color
--           ]
--         -- loc :: Vertex' r (plane :+ IpeColor r) -> Point 2 r
--         -- loc = \case
--         --   Real v  -> location2 v
--         --   Dummy p -> projectPoint p

--           -- Cone v           -> [iO $ defIO $ location2 v]
--           -- ClippedCone u v  -> [iO $ defIO $ ClosedLineSegment (location2 u) (location2 v)]


-- projectPrism :: (Plane_ plane r, Fractional r, Ord r)
--              => Prism r plane -> Triangle (Point 2 r)
-- projectPrism = fmap $ \case
--   Real v  -> location2 v
--   Dummy p -> projectPoint p





----------------------------------------------------------------------------------
-- * Drawing Utilities

drawVertices :: (Plane_ plane r, Fractional r, Ord plane, Ord r)
             => Set (EnvVertex r plane)
             -> [IpeObject r]
drawVertices = foldMap $ \v -> [iO $ defIO (v^.asPoint) &layer ?~  "vertices"
                               ]

drawEnv :: forall plane r.
           (Plane_ plane r, Ord plane, Ord r, Fractional r, Show r)
        => BoundedLowerEnvelope r (plane :+ IpeColor r) -> [IpeObject r]
drawEnv = ifoldMap draw'
  where
    draw' (_h :+ color) cell = [ iO $ ipeSimplePolygon cell &fill ?~ color
                                                            &layer ?~  "env"
                               ]




--------------------------------------------------------------------------------
-- * Voronoi Diagrams on a bounded region (via the lifting to the lower envelope)

-- TODO: Move this into the main library I guess

type BoundedVoronoiDiagram r site =
  MonoidalMap site (ConvexPolygon (Vertex' (EnvVertex r site) r site))

-- | Computes the voronoi diagram in a given region
voronoiDiagramIn        :: ( Point_ point 2 r, Ord r, Fractional r
                           , Foldable1 set, Functor set
                           , Ord point
                           , Show point, Show r -- TODO
                           )
                        => Triangle (Point 2 r) -> set point -> BoundedVoronoiDiagram r point
voronoiDiagramIn domain = MonoidalMap.mapKeys (^.extra)
                        . fmap (over vertices (first (fmap (^.extra))))
                        . lowerEnvelopeOn domain
                        . fmap (\p -> pointToPlane p :+ p)
  -- TODO: figure out if mapping monotonically is safe...


-- | Given a site and a voronoi diagram, find the set of sites that
-- are closest at the query point i.e. find the set of cells that
-- contain the query point.
--
-- (this uses a very naive O(n) time implementation)
closestAt      :: ( Point_ queryPoint 2 r
                  , Point_ site 2 r, Ord r, Fractional r, Ord site
                  , Show site, Show r
                  , HasIntersectionWith queryPoint (ConvexPolygon (OriginalOrExtra (EnvVertex r site) (Point 2 r :+ r)))
                  ) => queryPoint -> BoundedVoronoiDiagram r site -> Set.Set site
closestAt q = MonoidalMap.keysSet
            . MonoidalMap.filter (\cell -> q `intersects` cell)

-- | Naive closest at implementation
closestAt'   :: ( Point_ queryPoint 2 r
                , Point_ site 2 r, Ord r, Fractional r, Ord site
                , Show site, Show r, Foldable set
                ) => queryPoint -> set site -> Set.Set site
closestAt' q = maybe mempty fst
             . MonoidalMap.minView
             . foldMap (\s -> MonoidalMap.singleton (squaredEuclideanDist q s) (Set.singleton s)
                       )

--------------------------------------------------------------------------------


drawVD :: forall site r.
          (Ord site, Ord r, Fractional r, Show r)
       => BoundedVoronoiDiagram r (site :+ IpeColor r) -> [IpeObject r]
drawVD = ifoldMap draw'
  where
    draw' (h :+ color) cell = [ iO $ ipeSimplePolygon cell &fill    ?~ color
                                                           &layer   ?~  "env"
                                                           &opacity ?~ "30%"
                              ]

--------------------------------------------------------------------------------

voronoiSpec :: Spec
voronoiSpec = describe "Vornoi specs" $ do
                testIpe [osp|trivial.ipe|]
                        [osp|trivial_out|]
                testIpe [osp|simplest.ipe|]
                        [osp|simplest_out|]
                testIpe [osp|simpler.ipe|]
                        [osp|simpler_out|]
                testIpe [osp|simple.ipe|]
                        [osp|simple_out|]
                testIpe [osp|simple1.ipe|]
                        [osp|simple1_out|]
                testIpe [osp|foo.ipe|]
                        [osp|foo_out|]
                testIpe [osp|colinear.ipe|]
                        [osp|colinear_out|]
                testIpe [osp|pair.ipe|]
                        [osp|pair_out|]
                -- testIpe [osp|buggy.ipe|]
                --         [osp|buggy_out|]

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = do
    (points, domain) <- runIO $ do
        inFp' <- getDataFileName ([osp|test-with-ipe/VoronoiDiagram/|] <> inFp)
        (points' :: NonEmpty (Point 2 R :+ _))   <- NonEmpty.fromList <$> readAllFrom inFp'
        (domain' :: Triangle (Point 2 R) :+ _):_ <- readAllFrom inFp'
        pure (points'&mapped.extra %~ fromMaybe blue . (^.stroke)
             ,domain'^.core)

    let vd = voronoiDiagramIn     domain points
        -- vv = bruteForceVerticesIn domain points
        out = concat [ [iO $ defIO domain &layer ?~  "domain" ]
                     , drawVD vd
                     , [ iO $ defIO p &stroke     ?~ c
                                      &layer      ?~ "sites"
                                      &symbolSize ?~ IpeSize (Named "normal")
                       | p :+ c <- toList points
                       ]
                     ]
    goldenWith [osp|data/test-with-ipe/VoronoiDiagram/new/|]
               (ipeFileGolden { name = outFp })
               (addStyleSheet opacitiesStyle $ singlePageFromContent out)

type MyPoint = Point 2 R :+ IpeColor R

myPoints :: NonEmpty (Point 2 R :+ IpeColor R)
myPoints = NonEmpty.fromList . flip (zipWith (:+)) colors $
           [ Point2 0 0
           , Point2 10 10
           , Point2 100 20
           , Point2 20  200
           , Point2 30 40
           ]

testVD = writeIpeFile [osp|vd.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . addStyleSheet opacitiesStyle
            . singlePageFromContent $
              [ iO $ defIO p  &stroke ?~  c
                              &layer ?~  "sites"
              | p :+ c <- toList myPoints ]
              <>
              drawVD vd
              <>
              [ iO $ defIO domain  &layer ?~  "domain" ]

  where
    vd =   voronoiDiagramIn domain myPoints
    domain = Triangle (Point2 (-200) (-200)) (Point2 500 0) (Point2 0 500)


-- voronoiSpec = describe "Voronoi diagrams again" $ do
--                 prop "closest pair shares edge" $
--                   \(sites :: NESet.NESet MyPoint) ->





-- Triangle (Point2 (-15) 80) (Point2 0 0) (Point2 0 41.88235~)
-- Cone {_apex = Point2 42.775 22.01408~, _leftBoundaryVector = Vector2 0.66666~ (-80.73418~) :+ (), _rightBoundaryVector = Vector2 (-41.79311~) 46.16666~ :+ ()}

assignColors :: NESet.NESet (Point 2 R) -> NESet.NESet MyPoint
assignColors = snd . mapAccumLStrictlyMonotonic f (cycle basicNamedColors)
  where
    f cs p = case cs of
      (c:colors') -> (colors', p :+ c)
      _           -> error "absurd"

-- | mapAccumL; assuming the function is stritly monotonic; thus producing no duplicates.
mapAccumLStrictlyMonotonic      :: (s -> a -> (s, b)) -> s -> NESet.NESet a -> (s, NESet.NESet b)
mapAccumLStrictlyMonotonic f s0 = fmap NESet.fromDistinctAscList . mapAccumL f s0 . NESet.toList


bugI :: NonEmpty (Point 2 R)
bugI = Point2 (-1) 0 :| [Point2 0 0,Point2 0 3.7]
bugDomain ::Triangle (Point 2 R)
bugDomain = Triangle (Point2 5 6) (Point2 (-1) 2) (Point2 0 (-6))

bugX = voronoiDiagramIn bugDomain bugI
testBug = prop "brute force vornoi diagram; covers all points" $
          let sites' = NESet.fromList bugI
              domain = bugDomain
              sites = assignColors sites'
              queries = [Point2 (-0.77) 0]
              vd = voronoiDiagramIn domain (toNonEmpty sites)
              verifyClosest sites q vd =
                     let ss  = closestAt q vd
                         ss' = closestAt' q sites
                     in ss === ss'
          in not (null vd) ==>
                    ipeCounterExample (queries, domain, sites, vd) $
                    counterexample (show vd) $
                    conjoin [ verifyClosest sites q vd
                            | q <- toList queries
                            ]




--------------------------------------------------------------------------------

--match "/verifying cell properties/cells convex/" --seed 295573538
main = hspec verifyCellProperties

bug = prop "bug" $
  let domain :: Triangle (Point 2 R)
      domain = Triangle (Point2 0 0) (Point2 1 (-1)) (Point2 0 1)

      cone :: Cone R (Point 2 R) (Plane R)
      cone = Cone                       ( Point2 ( -0.51219 ) 1.09293 )
                   (Vector2 ( -1 ) 5.0625 :+ Plane 2.75 ( -1.33334 ) 3)
                   (Vector2 1 ( -0.97298 ) :+ Plane 2 3.5 ( -2.66667 ))
        -- Cone { _apex = -- EnvVertex ( Plane ( -4 ) ( -2.66667~ ) 1 )
        --                     --           ( Plane 2 3.5 ( -2.66667~ ) )
        --                     --           ( Plane 2.75 ( -1.33334~ ) 3 ) []
        --               ( Point2 ( -0.51219 ) 1.09293 )
        --               -- 0.13424~
        --           , _leftBoundaryVector = Vector2 ( -1 ) 5.0625 :+ Plane 2.75 ( -1.33334 ) 3
        --           , _rightBoundaryVector = Vector2 1 ( -0.97298 ) :+ Plane 2 3.5 ( -2.66667 )
        --           }
      Just res = coverCone domain cone
  in ipeCounterExample (domain,cone,res) (verifyConvex res)

bug2 = prop "bug2" $
  let domain :: Triangle (Point 2 R)
      domain = Triangle (Point2 0 0) (Point2 0 (-1)) (Point2 (-1) 1)

      cone :: Cone R (Point 2 R) ()
      cone = Cone ( Point2 ( -2.23530 ) ( -5.29412 ) )
                  (Vector2 1 ( -3 ) :+ ())
                  (Vector2 ( -1 ) ( -1.25 ) :+ ())

      Just res = coverCone domain cone
  in ipeCounterExample (domain,cone,res) (verifyConvex res)

-- coverCone", Cone
--     { _apex = EnvVertex
--         ( Plane ( -2 ) ( -1.33334~ ) ( -1 ) )
--         ( Plane ( -1 ) ( -1 ) 3 )
--         ( Plane 1.5 ( -3 ) ( -2 ) ) []
--         ( Point2 ( -2.23530~ ) ( -5.29412~ ) ) 10.52941~, _leftBoundaryVector = Vector2 1
--         ( -3 ) :+ Plane
--         ( -2 )
--         ( -1.33334~ )
--         ( -1 ), _rightBoundaryVector = Vector2 ( -1 ) ( -1.25 ) :+ Plane 1.5
--         ( -3 )
--         ( -2 )
