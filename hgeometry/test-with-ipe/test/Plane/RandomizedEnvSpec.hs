{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Plane.RandomizedEnvSpec
  where

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
-- import           Debug.Trace
import           Ipe.Draw
import           Test.Util
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

----------------------------------------

data Queries = Queries (Triangle (Point 2 R)) (NonEmpty (Point 2 R))
             deriving (Show,Eq)

instance Arbitrary Queries where
  arbitrary = do domain   <- arbitrary
                 queries' <- scale (*100) $
                             fmap NonEmpty.fromList . listOf1 $
                               arbitrary `suchThat` all (> 0)
                 let queries = barrycentric domain <$> queries'
                 pure $ Queries domain queries
  shrink (Queries tri qs) = [ Queries tri qs' | qs' <- shrink qs ]

barrycentric :: Triangle (Point 2 R) -> Vector 3 R -> Point 2 R
barrycentric (Triangle (Point a) (Point b) (Point c)) (normalize -> Vector3 x y z) =
    Point $ (x *^ a) ^+^ (y *^ b) ^+^ (z *^ c)

normalize   :: Vector 3 R -> Vector 3 R
normalize v = let s = sum v in (/s) <$> v

--------------------------------------------------------------------------------

-- | I don't think I really want this one; but just for debugging purposes it seems ok
type instance NumType (a,b) = NumType b
type instance NumType (a,b,c) = NumType c

--------------------------------------------------------------------------------
-- Move to Ipe.Draw

--------------------------------------------------------------------------------

instance (Point_ apex 2 r, Fractional r, Ord r, Show r
         ) => IsDrawable (Ipe r) (Cone r apex edge) where
  type AttrOf (Ipe r) (Cone r apex edge) = PathAttributes r
  draw ats c = [iO $ defIO c]

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






-- | Helper function to apply attributes
apply       :: [at -> at] -> at -> at
apply ats a = foldl' (flip ($)) a ats



--------------------------------------------------------------------------------

spec :: Spec
spec = describe "RandomizedEnvSpec" $ do


         it "coverCone" $ do
           let testCoverCone = coverCone domain (Point2 1 3) (Vector2 (-1) (-1)) (Vector2 1 0)
               domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)

           testCoverCone `shouldBe`
             uncheckedFromCCWPoints (NonEmpty.fromList
               [Extra (Point2 371 3),Extra (Point2 146 148),Original (Point2 1 3)])



         prop "cone cover contains corners comain" $
           \(domain :: Triangle (Point 2 R)) (cone :: Cone R (Point 2 R) ()) ->
             let poly = coverCone domain (cone^.apex) (negated $ cone^.leftBoundaryVector.core)
                                         (cone^.rightBoundaryVector.core)
                 corners' = filter (`intersects` cone) (toList domain)
             in ipeCounterExample (domain,cone,poly) $
                counterexample (show corners') $
                conjoin [ counterexample (show v) $ Every $ v `intersects` poly
                        | v <- corners'
                        ]

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


           xprop "brute force envelope; indeed lowest at query points" $
             \(planes :: NESet.NESet MyPlane) (Queries domain queries) ->
               let env   = lowerEnvelopeOn domain planes
               in counterexample (show env) $
                  ipeCounterExample (queries, domain, toList env) $
                    not (null env) ==>
                      conjoin [ verifyLowestEnv (toNonEmpty planes) q env
                              | q <- toList queries
                              ]


           xprop "brute force triangulated envelope; indeed lowest at query points" $
             \(planes :: NESet.NESet MyPlane) (Queries domain queries) ->
               let env   = triangulatedLowerEnvelopeOn domain planes
               in counterexample (show env) $
                  ipeCounterExample (queries, domain, toList env) $
                    not (null env) ==> conjoin [ verifyLowest (toNonEmpty planes) q env
                                               | q <- toList queries
                                               ]

           xprop "randomized2 same as (new) brute force" $
             \(planes :: NESet.NESet MyPlane)
              (domain :: Triangle (Point 2 R)) (gen :: StdGen) ->
               verticesOf (Randomized.verticesIn gen domain planes)
               ===
               verticesOf (bruteForceVerticesIn domain planes)


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

test :: IO ()
test = do
          writeIpeFile [osp|tri.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . addStyleSheet opacitiesStyle
            . ipeFile . NonEmpty.fromList . fmap (fromContent . concat) $
              [ [ let testCoverCone :: ConvexPolygon (OriginalOrExtra (Point 2 R) (Point 2 R))
                      testCoverCone = coverCone domain (Point2 1 3) (Vector2 (-1) (-1)) (Vector2 1 0)
                  in
                  [ iO $ defIO (mkCone (Point2 1 (3 :: R)) (Vector2 (-1) (-1)) (Vector2 1 0))
                  , iO $ defIO domain  &layer ?~  "domain"
                  , iO $ ipeSimplePolygon testCoverCone &layer  ?~ "result"
                                                        &fill   ?~ seagreen
                                                        &stroke ?~ black
                  ] ]
              , [ let leftV  = Vector2 1 (-1)
                      rightV = Vector2 1 2
                      al     = Point2 5 3
                      ar     = Point2 8 (3 :: R)
                      answer :: ConvexPolygon (Point 2 R)
                      answer = uncheckedFromCCWPoints $ NonEmpty.fromList
                               [ al .-^ (20 *^ leftV)
                               , al, ar
                               , ar .+^ (20 *^ rightV)
                               ]
                      result = coverClippedCone domain al leftV ar rightV
                  in
                  [ iO $ defIO answer &layer ?~  "clippedCone"
                                      &fill ?~ blue
                  , iO $ defIO domain  &layer ?~  "domain"
                  , iO $ ipeSimplePolygon result
                          &layer ?~  "result"
                          &fill ?~ seagreen
                          &stroke ?~  black
                  ] ]
              , [ let al     :: Point 2 R
                      al     = Point2 1.80000 0.60000
                      leftV  = Vector2 1 0.33333
                      ar     = Point2 0.79091 1.60909
                      rightV = Vector2 (-1) (-2.66666)
                      answer :: ConvexPolygon (Point 2 R)
                      answer = uncheckedFromCCWPoints $ NonEmpty.fromList
                               [ al .-^ (20 *^ leftV)
                               , al, ar
                               , ar .+^ (20 *^ rightV)
                               ]
                      result = coverClippedCone domain al leftV ar rightV
                  in
                  [ iO $ defIO answer &layer ?~  "clippedCone"
                                      &fill ?~ blue
                  , iO $ defIO domain  &layer ?~  "domain"
                  , iO $ ipeSimplePolygon result
                          &layer ?~  "result"
                          &fill ?~ seagreen
                          &stroke ?~  black
                  ] ]
              ]
  where
    domain :: Triangle (Point 2 R)
    domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)




test2 = runTest $ Input domain planes []
  where
    domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)

    planes :: NESet.NESet (MyPlane :+ IpeColor R)
    planes = NESet.fromList
           . NonEmpty.fromList . fmap (over core MyPlane) . flip (zipWith (:+)) colors $
            -- [ Plane 0    1    0
            -- , Plane 0    (-1) 0
            -- , Plane 1    0    2
            -- , Plane (-1) (1/100)    2
            -- ]
             -- [ Plane (-1) 3 1
             -- , Plane 1.66666 1.66666 (-3)
             -- , Plane 2.66666 (-1) 0.5
             -- , Plane 0 0 1
             -- , Plane (-2) 2 2
             -- ]

             -- [ Plane (-15.9) (-2.83334) (-4.16667)
             -- , Plane (-14.5) 17.57894 (-5.21053)
             -- ,Plane (-14.23530) 17.6 2.1
             -- ,Plane (-5) (-11.6) (-16)
             -- ,Plane 11 (-7.26667) (-7.23077)]

             [Plane (-17.10527) 14 15.77777, Plane (-4.3) (-12.93334) 0.28571,Plane (-2.42858) (-3.57143) (-9.92858),Plane (-0.27273) (-21.44445) 8.8,Plane 0.625 (-0.875) (-17.18182),Plane 1.2 0.28571 4.4,Plane 1.73333 (-10.9) 18,Plane 5.28571 4.85714 14,Plane 7.75 (-10.11112) (-16.14286),Plane 8.85714 7.25 (-13.3125),Plane 9.07142 3.5 21.44444,Plane 12.66666 (-5.52942) 17.77272,Plane 17.85714 10.8 (-5),Plane 18.4375 (-9.5) (-6.47620),Plane 20.1 9 14,Plane 21.29411 13.38461 3]







runTest (Input domain planes queries) = do
  print $ isInGeneralPosition planes
{-
          traverse_  print planes
          putStrLn "========================="

          traverse_ print vertices

          putStrLn "========================="
          writeIpeFile [osp|env.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . ipeFile . NonEmpty.fromList . fmap (fromContent . concat)
            $ [ [ drawEnv env
                , drawVertices vertices
                , [iO $ defIO domain  &layer ?~  "domain"]
                ]
              , [ drawVertices vertices
                , [iO $ defIO domain  &layer ?~  "domain"]
                , drawEnv env'
                ]
              ]

-}
          -- print $ intersectionVector orangePlane greenPlane

  where

    env = lowerEnvelopeOn domain planes
    vertices = bruteForceVertices planes

    subPlanes = NonEmpty.fromList [planes `ix'` 0, planes `ix'` 2, planes `ix'` 3]
    env' = lowerEnvelopeOn domain subPlanes
    ix' xs i = toList xs List.!! i

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
    draw' (h :+ color) cell = [ iO $ ipeSimplePolygon cell &fill ?~ color
                                                           &layer ?~  "env"
                              ]




--------------------------------------------------------------------------------


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

myPoints :: NonEmpty MyPoint
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
