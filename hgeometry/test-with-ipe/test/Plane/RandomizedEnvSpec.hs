{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.RandomizedEnvSpec
  where

import Control.Lens hiding (Prism)
import System.OsPath
import Ipe
import System.Random
import Data.List (sort)
import Data.Foldable
import HGeometry.Kernel
import Plane.BruteForce
import Plane.Sample
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as Original
import R
import Plane.LowerEnvelopeSpec (MyPlane(..))
import Data.Set.NonEmpty qualified as NESet
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Monoidal (MonoidalMap)
import Plane.Randomized2
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Ext
import Ipe.Color
import HGeometry.Polygon.Simple.PossiblyDegenerate
import HGeometry.Intersection

import Debug.Trace
--------------------------------------------------------------------------------

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

data Queries = Queries (Triangle (Point 2 R)) (NonEmpty (Point 2 R))
             deriving (Show,Eq)


barrycentric :: Triangle (Point 2 R) -> Vector 3 R -> Point 2 R
barrycentric (Triangle (Point a) (Point b) (Point c)) (normalize -> Vector3 x y z) =
    Point $ (x *^ a) ^+^ (y *^ b) ^+^ (z *^ c)

normalize   :: Vector 3 R -> Vector 3 R
normalize v = let s = sum v in (/s) <$> v

instance Arbitrary Queries where
  arbitrary = do domain   <- arbitrary
                 queries' <- fmap NonEmpty.fromList . listOf1 $
                             arbitrary `suchThat` (> zero)
                 let queries = barrycentric domain <$> queries'
                 pure $ Queries domain queries

verifyLowest          :: [MyPlane] -> Point 2 R
                      -> TriangulatedLowerEnvelope R MyPlane
                      -> Property
verifyLowest hs q = counterexample (show q) . ifoldMap allPrismsCorrect
  where
    allPrismsCorrect   :: MyPlane -> NonEmpty (Prism R MyPlane :+ extra) -> Every
    allPrismsCorrect h = Every . counterexample (show h) . foldMap (prismIsCorrect h)

    prismIsCorrect         :: MyPlane -> Prism R MyPlane :+ extra -> Every
    prismIsCorrect h (tri :+ _)
      | q `intersects` projectPrism tri = Every $ counterexample (show tri) $ isLowestAtQ h
      | otherwise                       = mempty

    isLowestAtQ   :: MyPlane -> Every
    isLowestAtQ h = let z = evalAt q h
                    in foldMap (\h' -> Every $
                                 counterexample (show h') $
                                 counterexample (show (z,evalAt q h')) $
                                 z <= evalAt q h'
                               ) hs

spec :: Spec
spec = describe "Plane.RandomizedEnvSpec" $ do
         modifyMaxSize (const 60) $ do
           prop "new brute force same as original" $
             \(planes :: NESet.NESet MyPlane) ->
               verticesOf (bruteForceVertices (Sample (toList planes)
                                                      (length planes) [] (length planes)))
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


           prop "brute force triangulated envelope; indeed lowest at query points" $
             \(planes :: NESet.NESet MyPlane) (Queries domain queries) ->
               let input = Sample (toList planes) (length planes) [] (length planes)
                   env   = bruteForceTriangulatedEnvelopeIn domain input
               in conjoin [ verifyLowest (toList planes) q env
                          | q <- toList queries
                          ]

           prop "dummy" $
             \(planes :: NESet.NESet MyPlane) ->
               let input = Sample (toList planes) (length planes) [] (length planes)
               in show (bruteForceTriangulatedEnvelope input) === "foo"

           prop "randomized2 same as (new) brute force" $
             \(planes :: NESet.NESet MyPlane) (gen :: StdGen) ->
               let input = Sample (toList planes) (length planes) [] (length planes) in
               verticesOf (randomizedVertices gen input)
               ===
               verticesOf (bruteForceVertices input)

         runIO test


verticesOf :: (Plane_ plane r, Ord r, Fractional r)
           => MonoidalMap (Vertex r plane) conflictLists -> [Point 3 r]
verticesOf = sort . map location   . MonoidalMap.keys


{-
-- | Test whether two prisms are interiorly disjoint
interiorlyDisjoint     :: (Plane_ plane r, Fractional r, Ord plane, Ord r)
                       => Prism r plane -> Prism r plane -> Bool
interiorlyDisjoint a b = case projectPrism a `intersect` projectPrism b  of
  Just (ActualPolygon _) -> False
  _                      -> True

-}
--------------------------------------------------------------------------------


test = writeIpeFile [osp|env.ipe|] $ singlePageFromContent . concat $
                                   [ draw env
                                   , drawVertices vertices
                                   ,  [iO $ defIO (computeDomain planes)
                                                 ! attr SLayer "domain"]
                                   ]
  where
    input = Sample (toList planes) (length planes) [] (length planes)
    env   = bruteForceTriangulatedEnvelope input

    planes :: NonEmpty (MyPlane :+ IpeColor R)
    planes = NonEmpty.fromList . fmap (over core MyPlane) $
             [ Plane (-1) 3 1           :+ red
             , Plane 1.66666 1.66666 (-3) :+ blue
             , Plane 2.66666 (-1) 0.5      :+ green
             , Plane 0 0 1               :+ orange
             , Plane (-2) 2 2              :+ yellow
             ]

    vertices   = bruteForceVertices input


drawVertices :: (Plane_ plane r, Fractional r, Ord plane, Ord r)
             => MonoidalMap (Vertex r plane) [plane]
             -> [IpeObject r]
drawVertices = ifoldMap $ \v _ -> [iO $ defIO (location2 v)
                                             ! attr SLayer "vertices"
                                  ]

draw :: forall plane r.
        (Plane_ plane r, Ord plane, Ord r, Fractional r, Show r)
     => TriangulatedLowerEnvelope r (plane :+ IpeColor r) -> [IpeObject r]
draw = ifoldMap draw'
  where
    draw' (h :+ color) = foldMap draw''
      where
        draw'' (prism :+ cl) =
          [ iO $ defIO (projectPrism prism) ! attr SFill color
          ]
        -- loc :: Vertex' r (plane :+ IpeColor r) -> Point 2 r
        -- loc = \case
        --   Real v  -> location2 v
        --   Dummy p -> projectPoint p

          -- Cone v           -> [iO $ defIO $ location2 v]
          -- ClippedCone u v  -> [iO $ defIO $ ClosedLineSegment (location2 u) (location2 v)]


projectPrism :: (Plane_ plane r, Fractional r, Ord r)
             => Prism r plane -> Triangle (Point 2 r)
projectPrism = fmap $ \case
  Real v  -> location2 v
  Dummy p -> projectPoint p





--------------------------------------------------------------------------------
