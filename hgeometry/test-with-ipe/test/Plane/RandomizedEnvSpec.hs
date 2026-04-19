{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.RandomizedEnvSpec
  where

import           Data.Ord
import           Control.Lens hiding (Prism)
import           System.OsPath
import           Ipe
import           System.Random
import           Data.List (sort)
import           Data.Foldable
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
import           Plane.Randomized2
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

import Debug.Pretty.Simple
-- import           Debug.Trace
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
                 queries' <- scale (*100) $
                             fmap NonEmpty.fromList . listOf1 $
                               arbitrary `suchThat` all (> 0)
                 let queries = barrycentric domain <$> queries'
                 pure $ Queries domain queries
  shrink (Queries tri qs) = [ Queries tri qs' | qs' <- shrink qs ]


-- verifyLowest          :: [MyPlane] -> Point 2 R
--                       -> TriangulatedLowerEnvelope R MyPlane
--                       -> Property
-- verifyLowest hs q = counterexample (show q)
--                   . allAtLowest
--                   . ifoldMap findContainingPrisms
--   where
--     findContainingPrisms    :: MyPlane -> NonEmpty (Prism R MyPlane :+ extra)
--                             -> [(MyPlane, Prism R MyPlane)]
--     findContainingPrisms h = foldMap $ \(tri :+ _) ->
--                                          ([(h, tri) | q `intersects` projectPrism tri])

--     allAtLowest = \case
--       []   -> Every $ counterexample "No prism containing the query point!"
--                     $ counterexample ("lowest should be: " <> show lowestAtQ) False
--       tris -> foldMap (\(h,tri) -> Every $ counterexample (show tri) $ isLowestAtQ h) tris


--     -- allPrismsCorrect   :: MyPlane -> NonEmpty (Prism R MyPlane :+ extra) -> Every
--     -- allPrismsCorrect h = Every . counterexample (show h) . foldMap (prismIsCorrect h)

--     -- prismIsCorrect         :: MyPlane -> Prism R MyPlane :+ extra -> Every
--     -- prismIsCorrect h (tri :+ _)
--     --   | q `intersects` projectPrism tri = Every $ counterexample (show tri) $ isLowestAtQ h
--     --   | otherwise                       = mempty

--     isLowestAtQ   :: MyPlane -> Every
--     isLowestAtQ h = let z = evalAt q h
--                     in foldMap (\h' -> Every $
--                                  counterexample (show h') $
--                                  counterexample (show (z,evalAt q h')) $
--                                  z <= evalAt q h'
--                                ) hs

--     lowestAtQ = minimumBy (comparing $ evalAt q) hs



spec :: Spec
spec = describe "Plane.RandomizedEnvSpec" $ do


         it "coverCone" $
           testCoverCone `shouldBe`
           uncheckedFromCCWPoints (NonEmpty.fromList [ Original (Point2 1 3)
                                                     ,Extra origin
                                                     ,Extra origin
                                                     ])

         prop "cone cover contains corners comain" $
           \(domain :: Triangle (Point 2 R)) (cone :: Cone R (Point 2 R) ()) ->
             let poly = coverCone domain (cone^.apex) (negated $ cone^.leftBoundaryVector.core)
                                         (cone^.rightBoundaryVector.core)
                 corners' = filter (`intersects` cone) (toList domain)
             in all (`intersects` poly) corners'

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


           -- prop "brute force triangulated envelope; indeed lowest at query points" $
           --   \(planes :: NESet.NESet MyPlane) (Queries domain queries) ->
           --     let input = Sample (toList planes) (length planes) [] (length planes)
           --         env   = bruteForceTriangulatedEnvelopeIn domain input
           --     in counterexample (show env) $
           --       not (null env) ==> conjoin [ verifyLowest (toList planes) q env
           --                                  | q <- toList queries
           --                                  ]
{-
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
-}
         -- runIO test


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
-- --------------------------------------------------------------------------------

colors :: [IpeColor R]
colors = cycle (drop 3 basicNamedColors <> (toList $ namedSet myColors))

myColors :: Map.Map _ (IpeValue (RGB R))
myColors = nonRepeated allColors

mkCone                     :: Num r => apex -> Vector 2 r -> Vector 2 r -> Cone r apex ()
mkCone a incoming outgoing = Cone a (negated incoming :+ ()) (outgoing :+ ())

testCoverCone :: ConvexPolygon (OriginalOrExtra (Point 2 R) (Point 2 R))
testCoverCone = coverCone domain (Point2 1 3) (Vector2 (-1) (-1)) (Vector2 1 0)

domain :: Triangle (Point 2 R)
domain = Triangle (Point2 (-10) (-10)) (Point2 20 0) (Point2 0 20)
-- domain = Triangle (Point2 100 100) (Point2 110 100) (Point2 100 110)

test = do
          writeIpeFile [osp|tri.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . addStyleSheet opacitiesStyle
            . ipeFile . NonEmpty.fromList . fmap (fromContent . concat) $
              [ [ [ iO $ defIO (mkCone (Point2 1 (3 :: R)) (Vector2 (-1) (-1)) (Vector2 1 0))
                  , iO $ defIO domain  ! attr SLayer "domain"
                  , iO $ ipeSimplePolygon testCoverCone ! attr SLayer "result"
                      ! attr SFill seagreen
                      ! attr SStroke black
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
                  [ iO $ defIO answer ! attr SLayer "clippedCone"
                                      ! attr SFill blue
                  , iO $ defIO domain  ! attr SLayer "domain"
                  , iO $ ipeSimplePolygon result
                          ! attr SLayer "result"
                          ! attr SFill seagreen
                          ! attr SStroke black
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
                  [ iO $ defIO answer ! attr SLayer "clippedCone"
                                      ! attr SFill blue
                  , iO $ defIO domain  ! attr SLayer "domain"
                  , iO $ ipeSimplePolygon result
                          ! attr SLayer "result"
                          ! attr SFill seagreen
                          ! attr SStroke black
                  ] ]
              ]









test2 = do
          traverse_  print planes
          putStrLn "========================="

          traverse_ print vertices

          putStrLn "========================="
          writeIpeFile [osp|env.ipe|]
            . addStyleSheet (createIpeStyle "myColors" myColors)
            . ipeFile . NonEmpty.fromList . fmap (fromContent . concat)
            $ [ [ draw env
                , drawVertices vertices
                , [iO $ defIO domain  ! attr SLayer "domain"]
                ]
              , [ drawVertices vertices
                , [iO $ defIO domain  ! attr SLayer "domain"]
                , draw env'
                ]
              ]


          -- print $ intersectionVector orangePlane greenPlane

  where
    env = lowerEnvelopeOn domain planes
    vertices = bruteForceVertices planes
    planes :: NonEmpty (MyPlane :+ IpeColor R)
    planes = NonEmpty.fromList . fmap (over core MyPlane) . flip (zipWith (:+)) colors $
            -- [ Plane 0    1    0
            -- , Plane 0    (-1) 0
            -- , Plane 1    0    2
            -- , Plane (-1) (1/100)    2
            -- ]
             [ Plane (-1) 3 1
             , Plane 1.66666 1.66666 (-3)
             , Plane 2.66666 (-1) 0.5
             , Plane 0 0 1
             -- , Plane (-2) 2 2
             ]

-- green should be the lower part; not the upper!
-- blue should be upper; not lower

    subPlanes = NonEmpty.fromList [planes `ix` 0, planes `ix` 2, planes `ix` 3]
    env' = lowerEnvelopeOn domain subPlanes
    ix xs i = toList xs List.!! i

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


drawVertices :: (Plane_ plane r, Fractional r, Ord plane, Ord r)
             => Set (EnvVertex r plane)
             -> [IpeObject r]
drawVertices = foldMap $ \v -> [iO $ defIO (v^.asPoint) ! attr SLayer "vertices"
                               ]

draw :: forall plane r.
        (Plane_ plane r, Ord plane, Ord r, Fractional r, Show r)
     => BoundedLowerEnvelope r (plane :+ IpeColor r) -> [IpeObject r]
draw = ifoldMap draw'
  where
    draw' (h :+ color) cell = [ iO $ ipeSimplePolygon cell ! attr SFill color
                                                           ! attr SLayer "env"
                              ]

--         draw'' (prism :+ cl) =
--           [ iO $ defIO (projectPrism prism) ! attr SFill color
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





-- --------------------------------------------------------------------------------

-- -- cssStandardColors =
-- --   black         #000000
-- -- silver  #c0c0c0
-- -- gray    #808080
-- -- white   #ffffff
-- -- maroon  #800000
-- -- red     #ff0000
-- -- purple  #800080
-- -- fuchsia         #ff00ff
-- -- green   #008000
-- -- lime    #00ff00
-- -- olive   #808000
-- -- yellow  #ffff00
-- -- navy    #000080
-- -- blue    #0000ff
-- -- teal    #008080
-- -- aqua    #00ffff

-- -- cssExtraColors =
-- -- aliceblue       #f0f8ff
-- -- antiquewhite    #faebd7
-- -- aqua    #00ffff
-- -- aquamarine      #7fffd4
-- -- azure   #f0ffff
-- -- beige   #f5f5dc
-- -- bisque  #ffe4c4
-- -- black   #000000
-- -- blanchedalmond  #ffebcd
-- -- blue    #0000ff
-- -- blueviolet      #8a2be2
-- -- brown   #a52a2a
-- -- burlywood       #deb887
-- -- cadetblue       #5f9ea0
-- -- chartreuse      #7fff00
-- -- chocolate       #d2691e
-- -- coral   #ff7f50
-- -- cornflowerblue  #6495ed
-- -- cornsilk        #fff8dc
-- -- crimson         #dc143c
-- -- cyan
-- --         #00ffff (synonym of aqua)
-- -- darkblue        #00008b
-- -- darkcyan        #008b8b
-- -- darkgoldenrod   #b8860b
-- -- darkgray        #a9a9a9
-- -- darkgreen       #006400
-- -- darkgrey        #a9a9a9
-- -- darkkhaki       #bdb76b
-- -- darkmagenta     #8b008b
-- -- darkolivegreen  #556b2f
-- -- darkorange      #ff8c00
-- -- darkorchid      #9932cc
-- -- darkred         #8b0000
-- -- darksalmon      #e9967a
-- -- darkseagreen    #8fbc8f
-- -- darkslateblue   #483d8b
-- -- darkslategray   #2f4f4f
-- -- darkslategrey   #2f4f4f
-- -- darkturquoise   #00ced1
-- -- darkviolet      #9400d3
-- -- deeppink        #ff1493
-- -- deepskyblue     #00bfff
-- -- dimgray         #696969
-- -- dimgrey         #696969
-- -- dodgerblue      #1e90ff
-- -- firebrick       #b22222
-- -- floralwhite     #fffaf0
-- -- forestgreen     #228b22
-- -- fuchsia         #ff00ff
-- -- gainsboro       #dcdcdc
-- -- ghostwhite      #f8f8ff
-- -- gold    #ffd700
-- -- goldenrod       #daa520
-- -- gray    #808080
-- -- green   #008000
-- -- greenyellow     #adff2f
-- -- grey    #808080 (synonym of gray)
-- -- honeydew        #f0fff0
-- -- hotpink         #ff69b4
-- -- indianred       #cd5c5c
-- -- indigo  #4b0082
-- -- ivory   #fffff0
-- -- khaki   #f0e68c
-- -- lavender        #e6e6fa
-- -- lavenderblush   #fff0f5
-- -- lawngreen       #7cfc00
-- -- lemonchiffon    #fffacd
-- -- lightblue       #add8e6
-- -- lightcoral      #f08080
-- -- lightcyan       #e0ffff
-- -- lightgoldenrodyellow    #fafad2
-- -- lightgray       #d3d3d3
-- -- lightgreen      #90ee90
-- -- lightgrey       #d3d3d3
-- -- lightpink       #ffb6c1
-- -- lightsalmon     #ffa07a
-- -- lightseagreen   #20b2aa
-- -- lightskyblue    #87cefa
-- -- lightslategray  #778899
-- -- lightslategrey  #778899
-- -- lightsteelblue  #b0c4de
-- -- lightyellow     #ffffe0
-- -- lime    #00ff00
-- -- limegreen       #32cd32
-- -- linen   #faf0e6
-- -- magenta
-- --         #ff00ff (synonym of fuchsia)
-- -- maroon  #800000
-- -- mediumaquamarine        #66cdaa
-- -- mediumblue      #0000cd
-- -- mediumorchid    #ba55d3
-- -- mediumpurple    #9370db
-- -- mediumseagreen  #3cb371
-- -- mediumslateblue         #7b68ee
-- -- mediumspringgreen       #00fa9a
-- -- mediumturquoise         #48d1cc
-- -- mediumvioletred         #c71585
-- -- midnightblue    #191970
-- -- mintcream       #f5fffa
-- -- mistyrose       #ffe4e1
-- -- moccasin        #ffe4b5
-- -- navajowhite     #ffdead
-- -- navy    #000080
-- -- oldlace         #fdf5e6
-- -- olive   #808000
-- -- olivedrab       #6b8e23
-- -- orange  #ffa500
-- -- orangered       #ff4500
-- -- orchid  #da70d6
-- -- palegoldenrod   #eee8aa
-- -- palegreen       #98fb98
-- -- paleturquoise   #afeeee
-- -- palevioletred   #db7093
-- -- papayawhip      #ffefd5
-- -- peachpuff       #ffdab9
-- -- peru    #cd853f
-- -- pink    #ffc0cb
-- -- plum    #dda0dd
-- -- powderblue      #b0e0e6
-- -- purple  #800080
-- -- rebeccapurple   #663399
-- -- red     #ff0000
-- -- rosybrown       #bc8f8f
-- -- royalblue       #4169e1
-- -- saddlebrown     #8b4513
-- -- salmon  #fa8072
-- -- sandybrown      #f4a460
-- -- seagreen        #2e8b57
-- -- seashell        #fff5ee
-- -- sienna  #a0522d
-- -- silver  #c0c0c0
-- -- skyblue         #87ceeb
-- -- slateblue       #6a5acd
-- -- slategray       #708090
-- -- slategrey       #708090
-- -- snow    #fffafa
-- -- springgreen     #00ff7f
-- -- steelblue       #4682b4
-- -- tan     #d2b48c
-- -- teal    #008080
-- -- thistle         #d8bfd8
-- -- tomato  #ff6347
-- -- turquoise       #40e0d0
-- -- violet  #ee82ee
-- -- wheat   #f5deb3
-- -- white   #ffffff
-- -- whitesmoke      #f5f5f5
-- -- yellow  #ffff00
-- -- yellowgreen     #9acd32
