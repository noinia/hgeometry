{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module Polygon.Convex.ConvexSpec
  (spec
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Default.Class
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.ConvexHull.GrahamScan (convexHull)
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Transformation
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 10

instance Default (Point 2 R) where
  def = origin

instance Default (Point 2 Rational) where
  def = origin

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  testCases [osp|Polygon/Convex/convexTests.ipe|]
  it "same minkowskisum manual"  $
    ( minkowskiSum polyP polyQ
    , naiveMinkowski polyP polyQ
    ) `shouldSatisfy` (uncurry isShiftOf)

  -- runIO foo

_foo = writeIpePage [osp|/tmp/out.ipe|] $ fromContent
                     [ toIO (minkowskiSum polyP polyQ)   $ attr SStroke red
                     , toIO (naiveMinkowski polyP polyQ) $ attr SStroke blue
                     , iO'' polyP                        $ attr SStroke black
                     , iO'' polyQ                        $ attr SStroke black
                     ]

polyP, polyQ :: ConvexPolygon (Point 2 Rational)
polyP = read "ConvexPolygon [Point2 ((-1) % 1) ((-3) % 2),Point2 (0 % 1) ((-2) % 1),Point2 (0 % 1) ((-1) % 1),Point2 ((-1) % 1) (1 % 2)]"
polyQ = read "ConvexPolygon [Point2 (0 % 1) (1 % 1),Point2 (1 % 1) ((-1) % 1),Point2 (3 % 2) ((-1) % 1),Point2 (0 % 1) (2 % 1)]"

--------------------------------------------------------------------------------


testCases    :: OsPath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading ConvexTests file" $
                   expectationFailure . unwords $
                     [ "Failed to read ipe file", show fp, ":", show e]
    Right tcs -> do minkowskiTests "polygons in ipe file" $ map _polygon tcs
                    mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: ConvexPolygon (Point 2 r)
                           }
                  deriving (Show)

readInputFromFile    :: OsPath -> IO (Either ConversionError [TestCase R])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase poly | (poly :+ _) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asConvexPolygon


toSingleSpec        :: (Num r, Ord r, Show r)
                    => ConvexPolygon (Point 2 r) -> Vector 2 r -> Expectation
toSingleSpec poly u = eq a c && eq b d `shouldBe` True
  where
    (a,b) = extremes u poly
    (c,d) = extremes u (review _ConvexPolygon poly)
    eq p q = cmpInDirection u p q == EQ

-- | generates 360 vectors "equally" spaced/angled
directions :: Num r => [Vector 2 r]
directions = map (fmap toRat . uncurry Vector2 . (cos &&& sin) . toRad) ([0..359] :: [Double])
  where
    toRad i = i * (pi / 180)
    toRat x = fromIntegral . round $ 100000 * x

toSpec                 :: (Num r, Ord r, Show r) => TestCase r -> SpecWith ()
toSpec (TestCase poly) = do
                           it "Extreme points; binsearch same as linear" $
                             mapM_ (toSingleSpec poly) directions

--------------------------------------------------------------------------------

-- | Center the given polygon at the origin. I.e. places the centroid at the origin.
centerAtOrigin    :: ( SimplePolygon_ polygon  point r
                     , Fractional r
                     , IsTransformable polygon
                     ) => polygon -> polygon
centerAtOrigin pg = translateBy (origin .-. centroid pg) pg

--------------------------------------------------------------------------------

minkowskiTests       ::  (Fractional r, Ord r, Show r, Default (Point 2 r)
                         , IpeWriteText r
                         )
                     => String -> [ConvexPolygon (Point 2 r)] -> Spec
minkowskiTests s pgs = describe ("Minkowskisums on " ++ s) $
    mapM_ (\(i,(p,q)) -> minkowskiTest i p q) $
      zip [0..]
          [ (p,centerAtOrigin q) | p <- pgs, q <- pgs ]

minkowskiTest       ::  ( Fractional r, Ord r, Show r, Default (Point 2 r)
                        , IpeWriteText r
                        )
                    => Int -> ConvexPolygon (Point 2 r) -> ConvexPolygon (Point 2 r) -> Spec
minkowskiTest i p q = describe "minkowskiTest" $ do
    is <- runIO $ encodeFS (show i)
    it "minkowskisum" $
      F (p,q) (minkowskiSum p q) `shouldBe` F (p,q) (naiveMinkowski p q)
    goldenWith [osp|data/golden/Polygon/Convex|]
               (ipeContentGolden { name = [osp|minkowski-vs-naive|] <> is
                                 }
               )
               [ toIO (minkowskiSum p q)   $ attr SStroke red
               , toIO (naiveMinkowski p q) $ attr SStroke blue
               , iO'' p                    $ attr SStroke black
               , iO'' q                    $ attr SStroke black
               ]



toIO    :: (Point_ point 2 r)
        => ConvexPolygon (point :+ extra)
        -> IpeAttributes Path r
        -> IpeObject r
toIO pg = iO'' (convert pg)
  where
    convert :: (Point_ point 2 r) => ConvexPolygon (point :+ extra) -> ConvexPolygon (Point 2 r)
    convert = over vertices (view (core.asPoint))

  -- view (core.asPoint))

data F a b = F a b deriving (Show)

instance (ShiftedEq b, Eq (ElemCyclic b)) => Eq (F a b) where
  (F _ b1) == (F _ b2) = b1 `isShiftOf` b2

naiveMinkowski     :: ( Ord r, Num r
                      , ConvexPolygon_ convexPolygon  point r
                      , ConvexPolygon_ convexPolygon' point' r
                      , Default point'
                      )
                   => convexPolygon -> convexPolygon'
                   -> ConvexPolygon (point :+ point')
naiveMinkowski p q = convexHull . NonEmpty.fromList
                   $ [ v .+. w | v <- p^..outerBoundary
                               , w <- q^..outerBoundary
                     ]
  where
    v .+. w = v .+^ (w^.vector) :+ w

-- newtype CP r = CP (ConvexPolygon (Point 2 r)) deriving (Eq,Show)

-- instance (Arbitrary r, Fractional r, Ord r) => Arbitrary (CP r) where
--   arbitrary =  CP <$> suchThat (convexHull <$> arbitrary)
--                                (\p -> numVertices p > 2)
