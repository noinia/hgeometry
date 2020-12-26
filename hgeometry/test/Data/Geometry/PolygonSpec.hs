{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.PolygonSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import           Control.Lens                                ((^.), (^..), over)
import           Control.Monad
import qualified Data.ByteString                             as BS
import           Data.Ext
import qualified Data.Foldable                               as F
import           Data.Geometry
import           Data.Geometry.Boundary
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon                       (fromPoints)
import           Data.Geometry.Triangle
import           Data.Proxy
import           Data.Ratio
import           Data.Serialize
import qualified Data.Vector                                 as V
import           Data.Vector.Circular                        (CircularVector)
import qualified Data.Vector.Circular                        as CV
import qualified Data.Vector.Circular.Util                   as CV
import           Paths_hgeometry_test
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances                   ()

{-# NOINLINE allSimplePolygons #-}
allSimplePolygons :: [SimplePolygon () Double]
allSimplePolygons = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "data/polygons.simple"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ fromPoints [ ext (Point2 x y) | (x,y) <- lst ]
      | lst <- pts
      ]

allSimplePolygons' :: [SimplePolygon () Rational]
allSimplePolygons' = map (realToFrac <$>) allSimplePolygons

{-# NOINLINE allMultiPolygons #-}
allMultiPolygons :: [MultiPolygon () Double]
allMultiPolygons = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "data/polygons.multi"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ MultiPolygon (toSimple boundary) (map toSimple holes)
      | (boundary:holes) <- pts
      ]
  where
    toSimple lst = fromPoints [ ext (Point2 x y) | (x,y) <- lst ]

allMultiPolygons' :: [MultiPolygon () Rational]
allMultiPolygons' = map (realToFrac <$>) allMultiPolygons

instance Arbitrary (SimplePolygon () Rational) where
  arbitrary = fmap realToFrac <$> elements allSimplePolygons
  shrink p
    | isTriangle p = simplifyP p
    | otherwise = cutEars p ++ simplifyP p

instance Arbitrary (MultiPolygon () Rational) where
  arbitrary = fmap realToFrac <$> elements allMultiPolygons

simplifyP :: SimplePolygon () Rational -> [SimplePolygon () Rational]
simplifyP p
      -- Scale up polygon such that each coordinate is a whole number.
    | lcmP /= 1 = [SimplePolygon $ CV.map (over core (multP lcmP)) vs]
      -- Scale down polygon maintaining each coordinate as a whole number
    | gcdP /= 1 = [SimplePolygon $ CV.map (over core (divP gcdP)) vs]
    -- | otherwise = [SimplePolygon $ CV.map (over core div2) vs]
    | otherwise = []
  where
    vs = p ^. outerBoundaryVector
    lcmP = lcmPoint p
    gcdP = gcdPoint p
    multP v (Point2 c d) = Point2 (c*v) (d*v)
    divP v (Point2 c d) = Point2 (c/v) (d/v)
    _div2 (Point2 a b) = Point2 (numerator a `div` 2 % 1) (numerator b `div` 2 % 1)

lcmPoint :: SimplePolygon () Rational -> Rational
lcmPoint p = realToFrac t
  where
    vs = F.toList (p^.outerBoundaryVector)
    lst = concatMap (\(Point2 x y :+ ()) -> [denominator x, denominator y]) vs
    t = foldl1 lcm lst

gcdPoint :: SimplePolygon () Rational -> Rational
gcdPoint p = realToFrac t
  where
    vs = F.toList (p^.outerBoundaryVector)
    lst = concatMap (\(Point2 x y :+ ()) -> [denominator x, denominator y]) vs
    t = foldl1 gcd lst

cutEarAt :: SimplePolygon () Rational -> Int -> SimplePolygon () Rational
cutEarAt p n = SimplePolygon $ CV.unsafeFromVector $ V.drop 1 $ CV.toVector $ CV.rotateRight n vs
  where
    vs = p^.outerBoundaryVector

cutEars :: SimplePolygon () Rational -> [SimplePolygon () Rational]
cutEars p | isTriangle p = []
cutEars p = map (cutEarAt p) ears
  where
    ears =
      [ n
      | n <- [0 .. F.length vs-1]
      , let prev = CV.index vs (n-1)
            cur  = CV.index vs n
            next = CV.index vs (n+1)
            triangle = Triangle prev cur next
      , ccw' prev cur next == CCW -- left turn.
      , CV.all (\pt -> pt `elem` [prev,cur,next] || not (onTriangle (_core pt) triangle)) vs
      ]
    vs = p^.outerBoundaryVector



spec :: Spec
spec = do
  testCases "test/Data/Geometry/pointInPolygon.ipe"
  it "read . show = id (SimplePolygon)" $ do
    property $ \(pts :: CircularVector (Point 2 Rational :+ ())) ->
      let p = SimplePolygon pts in
      read (show p) == p
  it "read . show = id (MultiPolygon)" $ do
    property $ \(pts :: CircularVector (Point 2 Rational :+ ())) ->
      let simple = SimplePolygon pts
          p = MultiPolygon simple [simple] in
      read (show p) == p
  it "valid polygons (Simple/Double)" $ do
    forM_ allSimplePolygons $ \poly -> do
      hasSelfIntersections poly `shouldBe` False
      isCounterClockwise poly `shouldBe` True
  it "valid polygons (Simple/Rational)" $ do
    forM_ allSimplePolygons' $ \poly -> do
      hasSelfIntersections poly `shouldBe` False
      isCounterClockwise poly `shouldBe` True
  it "valid polygons (Multi/Double)" $ do
    forM_ allMultiPolygons $ \poly -> do
      hasSelfIntersections poly `shouldBe` False
      isCounterClockwise poly `shouldBe` True
  it "valid polygons (Multi/Rational)" $ do
    forM_ allMultiPolygons' $ \poly -> do
      hasSelfIntersections poly `shouldBe` False
      isCounterClockwise poly `shouldBe` True


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading point in polygon file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


--   ipeF <- beforeAll $ readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
--   describe "Point in Polygon tests" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)


data TestCase r = TestCase { _polygon    :: SimplePolygon () r
                           , _inside     :: [Point 2 r]
                           , _onBoundary :: [Point 2 r]
                           , _outside    :: [Point 2 r]
                           }
                  deriving (Show)


toSingleSpec poly r q = (q `inPolygon` poly) `shouldBe` r
  -- where
  --   msg = "Point in polygon test with " ++ show q


toSpec (TestCase poly is bs os) = do
                                    specify "inside tests" $
                                      mapM_ (toSingleSpec poly Inside) is
                                    specify "on boundary tests" $
                                      mapM_ (toSingleSpec poly OnBoundary) bs
                                    specify "outside tests" $
                                      mapM_ (toSingleSpec poly Outside) os

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase poly
                        [ s^.symbolPoint | s <- myPoints ats, isInsidePt  s ]
                        [ s^.symbolPoint | s <- myPoints ats, isBorderPt  s ]
                        [ s^.symbolPoint | s <- myPoints ats, isOutsidePt s ]
             | (poly :+ ats) <- polies
             ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
        syms   = page^..content.traverse._IpeUse


        myPoints polyAts = [s | (s :+ ats) <- syms, belongsToPoly ats polyAts ]

        -- We test a point/polygon combination if they have the same color
        belongsToPoly symAts polyAts =
            lookupAttr colorP symAts == lookupAttr colorP polyAts

        -- A point i inside if it is a disk
        isInsidePt   :: IpeSymbol r -> Bool
        isInsidePt s = s^.symbolName == "mark/disk(sx)"

        -- Boxes are on the boundary
        isBorderPt s = s^.symbolName == "mark/box(sx)"

        -- crosses are outside the polygon
        isOutsidePt s = s^.symbolName == "mark/cross(sx)"

        colorP = Proxy :: Proxy Stroke




-- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
