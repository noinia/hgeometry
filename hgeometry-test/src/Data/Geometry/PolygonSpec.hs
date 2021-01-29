{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.PolygonSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import           Control.Lens                                (over, view, (^.), (^..))
import           Control.Monad.Random                        (Random, evalRand, mkStdGen)
import qualified Data.ByteString                             as BS
import           Data.Coerce
import           Data.Double.Approximate
import           Data.Ext
import qualified Data.Foldable                               as F
import           Data.Geometry
import           Data.Geometry.Boundary
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Monotone
import           Data.Geometry.Triangle
import           Data.Ord
import           Data.Proxy
import           Data.Ratio
import           Data.RealNumber.Rational
import           Data.Serialize
import qualified Data.Vector                                 as V
import           Data.Vector.Circular                        (CircularVector)
import qualified Data.Vector.Circular                        as CV
import           Paths_hgeometry_test
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances                   ()

type R = RealNumber 5

{-# NOINLINE allSimplePolygons #-}
allSimplePolygons :: [SimplePolygon () Double]
allSimplePolygons = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "data/polygons.simple"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ simpleFromPoints [ ext (Point2 x y) | (x,y) <- lst ]
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
    toSimple lst = simpleFromPoints [ ext (Point2 x y) | (x,y) <- lst ]

allMultiPolygons' :: [MultiPolygon () Rational]
allMultiPolygons' = map (realToFrac <$>) allMultiPolygons

instance Arbitrary (SimplePolygon () Rational) where
  arbitrary = do
    p <- elements allSimplePolygons'
    n <- chooseInt (0, size p-1)
    pure $ rotateLeft n p
  shrink p
    | isTriangle p = simplifyP p
    | otherwise = cutEars p ++ simplifyP p

instance Arbitrary (SimplePolygon () (RealNumber (p::Nat))) where
  arbitrary = elements (map (fmap realToFrac) allSimplePolygons')
  shrink = map (fmap realToFrac) . shrink . trunc
    where
      trunc :: SimplePolygon () (RealNumber (p::Nat)) -> SimplePolygon () Rational
      trunc = fmap realToFrac

instance Arbitrary (MultiPolygon () Rational) where
  arbitrary = elements allMultiPolygons'

simplifyP :: SimplePolygon () Rational -> [SimplePolygon () Rational]
simplifyP p
      -- Scale up polygon such that each coordinate is a whole number.
    | lcmP /= 1 = [unsafeFromCircularVector $ CV.map (over core (multP lcmP)) vs]
      -- Scale down polygon maintaining each coordinate as a whole number
    | gcdP /= 1 = [unsafeFromCircularVector $ CV.map (over core (divP gcdP)) vs]
    | minX /= 0 || minY /= 0
      = [unsafeFromCircularVector $ CV.map (over core align) vs]
    | otherwise =
      let p' = unsafeFromCircularVector $ CV.map (over core _div2) vs
      in [ p' | not (hasSelfIntersections p') ]
    -- otherwise = []
  where
    minX = F.minimumBy (comparing (view (core.xCoord))) vs ^. core.xCoord
    minY = F.minimumBy (comparing (view (core.yCoord))) vs ^. core.yCoord
    vs = p ^. outerBoundaryVector
    lcmP = lcmPoint p
    gcdP = gcdPoint p
    align :: Point 2 Rational -> Point 2 Rational
    align v = coerce (v .-. Point2 minX minY)
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
cutEarAt p n = unsafeFromVector $ V.drop 1 $ CV.toVector $ CV.rotateRight n vs
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

genMonotone :: (Random r, Ord r, Num r) => Gen (Vector 2 r, SimplePolygon () r)
genMonotone = do
  n <- max 3 <$> getSize
  seed <- mkStdGen <$> arbitrary
  let v = evalRand randomNonZeroVector seed
  pure (v, evalRand (randomMonotoneDirected n v) seed)

spec :: Spec
spec = do
  testCases "src/Data/Geometry/pointInPolygon.ipe"
  it "read . show = id (SimplePolygon)" $ do
    property $ \(pts :: CircularVector (Point 2 Rational :+ ())) ->
      let p = unsafeFromCircularVector pts in
      read (show p) == p
  it "read . show = id (MultiPolygon)" $ do
    property $ \(pts :: CircularVector (Point 2 Rational :+ ())) ->
      let simple = unsafeFromCircularVector pts
          p = MultiPolygon simple [simple] in
      read (show p) == p
  -- Hm, is this property always true? What happens when points are all colinear?
  specify "monotone is simple" $
    property $ forAll genMonotone $ \(_dir, mono :: SimplePolygon () R) ->
      isSimple mono
  it "is monotone" $
    property $ forAll genMonotone $ \(dir, mono :: SimplePolygon () R) ->
      isMonotone dir mono
  numericalSpec

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
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


----------------------------------
-- Numerical Robustness

-- Test case found by Kamil Figiela @kfigiela.
polygon :: (Eq r, Fractional r) => SimplePolygon () r
polygon = fromPoints $ map ext
  [ Point2 5584390.945938013 2284567.4635945037
  , Point2 5562410.061516319 2285869.7979417136
  , Point2 5563196.65161862  2250738.663576637
  , Point2 5579688.373487147 2252038.6420285213
  ]


insidePoint, outsidePoint :: Fractional r => Point 2 r
insidePoint  = Point2 5565974.538888888 2273030.9266712796
outsidePoint = Point2 5814191.399840455 2393283.2821864313

numericalSpec :: Spec
numericalSpec =
  describe "insidePolygon" $ do
    specify "baseline check" $ do
      ((insidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` Outside
    it "describes possible regression" $ do
      ((insidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` Outside
    it "describes possible regression" $ do
      ((insidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Outside
