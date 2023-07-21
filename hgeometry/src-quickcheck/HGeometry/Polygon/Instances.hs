module HGeometry.Polygon.Instances
  (

  ) where

-- import           HGeometry.LineSegment.Intersection
import           Control.Lens hiding (elements)
import           System.Random.Stateful
import           Control.Monad.State
-- import           Control.Monad.Random (Random, evalRand, mkStdGen)
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           HGeometry.Number.Real.Rational
import           Data.Serialize
import           HGeometry
import           HGeometry.Boundary
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Monotone
import           Hiraffe.Graph
import           Paths_hgeometry
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


--------------------------------------------------------------------------------

allSimplePolygons :: [SimplePolygon (Point 2 Double)]
allSimplePolygons = allSimplePolygonsWith id

allSimplePolygons' :: [SimplePolygon (Point 2 Rational)]
allSimplePolygons' = allSimplePolygonsWith realToFrac
  -- note: don't use map (realToFrac <$>) allSimplePolygons since that may create
  -- self-intersecting polygons

{-# NOINLINE allSimplePolygonsWith #-}
allSimplePolygonsWith   :: (Ord r, Fractional r) => (Double -> r) -> [SimplePolygon (Point 2 r)]
allSimplePolygonsWith f = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "polygons.simple"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure . catMaybes $
      [ fromPoints [ Point2 (f x) (f y) | (x,y) <- lst ]
      | lst <- pts
      ]

{-

allMultiPolygons :: [MultiPolygon () Double]
allMultiPolygons = allMultiPolygonsWith id

allMultiPolygons' :: [MultiPolygon () Rational]
allMultiPolygons' = allMultiPolygonsWith realToFrac

{-# NOINLINE allMultiPolygonsWith #-}
allMultiPolygonsWith   :: (Ord r, Fractional r) => (Double -> r) -> [MultiPolygon () r]
allMultiPolygonsWith f = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "polygons.multi"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ MultiPolygon (toSimple boundary) (map toSimple holes)
      | (boundary:holes) <- pts
      ]
  where
    toSimple lst = simpleFromPoints [ ext (f <$> Point2 x y) | (x,y) <- lst ]

-}

-- | Shifts the polygon to the left by n.
rotateLeft      :: SimplePolygon_ simplePolygon point r => Int -> simplePolygon -> simplePolygon
rotateLeft n pg = uncheckedFromCCWPoints $ pg^..ccwOuterBoundaryFrom (n `mod` numVertices pg)

instance Arbitrary (SimplePolygon (Point 2 Rational)) where
  arbitrary = do
    p <- elements allSimplePolygons'
    n <- chooseInt (0, numVertices p-1)
    pure $ rotateLeft n p
  -- shrink p
  --   | isTriangle p = simplifyP p
  --   | otherwise = cutEars p ++ simplifyP p

instance Arbitrary (SimplePolygon (Point 2 (RealNumber (p::Nat)))) where
  arbitrary = over (vertices.coordinates) realToFrac
           <$> (arbitrary :: Gen (SimplePolygon (Point 2 Rational)))
  shrink = map (over (vertices.coordinates) realToFrac)
         . shrink . trunc
    where
      trunc :: SimplePolygon (Point 2 (RealNumber (p::Nat))) -> SimplePolygon (Point 2 Rational)
      trunc = over (vertices.coordinates) realToFrac

-- instance Arbitrary (MultiPolygon () Rational) where
--   arbitrary = elements allMultiPolygons'

simplifyP :: SimplePolygon (Point  2 Rational) -> [SimplePolygon (Point 2 Rational)]
simplifyP pg
      -- Scale up polygon such that each coordinate is a whole number.
    | lcmP /= 1 = [ pg&vertices %~ multP lcmP  ]
        -- unsafeFromCircularVector $ CV.map (over core (multP lcmP)) vs]

      -- Scale down polygon maintaining each coordinate as a whole number
    | gcdP /= 1 = [ pg&vertices %~ divP gcdP ]

        -- unsafeFromCircularVector $ CV.map (over core (divP gcdP)) vs]
    | minX /= 0 || minY /= 0 = [ pg&vertices %~ align ]
      -- = [unsafeFromCircularVector $ CV.map (over core align) vs]
    | otherwise =
      let pg' = pg&vertices %~ _div2
            -- unsafeFromCircularVector $ CV.map (over core _div2) vs
      in [ pg' | not (hasSelfIntersections pg') ]
    -- otherwise = []
  where
    minX = first1Of (minimumVertexBy (comparing (^.xCoord)).xCoord) pg
    minY = first1Of (minimumVertexBy (comparing (^.yCoord)).yCoord) pg

      -- F.minimumBy (comparing (view (core.xCoord))) vs ^. core.xCoord
    -- minY = F.minimumBy (comparing (view (core.yCoord))) vs ^. core.yCoord

    -- vs = p ^. outerBoundaryVector


    lcmP = lcmPoint pg
    gcdP = gcdPoint pg
    align   :: Point 2 Rational -> Point 2 Rational
    align v = Point (v .-. Point2 minX minY)

    multP v (Point2 c d) = Point2 (c*v) (d*v)
    divP v (Point2 c d) = Point2 (c/v) (d/v)
    _div2 (Point2 a b) = Point2 (numerator a `div` 2 % 1) (numerator b `div` 2 % 1)

hasSelfIntersections = const True
-- FIXME!

lcmPoint :: SimplePolygon (Point 2 Rational) -> Rational
lcmPoint p = realToFrac t
  where
    vs = p^..outerBoundary
    lst = concatMap (\(Point2 x y) -> [denominator x, denominator y]) vs
    t = foldl1 lcm lst

gcdPoint :: SimplePolygon (Point 2 Rational) -> Rational
gcdPoint p = realToFrac t
  where
    vs = p^..outerBoundary
    lst = concatMap (\(Point2 x y) -> [denominator x, denominator y]) vs
    t = foldl1 gcd lst


{-
cutEarAt     :: SimplePolygon (Point 2 Rational) -> Int -> SimplePolygon (Point 2 Rational)
cutEarAt p n = unsafeFromVector $ V.drop 1 $ CV.toVector $ CV.rotateRight n vs
  where
    vs = p^..outerBoundary

cutEars :: SimplePolygon (Point 2 Rational) -> [SimplePolygon (Point 2 Rational)]
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
      , ccw prev cur next == CCW -- left turn.
      , CV.all (\pt -> pt `elem` [prev,cur,next] || not (onTriangle (_core pt) triangle)) vs
      ]
    vs = p^.outerBoundaryVector

-}

instance (Uniform r, Ord r, Num r) => Arbitrary (MonotonePolygon (Point 2 r)) where
  arbitrary = do
                n <- max 3    <$> getSize
                g <- mkStdGen <$> arbitrary
                let (v, g') = runState randomNonZeroVector g
                pure $ evalState (randomMonotoneDirected n v) g'
