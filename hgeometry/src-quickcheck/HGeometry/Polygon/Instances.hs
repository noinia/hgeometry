--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the polygon types in hgeometry
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Instances
  ( allSimplePolygons
  ) where

import Control.Lens hiding (elements)
import Control.Monad.State
import Data.Aeson (eitherDecodeFileStrict)
import Data.Ord
import Data.Ratio
import HGeometry
import HGeometry.Intersection
import HGeometry.Number.Real.Rational
import HGeometry.Polygon.Class
import HGeometry.Polygon.Monotone
import HGeometry.Polygon.Simple
import HGeometry.Triangle
import Paths_hgeometry
import System.IO.Unsafe
import System.Random.Stateful
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

allSimplePolygons :: [SimplePolygon (Point 2 Double)]
allSimplePolygons = allSimplePolygonsWith id

allSimplePolygons' :: [SimplePolygon (Point 2 Rational)]
allSimplePolygons' = allSimplePolygonsWith realToFrac
  -- note: don't use map (realToFrac <$>) allSimplePolygons since that may create
  -- self-intersecting polygons

{-# NOINLINE allSimplePolygonsWith #-}
allSimplePolygonsWith   :: forall r.
                            (Ord r, Fractional r) => (Double -> r) -> [SimplePolygon (Point 2 r)]
allSimplePolygonsWith f = unsafePerformIO $ do
  fp <- getDataFileName "polygons.simple.json"
  eitherDecodeFileStrict fp >>= \case
    Left msg  -> error msg
    Right pgs -> pure [ pg&allPoints %~ \p -> p&coordinates %~ f
                      | (pg :: SimplePolygon (Point 2 Double)) <- pgs
                      ]

-- tojson = encodeFile "out.json" $ allSimplePolygons


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
  shrink p
    | isTriangle p = simplifyP p
    | otherwise = cutEars p ++ simplifyP p

instance Arbitrary (SimplePolygon (Point 2 Double)) where
  arbitrary = do
    p <- elements allSimplePolygons
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

    | minX /= 0 || minY /= 0 = [ pg&vertices %~ align ]
      -- = [unsafeFromCircularVector $ CV.map (over core align) vs]
    | otherwise =
      let pg' = pg&vertices %~ _div2
            -- unsafeFromCircularVector $ CV.map (over core _div2) vs
      in [ pg' | hasNoSelfIntersections $ pg'^..vertices ]
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

-- remove vertex i, thereby dropping a vertex
cutEarAt      :: SimplePolygon (Point 2 Rational) -> Int -> SimplePolygon (Point 2 Rational)
cutEarAt pg i = uncheckedFromCCWPoints vs
  where
    vs = ifoldrOf outerBoundary (\j v vs' -> if i == j then vs' else v:vs') [] pg

cutEars :: SimplePolygon (Point 2 Rational) -> [SimplePolygon (Point 2 Rational)]
cutEars pg | isTriangle pg = []
           | otherwise     = [ cutEarAt pg i | i <- [0 .. (n -1)], isEar i ]
  where
    n = numVertices pg
    isEar i = let prev = pg^.outerBoundaryVertexAt ((i-1) `mod` n)
                  cur  = pg^.outerBoundaryVertexAt i
                  nxt  = pg^.outerBoundaryVertexAt ((i+1) `mod` n)
                  triangle = Triangle prev cur nxt
              in ccw prev cur nxt == CCW -- left turn.
                 &&
                 allOf outerBoundary
                       (\pt -> pt `elem` [prev,cur,nxt] || not (pt `intersects` triangle)) pg

isTriangle pg = numVertices pg == 3

instance (Uniform r, Ord r, Num r) => Arbitrary (MonotonePolygon (Point 2 r)) where
  arbitrary = do
                n <- max 3    <$> getSize
                g <- mkStdGen <$> arbitrary
                let (v, g') = runState randomNonZeroVector g
                pure $ evalState (randomMonotoneDirected n v) g'
