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
  , shrinkPolygon
  -- , runConvert
  ) where

import Control.Lens hiding (elements)
import Control.Monad.State
import Data.Aeson (eitherDecodeFileStrict)
import Data.Maybe (maybeToList)
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

-- runConvert :: IO ()
-- runConvert = do
--   fp <- getDataFileName "polygons.simple.json"
--   eitherDecodeFileStrict fp >>= \case
--     Left msg  -> error msg
--     Right pgs -> encodeFile "polygons.simple.out" $
--                    [ (pg  :: SimplePolygon (Point 2 Double))
--                    | (pg' :: SimplePolygon (Point 2 Double)) <- pgs
--                    , Just pg <- [fromPoints $ toNonEmptyOf outerBoundary pg']
--                    ]

      -- pure [ pg&allPoints %~ \p -> p&coordinates %~ f
      --                 | (pg :: SimplePolygon (Point 2 Double)) <- pgs
      --                 ]




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
  shrink = shrinkPolygon

-- | Shrink a simple polygon
shrinkPolygon      :: (Ord r, Fractional r, Real r)
                   => SimplePolygon (Point 2 r) -> [SimplePolygon (Point 2 r)]
shrinkPolygon p
    | isTriangle p = simplifyP p
    | otherwise    = cutEars p ++ simplifyP p

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

instance (Uniform r, Ord r, Num r) => Arbitrary (MonotonePolygon (Point 2 r)) where
  arbitrary = do
                n <- max 3    <$> getSize
                g <- mkStdGen <$> arbitrary
                let (v, g') = runState randomNonZeroVector g
                pure $ evalState (randomMonotoneDirected n v) g'





simplifyP     :: forall r. (Ord r, Fractional r, Real r)
              => SimplePolygon (Point 2 r) -> [SimplePolygon (Point 2 r)]
simplifyP pg
      -- Scale up polygon such that each coordinate is a whole number.
    | lcmP /= 1 = [ pg' | pg' <- fromPoints' $ multP lcmP <$> pg^..vertices ]
                 -- we use fromPoints to make sure that we don't create repeated vertices.
      -- Scale down polygon maintaining each coordinate as a whole number
    | gcdP /= 1 = [ pg' | pg' <- fromPoints' $ divP gcdP <$> pg^..vertices ]
    | minX /= 0 || minY /= 0 = [ pg' | pg' <- fromPoints' $ align <$> pg^..vertices ]
    | otherwise =
      let pg' = pg&vertices %~ _div2
      in [ pg' | hasNoSelfIntersections $ pg'^..vertices ]
    -- otherwise = []
  where
    fromPoints' = maybeToList . fromPoints


    minX = first1Of (minimumVertexBy (comparing (^.xCoord)).xCoord) pg
    minY = first1Of (minimumVertexBy (comparing (^.yCoord)).yCoord) pg

      -- F.minimumBy (comparing (view (core.xCoord))) vs ^. core.xCoord
    -- minY = F.minimumBy (comparing (view (core.yCoord))) vs ^. core.yCoord

    -- vs = p ^. outerBoundaryVector


    lcmP = lcmPoint pg
    gcdP = gcdPoint pg
    align   :: Point 2 r -> Point 2 r
    align v = Point (v .-. Point2 minX minY)

    multP v (Point2 c d) = Point2 (c*v) (d*v)
    divP v (Point2 c d) = Point2 (c/v) (d/v)
    _div2 p = let (Point2 a b) = p&coordinates %~ toRational
              in Point2 (fromRational $ numerator a `div` 2 % 1)
                        (fromRational $ numerator b `div` 2 % 1)


lcmPoint :: (Ord r, Fractional r, Real r) => SimplePolygon (Point 2 r) -> r
lcmPoint p = realToFrac t
  where
    vs = p^..outerBoundary
    lst = concatMap ((\(Point2 x y) -> [denominator x, denominator y])
                    . over coordinates toRational
                    ) vs
    t = foldl1 lcm lst

gcdPoint :: (Ord r, Fractional r, Real r) => SimplePolygon (Point 2 r) -> r
gcdPoint p = realToFrac t
  where
    vs = p^..outerBoundary
    lst = concatMap ((\(Point2 x y) -> [denominator x, denominator y])
                    . over coordinates toRational)
                    vs
    t = foldl1 gcd lst

-- remove vertex i, thereby dropping a vertex
cutEarAt      :: (Ord r, Fractional r)
              => SimplePolygon (Point 2 r) -> Int -> Maybe (SimplePolygon (Point 2 r))
cutEarAt pg i = fromPoints vs
  where
    vs = ifoldrOf outerBoundary (\j v vs' -> if i == j then vs' else v:vs') [] pg

cutEars :: (Ord r, Fractional r)
  => SimplePolygon (Point 2 r) -> [SimplePolygon (Point 2 r)]
cutEars pg | isTriangle pg = []
           | otherwise     = [ pg' | i <- [0 .. (n -1)], isEar i, Just pg' <- [cutEarAt pg i] ]
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
