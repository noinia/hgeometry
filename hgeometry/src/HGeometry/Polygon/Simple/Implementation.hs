{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.Implementation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- default implementations for simple polygons
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.Implementation
  (
  -- * Polygon
    areaSimplePolygon

  , isCounterClockwise
  , toCounterClockwiseOrder
  -- * Show
  , showSimplePolygon
  -- * Read
  , readsPrecSimplePolygon
  -- * Aeson
  , toJSONSimplePolgyon, parseJSONSimplePolygon
  -- * HasSquaredEuclideanDistance
  -- , pointClosestToWithDistanceSimplePolygon
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.List as List
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class

--------------------------------------------------------------------------------
-- * Polygon_

areaSimplePolygon :: ( Fractional r
                     , SimplePolygon_ simplePolygon point r
                     ) => simplePolygon -> r
areaSimplePolygon = signedArea


-- | Test if the outer boundary of the polygon is in clockwise or counter
-- clockwise order.
--
-- Note that this function is useful only for implementing fromPoints;
-- since any valid simplePolygon should be in CCW order!
--
-- running time: \( O(n) \)
isCounterClockwise :: (Num r, Eq r, SimplePolygon_ simplePolygon point r)
                   => simplePolygon -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea2X

-- | Make sure that every edge has the polygon's interior on its left,
-- by orienting the outer boundary into counter-clockwise order, and
-- the inner borders (i.e. any holes, if they exist) into clockwise order.
--
-- Note that this function is useful only for implementing fromPoints;
-- since any valid simplePolygon should be in CCW order!
--
-- running time: \( O(n) \)
toCounterClockwiseOrder   :: (Num r, Eq r, SimplePolygon_ simplePolygon point r)
                          => simplePolygon -> simplePolygon
toCounterClockwiseOrder pg
  | isCounterClockwise pg = pg
  | otherwise             = uncheckedFromCCWPoints . List.reverse $ pg^..outerBoundary

--------------------------------------------------------------------------------
-- * Show

-- | helper implementation for show
showSimplePolygon           :: ( SimplePolygon_ simplePolygon point r
                               , Show point
                               )
                          => String -- ^ Polygon type name
                          -> simplePolygon -> String
showSimplePolygon name pg = name <> " " <> show (pg^..outerBoundary)

--------------------------------------------------------------------------------
-- * Read

-- | default implementation for readsPrec
readsPrecSimplePolygon        :: forall simplePolygon point r.
                                 ( Read point
                                 , SimplePolygon_ simplePolygon point r
                                 )
                              => String -- ^ constructor name
                              -> Int -> ReadS simplePolygon
readsPrecSimplePolygon name d = readParen (d > app_prec) $ \r ->
      [ (uncheckedFromCCWPoints @simplePolygon @point @r @[] vs, t)
      | (name', s) <- lex r
      , name == name'
      , (vs, t) <- reads s
      ]
    where app_prec = 10


--------------------------------------------------------------------------------
-- * Aeson

toJSONSimplePolgyon    :: ( ToJSON point
                          , SimplePolygon_ simplePolygon point r
                          ) => simplePolygon -> Value
toJSONSimplePolgyon pg = object [ "tag"      Aeson..= ("SimplePolygon" :: String)
                                , "vertices" Aeson..= (pg^..outerBoundary)
                                ]

-- instance (FromJSON r, Eq r, Num r, FromJSON p) => FromJSON (Polygon Simple p r) where
parseJSONSimplePolygon :: forall simplePolygon point r.
                          ( FromJSON point
                          , SimplePolygon_ simplePolygon point r
                          ) => Value -> Parser simplePolygon
parseJSONSimplePolygon = withObject "Polygon" $ \o -> o .: "tag" >>= \case
                           "SimplePolygon" -> pSimple o
                           (_ :: String)   -> fail "Not a SimplePolygon"
  where
    pSimple o = uncheckedFromCCWPoints @simplePolygon @point @r @[]  <$> o .: "vertices"



--------------------------------------------------------------------------------
-- * HasSquaredEuclideanDistance

{-

pointClosestToWithDistanceSimplePolygon      :: forall simplePolygon point point' r.
                                                ( SimplePolygon_ simplePolygon point r
                                                , Point_ point' 2 r
                                                , Fractional r, Ord r
                                                )
                                             => point'
                                             -> simplePolygon
                                             -> (Point 2 r, r)
pointClosestToWithDistanceSimplePolygon q poly =
    minimumBy (comparing snd)
  . map (pointClosestToWithDistance q) . id @[ClosedLineSegment 2 point r]
  $ poly^..outerBoundaryEdgeSegments

-}

--------------------------------------------------------------------------------
