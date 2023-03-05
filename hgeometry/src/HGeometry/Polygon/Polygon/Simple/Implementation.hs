{-# LANGUAGE OverloadedStrings #-}
module HGeometry.Polygon.Simple.Implementation
  (
  -- * Polygon
    areaSimplePolygon
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
import           Data.Foldable (minimumBy)
import           Data.Ord (comparing)
import           HGeometry.Point
-- import           HGeometry.LineSegment.Boxed
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class

-- default implementations for simple polygons

--------------------------------------------------------------------------------
-- * Polygon_

areaSimplePolygon :: ( Fractional r
                     , SimplePolygon_ simplePolygon point r
                     ) => simplePolygon -> r
areaSimplePolygon = signedArea

--------------------------------------------------------------------------------
-- * Show

-- | default implementation for show
showSimplePolygon    :: ( SimplePolygon_ simplePolygon point r
                        , Show point
                        )
                     => simplePolygon -> String
showSimplePolygon pg = "SimplePolygon " <> show (pg^..outerBoundary)

--------------------------------------------------------------------------------
-- * Read

-- | default implementation for readsPrec
readsPrecSimplePolygon   :: forall simplePolygon point r.
                            ( Read point
                            , SimplePolygon_ simplePolygon point r
                            ) => Int -> ReadS simplePolygon
readsPrecSimplePolygon d = readParen (d > app_prec) $ \r ->
      [ (uncheckedFromCCWPoints @simplePolygon @point @r @[] vs, t)
      | ("SimplePolygon", s) <- lex r, (vs, t) <- reads s ]
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
