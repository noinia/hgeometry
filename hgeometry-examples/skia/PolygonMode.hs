{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module PolygonMode
  ( PolygonModeData(PolygonModeData), currentPolygon

  , PartialPolygon(..)
  , extendPolygonWith
  , completePolygon

  , SimplePolygon'
  ) where

import           Base
import           Control.Lens hiding (view, element)
import           Data.Coerce
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Sequence
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Sequence.NonEmpty
import           PolyLineMode
-- import           Modes


--------------------------------------------------------------------------------

-- | By default the vertices of a simple polygon are just 'Point 2 r's.
type SimplePolygon' r = SimplePolygon (Point 2 r)

-- | Just reuse whatever is in PolyLine mode.
newtype PartialPolygon r = PartialPolygon (PartialPolyLine r)
  deriving (Show,Read,Eq)

-- | Extend the current partial polyline with the given point (if it is on the canvas.)
extendPolygonWith    :: Eq r
                     => Maybe (Point 2 r) -> Maybe (PartialPolygon r) -> Maybe (PartialPolygon r)
extendPolygonWith mp = coerce . extendWith mp . coerce


-- | Try to complete the partial polygon into a proper simple polygon
completePolygon                  :: (Eq r, Num r)
                                 => PartialPolygon r -> Maybe (SimplePolygon' r )
completePolygon (coerce -> poly) = case poly of
  StartPoint _        -> Nothing
  PartialPolyLine pol -> fromPoints $ toNonEmptyOf vertices pol
    -- needs at least three vertices

-- drawPartialPolygon       :: (Real r, Ord r, Fractional r)
--                          => PolygonModeState r -> Maybe (Point 2 r :+ a) -> Canvas ()
-- drawPartialPolygon state = \case
--   Nothing       -> pure ()
--   Just (p :+ _) -> case extendPolygon p state of
--                      SinglePoint _     -> pure ()
--                      TwoPoints a b     -> lineSegment $ ClosedLineSegment (ext a) (ext b)
--                      MultiplePoints pl -> polygon $ polyLineToPolygon pl


--------------------------------------------------------------------------------

-- | The data for polygon mode
newtype PolygonModeData = PolygonModeData (Maybe (PartialPolygon R))
  deriving (Show,Read,Eq)

-- | Lens to access the current partial polygon
currentPolygon :: Lens' PolygonModeData (Maybe (PartialPolygon R))
currentPolygon = coerced

instance Default PolygonModeData where
  def = PolygonModeData Nothing
