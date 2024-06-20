{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module PolygonMode
  ( PolygonModeData(PolygonModeData), currentPolygon

  , PartialPolygon(..)

  ) where

import           Base
import           Control.Lens hiding (view, element)
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

-- type SimplePolygon' r = SimplePolygonF (ViewR1 (Point 2 r))

-- | Just reuse whatever is in PolyLine mode.
newtype PartialPolygon r = PartialPolygon (PartialPolyLine r)
  deriving (Show,Read,Eq)



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

currentPolygon :: Lens' PolygonModeData (Maybe (PartialPolygon R))
currentPolygon = coerced

instance Default PolygonModeData where
  def = PolygonModeData Nothing
