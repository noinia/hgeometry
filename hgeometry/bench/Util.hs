module Util
  ( randomPoints
  , genByName
  , take'
  ) where

-- import           Control.DeepSeq
import           Data.Hashable
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
-- import           Data.RealNumber.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           System.Random.Stateful

--------------------------------------------------------------------------------

-- | Generate an infinite list random points in a sufficiently large
-- bounding box.
randomPoints       :: forall point r. ( UniformRange point
                      , Point_ point 2 r
                      , Num r
                      )
                   => StdGen -- ^ generator to use
                   -> r -- ^ max coordinate value
                   -> NonEmpty point
randomPoints gen m = points
  where
    points = NonEmpty.unfoldr (fmap Just . uniformR box) gen

    box = (origin, fromVector $ Vector2 m m)

-- | construct a determinsitc generator based on a name.
genByName      :: String ->  StdGen
genByName name = mkStdGen (hash name)

-- | Take n > 0 items from a non-mempty list.
take'   :: Int -> NonEmpty a -> NonEmpty a
take' n = NonEmpty.fromList . NonEmpty.take n
