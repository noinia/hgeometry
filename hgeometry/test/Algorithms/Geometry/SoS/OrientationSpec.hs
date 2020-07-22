module Algorithms.Geometry.SoS.OrientationSpec where

import           Algorithms.Geometry.SoS
import           Control.Lens
import           Control.Monad (forM_)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.Proxy
import           Data.Util
import           GHC.TypeNats
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = pure ()

-- do describe "CCW implementation" $ do
--             it "quickcheck " $ property $
--               \(p :: Point 2 Int) q r ->
--                 ccwDirect p q r == ccw p q r

-- -- | Given three points p q and r determine the orientation when going from p to r via q.
-- ccwDirect       :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> CCW
-- ccwDirect p q r = CCWWrap $ z `compare` 0
--             -- case z `compare` 0 of
--             --   LT -> CW
--             --   GT -> CCW
--             --   EQ -> CoLinear
--      where
--        Vector2 ux uy = q .-. p
--        Vector2 vx vy = r .-. p
--        z             = ux * vy - uy * vx
