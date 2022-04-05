module Algorithms.Geometry.ConvexHull.Minimalist.Hull where

import           Algorithms.BinarySearch
import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Control.Lens ((^.), view)
import           Data.Geometry.Point (xCoord, yCoord, zCoord)
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Properties
import           Data.List.Util
import           Data.Ord (comparing, Down(..))
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..), ViewL(..), ViewR(..))
import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Kind

--------------------------------------------------------------------------------

class Hull (hull :: Type -> Type) where
  singleton :: point -> hull point

  focus :: hull point -> point
  goLeft  :: hull point -> hull point
  goRight :: hull point -> hull point

  predOf :: point -> hull point -> Maybe point
  succOf :: point -> hull point -> Maybe point

  fromBridge :: Bridge hull point -> hull point
  -- default fromBridge :: (Semigroup (hull point)) => Bridge hull point -> hull point
  -- fromBridge (Bridge ll l _ _ r rr) = ll <> singleton l <> singleton r <> rr

  bridgeOf :: hull point -> hull point -> Bridge hull point

  delete :: Point point => point -> hull point -> hull point
  insert :: Point point => point -> hull point -> hull point

--------------------------------------------------------------------------------

data Bridge hull point = Bridge (hull point) (hull point)
                       deriving (Show,Eq)

--------------------------------------------------------------------------------

data HullZ point = HullZ (Seq point) point (Seq point)
                 deriving (Show,Eq)

instance Hull HullZ where
  singleton p = HullZ mempty p mempty
  focus (HullZ _ p _) = p

  goLeft (HullZ ll p rr) = case Seq.viewr ll of
                             EmptyR   -> error "cannot go left"
                             ll' :> l -> HullZ ll' l (p :<| rr)
  goRight (HullZ ll p rr) = case Seq.viewl rr of
                              EmptyL    -> error "cannot go right"
                              r :< rr' -> HullZ (ll :|> p) r rr'


  fromBridge (Bridge (HullZ ll l _) (HullZ _ r rr)) = HullZ ll l (r :<| rr)

  bridgeOf l r = undefined

  delete q (HullZ ll p rr) = case q `compareX` p of
                               LT -> HullZ (delete' q ll) p rr
                               EQ -> undefined
                               GT -> HullZ ll p (delete' q rr)

delete' = undefined

  -- extractPred :: point -> hull point -> Maybe (hull point, point)
  -- extractSucc :: point -> hull point -> Maybe (point, hull point)

type Hull' = NonEmpty
