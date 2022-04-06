{-# LANGUAGE DerivingStrategies #-}
module Algorithms.Geometry.ConvexHull.Minimalist.Hull where

import           Algorithms.BinarySearch
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Control.Applicative ((<|>))
import           Control.Lens ((^.), view)
import           Data.Geometry.Point (xCoord, yCoord, zCoord, ccw, CCW(..), pattern CCW)
import           Data.Geometry.Properties
import qualified Data.List as List
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
import           Data.Set (Set)
import           Data.Maybe
import           Data.Kind

--------------------------------------------------------------------------------

class Hull (hull :: Type -> Type) where
  -- | Creates a singleton hull
  singleton :: point -> hull point

  focus :: Point point => hull point -> point

  -- | moves the focus left
  goLeft  :: Point point => hull point -> Maybe (hull point)
  -- | moves the focus right
  goRight :: Point point => hull point -> Maybe (hull point)

  predOf :: Point point => point -> hull point -> Maybe point
  succOf :: Point point => point -> hull point -> Maybe point

  fromBridge :: Point point => Bridge hull point -> hull point

  delete :: Point point => point -> hull point -> hull point
  insert :: Point point => point -> hull point -> hull point

  -- | Moves focus to rightmost point
  goRightMost   :: Point point => hull point -> hull point
  goRightMost h = maybe h goRightMost $ goRight h
  -- | Moves focus to leftmost point
  goLeftMost   :: Point point => hull point -> hull point
  goLeftMost h = maybe h goLeftMost $ goLeft h

  -- | Get the predecessor of the focus
  predOfF :: Point point => hull point -> Maybe point
  predOfF h = predOf (focus h) h
  -- | Get the successor of the focus
  succOfF :: Point point => hull point -> Maybe point
  succOfF h = succOf (focus h) h


toList :: (Hull hull, Point point) => hull point -> [point]
toList = List.unfoldr (fmap (\h -> (focus h, goRight h))) . Just . goLeftMost

--------------------------------------------------------------------------------

-- | Bridge ; so that (focus l, focus r) represents the actual bridge.
data Bridge hull point = Bridge (hull point) (hull point)
                       deriving (Show,Eq)

--------------------------------------------------------------------------------

newtype X point = X { unX :: point } deriving newtype Show

instance Point point => Eq (X point) where
  p == q = p `compare` q == EQ
instance Point point => Ord (X point) where
  (X p) `compare` (X q) = compareX p q


-- | hull zipper
data HullZ point = HullZ (Set (X point)) point (Set (X point))
                 deriving (Show,Eq)

instance Point point => Semigroup (HullZ point) where
  l <> r = fromBridge $ bridgeOf l r

instance Hull HullZ where
  singleton p = HullZ Set.empty p Set.empty
  focus (HullZ _ p _) = p

  goLeft (HullZ ll p rr) =
    (\(X p',ll') -> HullZ ll' p' (Set.insert (X p) rr)) <$> Set.maxView ll

  goRight (HullZ ll p rr) =
    (\(X p',rr') -> HullZ (Set.insert (X p) ll) p' rr') <$> Set.minView rr

  succOf q (HullZ ll p rr) = case q `compareX` p of
                               LT -> (unX <$> Set.lookupGT (X q) ll) <|> Just p
                               _  -> (unX <$> Set.lookupGT (X q) rr)

  predOf q (HullZ ll p rr) = case q `compareX` p of
                               GT -> (unX <$> Set.lookupLT (X q) rr) <|> Just p
                               _  -> (unX <$> Set.lookupLT (X q) ll)


  fromBridge (Bridge (HullZ ll l _) (HullZ _ r rr)) = HullZ ll l (Set.insert (X r) rr)

  delete q (HullZ ll p rr) = case q `compareX` p of
                               LT -> HullZ (Set.delete (X q) ll) p rr
                               EQ -> error "HullZ: trying to delete focus point"
                                 -- TODO: this is probably actually possible.
                               GT -> HullZ ll p (Set.delete (X q) rr)

  insert q (HullZ ll p rr) = case q `compareX` p of
                               LT -> HullZ (Set.insert (X q) ll) p rr
                               EQ -> error "HullZ: trying to insert existing point"
                               GT -> HullZ ll p (Set.insert (X q) rr)


  succOfF (HullZ _ _ rr) = (\(X p) -> p) <$> Set.lookupMin rr
  predOfF (HullZ ll _ _) = (\(X p) -> p) <$> Set.lookupMax ll

  goRightMost h@(HullZ ll p rr) = case Set.maxView rr of
    Nothing        -> h
    Just (X r,rr') -> HullZ (ll <> Set.insert (X p) rr') r Set.empty

  goLeftMost h@(HullZ ll p rr) = case Set.minView ll of
    Nothing        -> h
    Just (X l,ll') -> HullZ Set.empty l (ll' <> Set.insert (X p) rr)


-- | Computes the bridge of the two given hulls
bridgeOf       :: (Hull hull, Point point)
               => hull point -> hull point -> Bridge hull point
bridgeOf l0 r0 = go (goLeftMost l0) (goRightMost r0)
    where
      go l r | isRight' (succOfF r) l r = go l          (goRight' r)
             | isRight' (predOfF l) l r = go (goLeft' l) r
             | otherwise                = Bridge l r

      isRight' Nothing  _ _ = False
      isRight' (Just x) l r = ccw (toPt l) (toPt r) (toPt2 t x) /= CCW

      goLeft'  = fromMaybe (error "goLeft': no left") . goLeft
      goRight' = fromMaybe (error "goRight': no right") . goRight

      toPt h = toPt2 t (focus h)

      t = -100000000 -- FIXME: hack


--------------------------------------------------------------------------------

  -- goLeft (HullZ ll p rr) = case Seq.viewr ll of
  --                            EmptyR   -> error "cannot go left"
  --                            ll' :> l -> HullZ ll' l (p :<| rr)
  -- goRight (HullZ ll p rr) = case Seq.viewl rr of
  --                             EmptyL    -> error "cannot go right"
  --                             r :< rr' -> HullZ (ll :|> p) r rr'


--------------------------------------------------------------------------------

-- type Hull' = NonEmpty

-- instance Hull NonEmpty where
--   singleton = (:| [])
--   bridgeOf lh rh = undefined
