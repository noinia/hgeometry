module Algorithms.Geometry.ConvexHull.Minimalist.Hull
  ( Zipper(..)
  , RandomAccessZipper(..)

  , Hull(..)
  , toList
  , hullAt

  , Bridge(..), bridgeOf

  , HullSet
  , HullIntMap

  ,  minInftyT
  ) where

-- import           Algorithms.BinarySearch
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Control.Applicative ((<|>))
import           Data.Ext
import           Geometry.Point (ccw, pattern CCW)
import qualified Geometry.PolyLine as PolyLine
import           Geometry.Properties
import qualified Data.List as List
-- import           Geometry.Triangle
-- import qualified Data.List as List
-- import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Sequence as Seq
-- import           Data.Sequence (Seq(..), ViewL(..), ViewR(..))
-- import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import           Data.Set (Set)
import           Data.Maybe
import           Data.Kind


--------------------------------------------------------------------------------

minInftyT :: Num r => r
minInftyT = -1000000000 -- FIXME

--------------------------------------------------------------------------------

-- | class modelling a zipper on a sequence type
class Zipper zipper elem where
  -- | Creates a singleton zipper
  singleton :: elem -> zipper elem
  -- | Gets the element under focus
  focus :: zipper elem -> elem
  -- | moves the focus left
  goLeft  :: zipper elem -> Maybe (zipper elem)
  -- | moves the focus right
  goRight :: zipper elem -> Maybe (zipper elem)
  -- | Moves focus to rightmost elem
  goRightMost   :: zipper elem -> zipper elem
  goRightMost h = maybe h goRightMost $ goRight h
  -- | Moves focus to leftmost elem
  goLeftMost   :: zipper elem -> zipper elem
  goLeftMost h = maybe h goLeftMost $ goLeft h
  {-# MINIMAL singleton, focus, goLeft, goRight #-}

-- | Zipper that also supports prececessor/successor search and
-- insertions and deletions at arbitrary positions.
class (Zipper hull point) => RandomAccessZipper (hull :: Type -> Type) point where
  predOf :: point -> hull point -> Maybe point
  succOf :: point -> hull point -> Maybe point

  -- | delete the point from the hull. If we delete the focus, try to
  -- take from the right first, if that does not work, take the new
  -- focus from the left instead.
  --
  -- pre : the hull remains non-empty
  delete :: point -> hull point -> hull point
  insert :: point -> hull point -> hull point

  -- | Get the predecessor of the focus
  predOfF :: hull point -> Maybe point
  predOfF h = predOf (focus h) h
  -- | Get the successor of the focus
  succOfF :: hull point -> Maybe point
  succOfF h = succOf (focus h) h
  {-# MINIMAL predOf, succOf, delete, insert #-}

class (RandomAccessZipper hull point) => Hull (hull :: Type -> Type) point where
  fromBridge :: Point point => Bridge hull point -> hull point


-- | turn a hull into a list of points
toList :: (Hull hull point, Point point) => hull point -> [point]
toList = List.unfoldr (fmap (\h -> (focus h, goRight h))) . Just . goLeftMost

-- | renders the hull at a particular time.
hullAt   :: (Hull hull point, Point point) => Time point -> hull point
         -> Maybe (PolyLine.PolyLine 2 () (NumType point))
hullAt t = PolyLine.fromPoints . fmap (ext . toPt2 t) . toList

--------------------------------------------------------------------------------

-- | Bridge ; so that (focus l, focus r) represents the actual bridge.
data Bridge hull point = Bridge (hull point) (hull point)
                       deriving (Show,Eq)

type instance NumType (Bridge hull point) = NumType point

--------------------------------------------------------------------------------
-- * Implementation of a Hull using Data.Set

-- | The Hull data type
data HullSet point = HullSet (Set (X point)) point (Set (X point))

type instance NumType (HullSet point) = NumType point

-- | Data type so that we can compare by the x-coordinate of a point
newtype X point = X { unX :: point } deriving newtype Show

instance Point point => Eq (X point) where
  p == q = p `compare` q == EQ
instance Point point => Ord (X point) where
  (X p) `compare` (X q) = compareX p q

instance Show point => Show (HullSet point) where
  showsPrec d (HullSet ll p rr) = showParen (d > app_prec) $ showString "HullSet "
      .  showList (Set.toAscList ll)
      .  showString " " .  showsPrec (app_prec+1) p .  showString " "
      .  showList (Set.toAscList rr)
    where app_prec = 10

instance Point point => Semigroup (HullSet point) where
  l <> r = fromBridge $ bridgeOf l r

instance Point point => Zipper HullSet point where
  singleton p = HullSet Set.empty p Set.empty
  focus (HullSet _ p _) = p

  goLeft (HullSet ll p rr) =
    (\(X p',ll') -> HullSet ll' p' (Set.insert (X p) rr)) <$> Set.maxView ll

  goRight (HullSet ll p rr) =
    (\(X p',rr') -> HullSet (Set.insert (X p) ll) p' rr') <$> Set.minView rr

  goRightMost h@(HullSet ll p rr) = case Set.maxView rr of
    Nothing        -> h
    Just (X r,rr') -> HullSet (ll <> Set.insert (X p) rr') r Set.empty

  goLeftMost h@(HullSet ll p rr) = case Set.minView ll of
    Nothing        -> h
    Just (X l,ll') -> HullSet Set.empty l (ll' <> Set.insert (X p) rr)

instance Point point => RandomAccessZipper HullSet point where
  succOf q (HullSet ll p rr) = case q `compareX` p of
                                 LT -> (unX <$> Set.lookupGT (X q) ll) <|> Just p
                                 _  -> unX <$> Set.lookupGT (X q) rr

  predOf q (HullSet ll p rr) = case q `compareX` p of
                                 GT -> (unX <$> Set.lookupLT (X q) rr) <|> Just p
                                 _  -> unX <$> Set.lookupLT (X q) ll

  delete q (HullSet ll p rr) = case q `compareX` p of
      LT -> HullSet (Set.delete (X q) ll) p rr
      EQ -> case Set.minView rr of
              Nothing         -> case Set.maxView ll of
                Nothing         -> error "HullSet: delete hull is now empty?"
                Just (X p',ll') -> HullSet ll' p' rr
              Just (X p',rr') -> HullSet ll p' rr'
      GT -> HullSet ll p (Set.delete (X q) rr)

  insert q (HullSet ll p rr) = case q `compareX` p of
                                 LT -> HullSet (Set.insert (X q) ll) p rr
                                 EQ -> error "HullSet: trying to insert existing point"
                                 GT -> HullSet ll p (Set.insert (X q) rr)

  succOfF (HullSet _ _ rr) = unX <$> Set.lookupMin rr
  predOfF (HullSet ll _ _) = unX <$> Set.lookupMax ll

instance Point point => Hull HullSet point where
  fromBridge (Bridge (HullSet ll l _) (HullSet _ r rr)) = HullSet ll l (Set.insert (X r) rr)


-- | Computes the bridge of the two given hulls
bridgeOf       :: (Hull hull point, Point point)
               => hull point -> hull point -> Bridge hull point
bridgeOf l0 r0 = go (goRightMost l0) (goLeftMost r0)
    where
      go l r | isRight' (succOfF r) l r = go l          (goRight' r)
             | isRight' (predOfF l) l r = go (goLeft' l) r
             | otherwise                = Bridge l r

      isRight' Nothing  _ _ = False
      isRight' (Just x) l r = ccw (toPt l) (toPt r) (toPt2 t x) /= CCW

      goLeft'  = fromMaybe (error "goLeft': no left")   . goLeft
      goRight' = fromMaybe (error "goRight': no right") . goRight

      toPt h = toPt2 t (focus h)
      t = minInftyT

--------------------------------------------------------------------------------

-- | The Hull data type based on an IntMap implementation.
data HullIntMap point = HullIntMap (IntMap.IntMap point) point (IntMap.IntMap point)

type instance NumType (HullIntMap point) = NumType point


instance Show point => Show (HullIntMap point) where
  showsPrec d (HullIntMap ll p rr) = showParen (d > app_prec) $ showString "HullIntMap "
      .  showList (IntMap.toAscList ll)
      .  showString " " .  showsPrec (app_prec+1) p .  showString " "
      .  showList (IntMap.toAscList rr)
    where app_prec = 10

instance (HasIndex point, Point point) => Semigroup (HullIntMap point) where
  l <> r = fromBridge $ bridgeOf l r

instance HasIndex point => Zipper HullIntMap point where
  singleton p = HullIntMap mempty p mempty
  -- | Gets the element under focus
  focus (HullIntMap _ p _) = p
  -- | moves the focus left
  goLeft (HullIntMap ll p rr) =
    (\(p',ll') -> HullIntMap ll' p' (IntMap.insert (indexOf p) p rr)) <$> IntMap.maxView ll

  goRight (HullIntMap ll p rr) =
    (\(p',rr') -> HullIntMap (IntMap.insert (indexOf p) p ll) p' rr') <$> IntMap.minView rr

  goRightMost h@(HullIntMap ll p rr) = case IntMap.maxView rr of
    Nothing      -> h
    Just (r,rr') -> HullIntMap (ll <> IntMap.insert (indexOf p) p rr') r IntMap.empty

  goLeftMost h@(HullIntMap ll p rr) = case IntMap.minView ll of
    Nothing      -> h
    Just (l,ll') -> HullIntMap IntMap.empty l (ll' <> IntMap.insert (indexOf p) p rr)


instance HasIndex point => RandomAccessZipper HullIntMap point where
  succOf q (HullIntMap ll p rr) = case q `compareIdx` p of
                                 LT -> (snd <$> IntMap.lookupGT (indexOf q) ll) <|> Just p
                                 _  -> snd <$> IntMap.lookupGT (indexOf q) rr

  predOf q (HullIntMap ll p rr) = case q `compareIdx` p of
                                 GT -> (snd <$> IntMap.lookupLT (indexOf q) rr) <|> Just p
                                 _  -> snd <$> IntMap.lookupLT (indexOf q) ll

  delete q (HullIntMap ll p rr) = case q `compareIdx` p of
      LT -> HullIntMap (IntMap.delete (indexOf q) ll) p rr
      EQ -> case IntMap.minView rr of
              Nothing         -> case IntMap.maxView ll of
                Nothing         -> error "HullIntMap: delete hull is now empty?"
                Just (p',ll') -> HullIntMap ll' p' rr
              Just (p',rr') -> HullIntMap ll p' rr'
      GT -> HullIntMap ll p (IntMap.delete (indexOf q) rr)

  insert q (HullIntMap ll p rr) = case q `compareIdx` p of
                                 LT -> HullIntMap (IntMap.insert (indexOf q) q ll) p rr
                                 EQ -> error "HullIntMap: trying to insert existing point"
                                 GT -> HullIntMap ll p (IntMap.insert (indexOf q) q rr)

  succOfF (HullIntMap _ _ rr) = snd <$> IntMap.lookupMin rr
  predOfF (HullIntMap ll _ _) = snd <$> IntMap.lookupMax ll

instance HasIndex point => Hull HullIntMap point where
  fromBridge (Bridge (HullIntMap ll l _) (HullIntMap _ r rr)) =
    HullIntMap ll l (IntMap.insert (indexOf r) r rr)

--------------------------------------------------------------------------------

-- type Hull' = NonEmpty

-- instance Hull NonEmpty where
--   singleton = (:| [])
--   bridgeOf lh rh = undefined
