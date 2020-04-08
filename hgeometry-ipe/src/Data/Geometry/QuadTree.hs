{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.Geometry.QuadTree-- ( module Data.Geometry.QuadTree.Cell
                             -- , module Data.Geometry.QuadTree.Quadrants
                             -- , module Data.Geometry.QuadTree.Split
                             -- , QuadTree(..)
                             -- , leaves
                             -- , withCells
                             -- )
                             where


import           Control.Lens (makeLenses,makePrisms,(^.),(.~),(%~),(&),(^?!),(^@.),ix,iix,view,to)
-- import           Control.Zipper
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.BinaryTree (RoseElem(..))
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Apply
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable.Class
import           Data.Semigroup.Traversable.Class
import qualified Data.Sequence as Seq
import qualified Data.Tree as RoseTree
import           Debug.Trace
import           Numeric.Natural(Natural)
import           GHC.Generics (Generic)
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Quadrants
import           Data.Geometry.QuadTree.Tree (Tree(..))
import qualified Data.Geometry.QuadTree.Tree as Tree
import           Data.Geometry.QuadTree.Split


import           Debug.Trace

--------------------------------------------------------------------------------

-- data Quadrants a b c d = Quadrants { _northEast  :: a
--                                    , _southEast  :: b
--                                    , _southWest  :: c
--                                    , _northWest  :: d
--                                    } deriving (Show,Eq,Ord,Generic)
-- type Quadrants' a = Quadrants a a a a

-- qMap f g h i (Quadrants a b c d) = Quadrants (f a) (g b) (h c) (i d)


--------------------------------------------------------------------------------
--


--------------------------------------------------------------------------------

-- | Subdiv of the area from [0,2^w] x [0,2^w]
data QuadTree v p = QuadTree { _boxWidthIndex :: WidthIndex
                             , _tree          :: Tree v p
                             }
                  deriving (Show,Eq)
makeLenses ''QuadTree

--------------------------------------------------------------------------------
-- * Functions operating on the QuadTree (in terms of the 'Tree' type)

withCells    :: (Fractional r, Ord r) => QuadTree v p -> QuadTree (v :+ Cell r) (p :+ Cell r)
withCells qt = qt&tree .~ withCellsTree qt

withCellsTree                :: (Fractional r, Ord r)
                             => QuadTree v p -> Tree (v :+ Cell r) (p :+ Cell r)
withCellsTree (QuadTree w t) = Tree.withCells (Cell w origin) t

leaves :: QuadTree v p -> NonEmpty p
leaves = Tree.leaves . view tree

--------------------------------------------------------------------------------

-- | Given a starting cell, a Tree builder, and some input required by
-- the builder, constructs a quadTree.
buildOn           :: Cell r -> (Cell r -> i -> Tree v p) -> i -> QuadTree v p
buildOn c builder = QuadTree (c^.cellWidthIndex) . builder c

-- | The Equivalent of Tree.build for constructing a QuadTree
build     :: (Fractional r, Ord r) => (Cell r -> i -> Split i v p) -> Cell r -> i -> QuadTree v p
build f c = buildOn c (Tree.build f)

-- | Build a QuadtTree from a set of points.
--
-- pre: the points lie inside the initial given cell.
--
-- running time: \(O(nh)\), where \(n\) is the number of points and
-- \(h\) is the height of the resulting quadTree.
fromPointsBox   :: (Fractional r, Ord r)
                 => Cell r -> [Point 2 r :+ p] -> QuadTree () (Maybe (Point 2 r :+ p))
fromPointsBox c = buildOn c Tree.fromPoints

fromPoints     :: (RealFrac r, Ord r)
               => NonEmpty (Point 2 r :+ p) -> QuadTree () (Maybe (Point 2 r :+ p))
fromPoints pts = buildOn c Tree.fromPoints (F.toList pts)
  where
    c = fitsRectangle $ boundingBoxList (view core <$> pts)

-- | Locates the cell containing the given point, if it exists.
--
-- running time: \(O(h)\), where \(h\) is the height of the quadTree
findLeaf                                       :: (Fractional r, Ord r)
                                               => Point 2 r -> QuadTree v p -> Maybe (p :+ Cell r)
findLeaf q (QuadTree w t) | q `intersects` c0  = Just $ findLeaf' c0 t
                          | otherwise          = Nothing
  where
    c0 = Cell w origin
    -- |
    -- pre: p intersects c
    findLeaf' c = \case
      Leaf p    -> p :+ c
      Node _ qs -> let quad = quadrantOf q c
                   in findLeaf' ((splitCell c)^?!ix quad) (qs^?!ix quad)

--------------------------------------------------------------------------------


fromZeros :: (Fractional r, Ord r, Num a, Eq a, v ~ Quadrants Sign)
          => Cell r -> (Point 2 r -> a) -> QuadTree v (Either v Sign)
fromZeros = fromZerosWith (limitWidthTo (-1))


fromZerosWith            ::  (Fractional r, Ord r, Eq a, Num a)
                         => Limiter r (Corners Sign) (Corners Sign) Sign
                         -> Cell r
                         -> (Point 2 r -> a)
                         -> QuadTree (Quadrants Sign) (Signs Sign)
fromZerosWith limit c0 f = fromZerosWith' limit c0 (fromSignum f)


type Signs sign = Either (Corners sign) sign


fromZerosWith'           :: (Eq sign, Fractional r, Ord r)
                         => Limiter r (Corners sign) (Corners sign) sign
                         -> Cell r
                         -> (Point 2 r -> sign)
                         -> QuadTree (Quadrants sign) (Signs sign)
fromZerosWith' limit c0 f = build (limit $ shouldSplitZeros f) c0 (f <$> cellCorners c0)



-- type Sign = Ordering

-- pattern Negative :: Sign
-- pattern Negative = LT
-- pattern Zero :: Sign
-- pattern Zero     = EQ
-- pattern Positive :: Sign
-- pattern Positive = GT
-- {-# COMPLETE Negative, Zero, Positive #-}

-- fromOrdering :: Ordering -> Sign
-- fromOrdering = id


data Sign = Negative | Zero | Positive deriving (Show,Eq,Ord)



-- | Interpret an ordering result as a Sign
fromOrdering :: Ordering -> Sign
fromOrdering = \case
    LT -> Negative
    EQ -> Zero
    GT -> Positive

fromSignum   :: (Num a, Eq a) => (b -> a) -> b -> Sign
fromSignum f = \x -> case signum (f x) of
                       -1 -> Negative
                       0  -> Zero
                       1  -> Positive
                       _  -> error "absurd: fromSignum"

shouldSplitZeros :: forall r sign. (Fractional r, Eq sign)
                 => (Point 2 r -> sign) -- ^ The function we are evaluating
                 -> Splitter r
                             (Quadrants sign) -- ^ the input are the signs of the corners
                             (Quadrants sign) -- ^ at internal nodes we store signs of corners
                             sign
shouldSplitZeros f (Cell w' p) qs@(Quadrants nw ne se sw) | all sameSign qs = No ne
                                                          | otherwise       = Yes qs qs'
  where
    m = fAt rr rr
    n = fAt rr ww
    e = fAt ww rr
    s = fAt rr 0
    w = fAt 0  rr

    sameSign = (== ne)

    -- signs at the new corners
    qs' = Quadrants (Quadrants nw n m w)
                    (Quadrants n ne e m)
                    (Quadrants m e se s)
                    (Quadrants w m s sw)

    r     = w' - 1
    rr    = pow r
    ww    = pow w'

    fAt x y = f $ p .+^ Vector2 x y


isZeroCell   :: (Eq sign) => sign -- ^ the zero value
             -> Either v sign -> Bool
isZeroCell z = \case
    Left _  -> True -- if we kept splitting then we must have a sign transition
    Right s -> s == z

--------------------------------------------------------------------------------



-- | Constructs an empty/complete tree from the starting width
completeTree    :: WidthIndex -> QuadTree () ()
completeTree w0 =
    build (\_ w -> if w == 0 then No () else Yes () (pure $ w - 1)) (Cell w0 origin) w0

--------------------------------------------------------------------------------



-- neighbours   :: _ :> zipper -> [zipper]
-- neighbours z = upwards


-- test :: Top :> (Quadrants a :@ Directions) a
-- test = zipper (Quadrants _ _ _ _) & downward northEast

-- data TZ a = TZ { _focus            :: RoseTree.Tree a
--                , _leftSiblings     :: [RoseTree.Tree a]
--                , _rightSiblings    :: [RoseTree.Tree a]
--                , _parent           :: Maybe (TZ a)
--                }

-- toRoot t = TZ t [] [] Nothing2

-- toFirstChild :: TZ a -> Maybe (TZ a)
-- toFirstChild z@(TZ (RoseTree.Node _ chs) _ _ _) = case chs of
--                                                       []       -> Nothing
--                                                       (c:chs') -> Just $ TZ c [] chs' (Just z)




-- toParent (TZ t ls rs mp) = case mp of
--     Nothing -> Nothing
--     Just (TZ x als ars mg) -> Just
--                            $ TZ (RoseTree.Node (root x) (reverse ls <> [t] <> rs)) als ars mg
--   where
--     root (RoseTree.Node x _) = x
