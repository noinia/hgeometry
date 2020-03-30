{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.Geometry.QuadTree where

import           Control.Lens (makeLenses,makePrisms,(^.),(.~),(%~),(&),(^?!),(^@.),ix,iix)
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
import           GHC.Generics (Generic)


import           Debug.Trace

--------------------------------------------------------------------------------

type Quadrants = Corners

pattern Quadrants         :: a -> a -> a -> a -> Corners a
pattern Quadrants a b c d = Corners a b c d
{-# COMPLETE Quadrants #-}

-- data Quadrants a b c d = Quadrants { _northEast  :: a
--                                    , _southEast  :: b
--                                    , _southWest  :: c
--                                    , _northWest  :: d
--                                    } deriving (Show,Eq,Ord,Generic)
-- type Quadrants' a = Quadrants a a a a

-- qMap f g h i (Quadrants a b c d) = Quadrants (f a) (g b) (h c) (i d)


--------------------------------------------------------------------------------
--

-- | The Actual Tree type representing a quadTree
data Tree v p = Leaf p
              | Node v (Quadrants (Tree v p))
              deriving (Show,Eq)

instance Bifunctor Tree where
  bimap = bimapDefault

instance Bifoldable Tree where
  bifoldMap = bifoldMapDefault

instance Bitraversable Tree where
  bitraverse f g = \case
    Leaf p    -> Leaf <$> g p
    Node v qs -> Node <$> f v <*> traverse (bitraverse f g) qs

instance Bifoldable1 Tree
instance Bitraversable1 Tree where
  bitraverse1 f g = \case
    Leaf p    -> Leaf <$> g p
    Node v qs -> Node <$> f v <.> traverse1 (bitraverse1 f g) qs

-- | Fold on the Tree type.
foldTree     :: (p -> b) -> (v -> Quadrants b -> b) -> Tree v p -> b
foldTree f g = go
  where
    go = \case
      Leaf p    -> f p
      Node v qs -> g v (go <$> qs)

-- | Produce a list of all leaves of a quad tree
leaves :: Tree v p -> NonEmpty p
leaves = NonEmpty.fromList . bifoldMap (const []) (:[])

-- | Converts into a RoseTree
toRoseTree :: Tree v p -> RoseTree.Tree (RoseElem v p)
toRoseTree = foldTree (\p    -> RoseTree.Node (LeafNode p)     [])
                      (\v qs -> RoseTree.Node (InternalNode v) (F.toList qs))

-- | Computes the height of the quadtree
height :: Tree v p -> Integer
height = foldTree (const 1) (\_ -> (1 +) . maximum)

--------------------------------------------------------------------------------

-- | side lengths will be 2^i for some integer i
type WidthIndex = Int
           --
-- | Subdiv of the area from [0,2^w] x [0,2^w]
data QuadTree v p = QuadTree { _boxWidthIndex :: WidthIndex
                             , _tree          :: Tree v p
                             }
                  deriving (Show,Eq)
makeLenses ''QuadTree

--------------------------------------------------------------------------------

type R = Int

-- | A Cell corresponding to a node in the QuadTree
data Cell = Cell { _cellWidthIndex :: WidthIndex
                 , _lowerLeft      :: Point 2 Int
                 } deriving (Show,Eq)
makeLenses ''Cell


type instance IntersectionOf (Point 2 r) Cell = '[ NoIntersection, Point 2 r]

instance (Ord r, Num r) => (Point 2 r) `IsIntersectableWith` Cell where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` c = p `intersect` (second toR $ toBox c)
    where
      toR = fromInteger @r . toInteger


cellWidth            :: Cell -> Int
cellWidth (Cell w _) = 2 ^ w

toBox            :: Cell -> Box 2 () Int
toBox (Cell w p) = box (ext $ p) (ext $ p .+^ Vector2 (2^w) (2^w))

inCell            :: Point 2 R :+ p -> Cell -> Bool
inCell (p :+ _) c = p `inBox` (toBox c)

cellCorners :: Cell -> Quadrants (Point 2 R)
cellCorners = fmap (^.core) . corners . toBox

-- | Sides are open
cellSides :: Cell -> Sides (LineSegment 2 () R)
cellSides = fmap (\(ClosedLineSegment p q) -> OpenLineSegment p q) . sides . toBox

splitCell            :: Cell -> Quadrants Cell
splitCell (Cell w p) = Quadrants (Cell r $ f 0 rr)
                                 (Cell r $ f rr rr)
                                 (Cell r $ f rr 0)
                                 (Cell r p)
  where
    r     = w - 1
    rr    = 2 ^ r
    f x y = p .+^ Vector2 x y


midPoint            :: Cell -> Point 2 Int
midPoint (Cell w p) = let rr = 2 ^ (w - 1) in p .+^ Vector2 rr rr


--------------------------------------------------------------------------------
-- * Functions operating on the QuadTree (in temrs of the 'Tree' type)

-- | Data Type to Decide if we should continue splitting the current cell
data Split i v p = No !p | Yes !v (Quadrants i) deriving (Show,Eq,Ord)
makePrisms ''Split

-- build                      :: Cell -> (Cell -> Split v p)
--                            -> QuadTree v p
-- build cc@(w _ Cell) = QuadTree w . build' cc

-- | Builds a QuadTree
build             :: (Cell -> pts -> Split pts v p)
                  -> Cell -> pts -> Tree v p
build shouldSplit = build'
  where
    build' cc pts = case shouldSplit cc pts of
                      No p     -> Leaf p
                      Yes v qs -> Node v $ build' <$> splitCell cc <*> qs

-- | Annotate the tree with its corresponing cells
withCells' :: Cell -> Tree v p -> Tree (v :+ Cell) (p :+ Cell)
withCells' c0 = \case
  Leaf p    -> Leaf (p :+ c0)
  Node v qs -> Node (v :+ c0) (withCells' <$> splitCell c0 <*> qs)

--------------------------------------------------------------------------------
-- * Functions operating on the QuadTree (in terms of the 'Tree' type)

withCells    :: QuadTree v p -> QuadTree (v :+ Cell) (p :+ Cell)
withCells qt = qt&tree .~ withCellsTree qt

withCellsTree                :: QuadTree v p -> Tree (v :+ Cell) (p :+ Cell)
withCellsTree (QuadTree w t) = withCells' (Cell w origin) t

--------------------------------------------------------------------------------


-- | Build a QuadtTree from a set of points.
--
-- pre: the points lie inside the initial given cell.
--
-- running time: \(O(nh)\), where \(n\) is the number of points and
-- \(h\) is the height of the resulting quadTree.
fromPoints :: (Num r, Ord r)
           => Cell -> [Point 2 r :+ p]
           -> Tree () (Maybe (Point 2 r :+ p))
fromPoints = build f
  where
    f c = \case
      []   -> No Nothing
      [p]  -> No (Just p)
      pts  -> Yes () $ partitionPoints c pts
        -- (\cell -> filter (`inCell` cell) pts) <$> splitCell c

-- | Partitions the points into quadrants. See 'quadrantOf' for the
-- precise rules.
partitionPoints   :: (Num r, Ord r) => Cell -> [Point 2 r :+ p] -> Quadrants [Point 2 r :+ p]
partitionPoints c = foldMap (\p -> let q = quadrantOf (p^.core) c in mempty&ix q %~ (p:))


-- | Locates the cell containing the given point, if it exists.
--
-- running time: \(O(h)\), where \(h\) is the height of the quadTree
findLeaf                                       :: (Num r, Ord r)
                                               => Point 2 r -> QuadTree v p -> Maybe (p :+ Cell)
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


-- | Computes the quadrant of the cell corresponding to the current
-- point. Note that we decide the quadrant solely based on the
-- midpoint. If the query point lies outside the cell, it is still
-- assigned a quadrant.
--
-- - The northEast quadrants includes its bottom and left side
-- - The southEast quadrant  includes its            left side
-- - The northWest quadrant  includes its bottom          side
-- - The southWest quadrants does not include any of its sides.
--
--
-- >>> quadrantOf (Point2 9 9) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 9) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 8) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 7) (Cell 4 origin)
-- SouthEast
-- >>> quadrantOf (Point2 4 7) (Cell 4 origin)
-- SouthWest
-- >>> quadrantOf (Point2 4 10) (Cell 4 origin)
-- NorthWest
-- >>> quadrantOf (Point2 4 40) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 4 40) (Cell 4 origin)
-- NorthWest
quadrantOf     :: forall r. (Num r, Ord r)
               => Point 2 r -> Cell -> InterCardinalDirection
quadrantOf q c = let m = fromInteger @r . toInteger <$> midPoint c
                 in case (q^.xCoord < m^.xCoord, q^.yCoord < m^.yCoord) of
                      (False,False) -> NorthEast
                      (False,True)  -> SouthEast
                      (True,False)  -> NorthWest
                      (True,True)   -> SouthWest


--------------------------------------------------------------------------------
-- * Split Functions

-- | Split only when the Cell-width is at least wMin
limitWidthTo        :: WidthIndex -- ^ smallest allowed width of a cell (i.e. width of a leaf)
                    -> (Cell -> pts -> Split pts v p)
                    -> (Cell -> pts -> Split pts v (Either pts p))
limitWidthTo wMin f = \c pts -> case f c pts of
                                  No p                                -> No (Right p)
                                  Yes v qs | wMin < c^.cellWidthIndex -> Yes v qs
                                           | otherwise                -> No (Left pts)
  -- note that it is important that we still evaluate the function so
  -- that we can distinguish at the last level i.e. between a regular
  -- " we are done splitting (No (Right p))" and a "we are no longer
  -- allowed to split further (No (Left p))"

--------------------------------------------------------------------------------


fromZeros :: (Num a, Eq a, v ~ Quadrants Sign)
          => Cell -> (Point 2 R -> a) -> QuadTree v (Either v Sign)
fromZeros = fromZerosWith (limitWidthTo 0)


fromZerosWith           :: (Num a, Eq a, v ~ Quadrants Sign, p ~ Sign, i ~ v)
                        => (  (Cell -> i -> Split i v p)
                           -> (Cell -> i -> Split i v (Either i p))
                           )
                        -> Cell -> (Point 2 R -> a) -> QuadTree (Quadrants Sign) (Either i p)
fromZerosWith limit c0 f = QuadTree (c0^.cellWidthIndex)
                        $ build (limit $ shouldSplitZeros f') c0 (f' <$> cellCorners c0)
  where
    f' = fromSignum f




data Sign = Negative | Zero | Positive deriving (Show,Eq,Ord)

fromSignum   :: (Num a, Eq a) => (b -> a) -> b -> Sign
fromSignum f = \x -> case signum (f x) of
                       -1 -> Negative
                       0  -> Zero
                       1  -> Positive
                       _  -> error "absurd: fromSignum"



shouldSplitZeros :: (Point 2 R -> Sign) -- ^ The function we are evaluating
                 -> Cell
                 -> Quadrants Sign -- ^ signs of the corners
                 -> Split (Quadrants Sign) -- ^ to compute further signs we use signs
                          (Quadrants Sign) -- ^ we store the signs of the corners
                          Sign             -- ^ Leaves store the sign corresponding to the leaf
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
    rr    = 2 ^ r
    ww    = 2 ^ w'

    fAt x y = f $ p .+^ Vector2 x y





isZeroCell :: Either v Sign -> Bool
isZeroCell = \case
    Left _  -> True -- if we kept splitting then we must have a sign transition
    Right s -> s == Zero





withNeighbours    :: [p :+ Cell] -> [(p :+ Cell) :+ Sides [p :+ Cell]]
withNeighbours cs = map (\c@(_ :+ me) -> c :+ neighboursOf me) cs
  where
    -- neighboursOf me = fmap mconcat $ traverse (`relationTo` me) cs
    neighboursOf me = foldMap (`relationTo` me) cs



leafNeighboursOf   :: Cell -> QuadTree v p -> Sides [p :+ Cell]
leafNeighboursOf c = neighboursOf c . leaves . withCellsTree
  where
    neighboursOf me = foldMap (`relationTo` me)


exploreWith               :: forall p. Eq p
                          => (p :+ Cell -> Bool) -- ^ continue exploring?
                          -> p :+ Cell -- ^ start
                          -> [p :+ Cell] -- ^ all cells
                          -> RoseTree.Tree (p :+ Cell)
exploreWith p start' cells = go0 start'
  where
    cs   :: [(p :+ Cell) :+ Sides [p :+ Cell]]
    cs   = withNeighbours cells

    -- initially, just explore everyone
    go0      c = RoseTree.Node c [ go c n | n <- neighboursOf c, p n ]

    -- explore only the nodes other than the one we just came from
    go pred' c = RoseTree.Node c [ go c n | n <- neighboursOf c, continue pred' n ]

    continue pred' n = n /= pred' && p n

    neighboursOf   :: p :+ Cell -> [p :+ Cell]
    neighboursOf c = case List.find (\n -> n^.core == c) cs of
                       Nothing       -> []
                       Just (_ :+ s) -> concat s


explorePathWith          :: Eq p
                         => ((p :+ Cell) -> Bool)
                         -> (p :+ Cell) -> [p :+ Cell] -> NonEmpty (p :+ Cell)
explorePathWith p start' = toPath . exploreWith p start'
  where
    toPath (RoseTree.Node x chs) = ($ x ) $ case chs of
                                              []    -> (:| [])
                                              (c:_) -> (NonEmpty.<| toPath c)






-- trace             :: Cell -> [p :+ Cell] -> [p :+ Cell]
-- trace start cells = go0 start'
--   where
--     cs     = withNeighbours cs
--     start' = find (\c -> c^.extra == start) cs

--     go0 = undefined
--     go p (s :+ neighs) = case find (\c -> ) neighs

--     ( s:+ ) =

--       \case
--       Nothing            -> []
--       Just (s :+ neighs) -> s : go (otherN)
--     go1 s =





relationTo        :: (p :+ Cell) -> Cell -> Sides [p :+ Cell]
c `relationTo` me = f <$> Sides b l t r <*> cellSides me
  where
    Sides t r b l = cellSides (c^.extra)
    f e e' | e `sideIntersects` e' = [c]
           | otherwise             = []

    sa `sideIntersects` sb = (toRat <$> sa) `intersects` (toRat <$> sb)
    toRat :: Int -> Rational
    toRat = realToFrac

-- eastNeighbours    :: [p :+ Cell] -> [(p :+ Cell) :+ [p :+ Cell]]
-- eastNeighbours cs = map (\(_ :+ c) -> Map.lookup c ns) cs
--   where






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
