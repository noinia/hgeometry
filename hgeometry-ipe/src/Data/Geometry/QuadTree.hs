{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.QuadTree where

import           Control.Lens
import           Data.Bifoldable
import           Data.BinaryTree (RoseElem(..))
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tree as RoseTree
import           Debug.Trace
import           GHC.Generics (Generic)
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------

data Quadrants a = Quadrants { _northEast  :: !a
                             , _southEast  :: !a
                             , _southWest  :: !a
                             , _northWest  :: !a
                             } deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''Quadrants


instance Applicative Quadrants where
  pure x = Quadrants x x x x
  (Quadrants f g h i) <*> (Quadrants a b c d) = Quadrants (f a) (g b) (h c) (i d)

-- data Quadrants a b c d = Quadrants { _northEast  :: a
--                                    , _southEast  :: b
--                                    , _southWest  :: c
--                                    , _northWest  :: d
--                                    } deriving (Show,Eq,Ord,Generic)


-- type Quadrants' a = Quadrants a a a a

-- type Quadrants' = Quadrants

-- qMap f g h i (Quadrants a b c d) = Quadrants (f a) (g b) (h c) (i d)


data Tree v p = Leaf p
              | Node v (Quadrants (Tree v p))
              deriving (Show,Eq)


-- | Converts into a RoseTree
toRoseTree :: Tree v p -> RoseTree.Tree (RoseElem v p)
toRoseTree = go
  where
    go = \case
      Leaf p    -> RoseTree.Node (LeafNode p) []
      Node v qs -> RoseTree.Node (InternalNode v) (go <$> F.toList qs)


-- type Lvl a = [a]

-- levels   :: RoseTree.Tree a -> [[a]]
-- levels = go . Seq.singleton . (: [])
--   where
--     go   :: Seq.Seq [RoseTree.Tree a] -> [[a]]
--     go s = case Seq.viewl s of
--       Seq.EmptyL                         -> []
--       (xs Seq.:< queue) -> map root xs : go (concatMap children xs)

--         x : go (queue <> Seq.fromList chs)


-- | BFS Traversal of the rose tree that decomposes it into levels.
--
-- running time: \(O(n)\)
levels :: RoseTree.Tree a -> NonEmpty (NonEmpty a)
levels = go1 . (:| [])
  where
    go0   :: [RoseTree.Tree a] -> [NonEmpty a]
    go0 q = case NonEmpty.nonEmpty q of
              Nothing -> []
              Just q1 -> NonEmpty.toList $ go1 q1
    {-# INLINE go0 #-}

    -- all work essentially happens here: given a bunch of trees whose
    -- root elements all have the same level, extract the values
    -- stored at these root nodes, collect all children in a big list,
    -- and explore those recursively.
    go1    :: NonEmpty (RoseTree.Tree a) -> NonEmpty (NonEmpty a)
    go1 qs = fmap root qs :| go0 (concatMap children' qs)
    {-# INLINE go1 #-}

    root      (RoseTree.Node x _)   = x
    children' (RoseTree.Node _ chs) = chs













-- | side lengths will be 2^i for some integer i
type Width = Int
           --
-- | Subdiv of the area from [0,2^w] x [0,2^w]
data QuadTree v p = QuadTree { _boxWidth :: Width
                             , _tree     :: Tree v p
                             }
                  deriving (Show,Eq)



data Split i v p = No !p | Yes !v (Quadrants i) deriving (Show,Eq,Ord)
makePrisms ''Split




data Cell = Cell { _cellWidthF :: Width
                 , _lowerLeft  :: Point 2 Int
                 } deriving (Show,Eq)
makeLenses ''Cell

type R = Int

cellWidth            :: Cell -> Int
cellWidth (Cell w _) = 2 ^ w

toBox            :: Cell -> Box 2 () Int
toBox (Cell w p) = box (ext $ p) (ext $ p .+^ Vector2 (2^w) (2^w))

inCell            :: Point 2 R :+ p -> Cell -> Bool
inCell (p :+ _) c = p `inBox` (toBox c)

cellCorners            :: Cell -> Quadrants (Point 2 R)
cellCorners (Cell w p) = Quadrants (f ww ww) (f ww 0) p (f 0 ww)
  where
    ww    = 2 ^ w
    f x y = p .+^ Vector2 x y

splitCell            :: Cell -> Quadrants Cell
splitCell (Cell w p) = Quadrants (Cell r $ f rr rr)
                                 (Cell r $ f rr 0)
                                 (Cell r p)
                                 (Cell r $ f 0 rr)
  where
    r     = w - 1
    rr    = 2 ^ r
    f x y = p .+^ Vector2 x y


midPoint            :: Cell -> Point 2 Int
midPoint (Cell w p) = let rr = 2 ^ (w - 1) in p .+^ Vector2 rr rr



-- | Annotate the tree with its corresponing cells
withCells :: Cell -> Tree v p -> Tree (v :+ Cell) (p :+ Cell)
withCells c0 = \case
  Leaf p    -> Leaf (p :+ c0)
  Node v qs -> Node (v :+ c0) (withCells <$> splitCell c0 <*> qs)









-- build                      :: Cell -> (Cell -> Split v p)
--                            -> QuadTree v p
-- build cc@(w _ Cell) = QuadTree w . build' cc

-- | Builds a QuadTree
build                       :: (Cell -> pts -> Split pts v p)
                            -> Cell
                            -> pts
                            -> Tree v p
build shouldSplit = build'
  where
    build' cc pts = case shouldSplit cc pts of
                      No p     -> Leaf p
                      Yes v qs -> Node v $ build' <$> splitCell cc <*> qs



--------------------------------------------------------------------------------

limitWidthTo        :: Width -- ^ smallest allowed width
                    -> (Cell -> pts -> Split pts v p)
                    -> (Cell -> pts -> Split pts v (Either pts p))
limitWidthTo wMin f = \c pts -> if wMin < c^.cellWidthF then case f c pts of
                                                               No p     -> No (Right p)
                                                               Yes v qs -> Yes v qs
                                                        else No (Left pts)



fromPoints :: Cell
           -> [Point 2 R :+ p]
           -> Tree () (Maybe (Point 2 R :+ p))
fromPoints = build f
  where
    f c = \case
      []   -> No Nothing
      [p]  -> No (Just p)
      pts  -> Yes () $ (\cell -> filter (`inCell` cell) pts) <$> splitCell c


--------------------------------------------------------------------------------

fromZeros      :: (Num a, Eq a) => Cell -> (Point 2 R -> a) -> Tree (Quadrants Sign) Sign
fromZeros c0 f = build (shouldSplitZeros f') c0 (f' <$> cellCorners c0)
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
shouldSplitZeros f (Cell w' p) qs@(Quadrants ne se sw nw) | all (== Zero) qs = No Zero
                                                          | otherwise        = Yes qs qs'
  where
    m = fAt rr rr
    n = fAt rr ww
    e = fAt ww rr
    s = fAt rr 0
    w = fAt 0  rr

    -- signs at the new corners
    qs' = Quadrants (Quadrants ne e m n)
                    (Quadrants e se s m)
                    (Quadrants m s sw w)
                    (Quadrants n m w nw)

    r     = w' - 1
    rr    = 2 ^ r
    ww    = 2 ^ w'

    fAt x y = f $ p .+^ Vector2 x y
