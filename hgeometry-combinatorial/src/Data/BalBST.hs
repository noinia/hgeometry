{-# LANGUAGE RecordWildCards #-}
module Data.BalBST where

import           Control.Applicative((<|>))
import           Data.Bifunctor
import           Data.Function (on)
import           Data.Functor.Contravariant
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Tree as T
import           Prelude hiding (lookup,null)

--------------------------------------------------------------------------------

-- | Describes how to search in a tree
data TreeNavigator k a = Nav { goLeft     :: a -> k -> Bool
                             , extractKey :: a -> a -> k
                             }

instance Contravariant (TreeNavigator k) where
  contramap f (Nav gL eK) = Nav (\a k -> gL (f a) k) (\x y -> eK (f x) (f y))


ordNav :: Ord a => TreeNavigator a a
ordNav = Nav (<=) min


ordNavBy   :: Ord b => (a -> b) ->  TreeNavigator b a
ordNavBy f = Nav (\x k -> f x <= k) (min `on` f)


-- instance Functor (TreeNavigator k) where
--   fmap f Nav{..} = Nav (\b k -> )



-- | A balanced binary search tree
data BalBST k a = BalBST { nav    :: !(TreeNavigator k a)
                         , toTree :: !(Tree k a)
                         }

instance (Show k, Show a) => Show (BalBST k a) where
  show (BalBST _ t) = "BalBST (" ++ show t ++ ")"


data Color = Red | Black deriving (Show,Read,Eq,Ord)

type Height = Int

-- Red-Black tree with values in the leaves
data Tree k a = Empty
              | Leaf !a
              | Node !Color !Height (Tree k a) !k (Tree k a) deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | Creates an empty BST
empty   :: TreeNavigator k a -> BalBST k a
empty n = BalBST n Empty


-- | \(O(n\log n)\)
fromList :: TreeNavigator k a -> [a] -> BalBST k a
fromList n = foldr insert (empty n)

fromList' :: Ord a => [a] -> BalBST a a
fromList' = fromList ordNav


-- -- | \(O(n)\)
-- fromAscList :: TreeNavigator k a -> [a] -> BalBST k a
-- fromAscList = undefined


--------------------------------------------------------------------------------

-- | Check if the tree is empty
null                  :: BalBST k a -> Bool
null (BalBST _ Empty) = True
null _                = False

-- | Test if an element occurs in the BST.
-- \(O(\log n)\)
lookup :: Eq a => a -> BalBST k a -> Maybe a
lookup x (BalBST Nav{..} t) = lookup' t
  where
    lookup' Empty            = Nothing
    lookup' (Leaf y)         = if x == y then Just y else Nothing
    lookup' (Node _ _ l k r)
      | goLeft x k           = lookup' l
      | otherwise            = lookup' r

-- | \(O(\log n)\)
member   :: Eq a => a -> BalBST k a -> Bool
member x = isJust . lookup x

-- | Search for the Predecessor
-- \(O(\log n)\)
lookupLE :: Ord k => k -> BalBST k a -> Maybe a
lookupLE kx (BalBST n@Nav{..} t) = lookup' t
  where
    lookup' Empty            = Nothing
    lookup' (Leaf y)         = if goLeft y kx then Just y else Nothing
    lookup' (Node _ _ l k r)
      | kx <= k              = lookup' l
      | otherwise            = lookup' r <|> lookupMax (BalBST n l)


-- | Insert an element in the BST.
--
-- \(O(\log n)\)
insert :: a -> BalBST k a -> BalBST k a
insert x (BalBST n@Nav{..} t) = BalBST n (blacken $ insert' t)
  where
    insert' Empty    = Leaf x
    insert' (Leaf y) = let k     = extractKey x y
                           (l,r) = if goLeft x k then (x,y) else (y,x)
                       in red 2 (Leaf l) k (Leaf r)
    insert' (Node c h l k r)
      | goLeft  x k  = balance c h (insert' l) k r
      | otherwise    = balance c h l           k (insert' r)



-- delete = undefined

-- | Delete (one occurance of) an element.
-- \(O(\log n)\)
delete                        :: Eq a => a -> BalBST k a -> BalBST k a
delete x t = let Split l _ r = split x t
                 n           = nav t
             in BalBST n $ joinWith n l r


-- (BalBST n@Nav{..} t) = delete' t
--   where
--     delete' Empty      = Empty
--     delete' l@(Leaf y) = if x == y then Empty else l
--     delete' (Node c h l k r)
--       | goLeft x k     =


--------------------------------------------------------------------------------


-- | Extract the minimum from the tree
-- \(O(\log n)\)
minView              :: BalBST k a -> Maybe (a, Tree k a)
minView (BalBST n t) = minView' t
  where
    minView' Empty            = Nothing
    minView' (Leaf x)         = Just (x,Empty)
    minView' (Node _ _ l _ r) = fmap (flip (joinWith n) r) <$> minView' l

lookupMin :: BalBST k b -> Maybe b
lookupMin = fmap fst . maxView

-- | Extract the maximum from the tree
-- \(O(\log n)\)
maxView              :: BalBST k a -> Maybe (a, Tree k a)
maxView (BalBST n t) = maxView' t
  where
    maxView' Empty            = Nothing
    maxView' (Leaf x)         = Just (x,Empty)
    maxView' (Node _ _ l _ r) = fmap (joinWith n l) <$> maxView' r

lookupMax :: BalBST k b -> Maybe b
lookupMax = fmap fst . maxView


-- | Joins two BSTs. Assumes that the ranges are disjoint. It takes the left Tree nav
--
-- \(O(\log n)\)
join                           :: BalBST k a -> BalBST k a -> BalBST k a
join (BalBST n l) (BalBST _ r) = BalBST n $ joinWith n l r

-- | Joins two BSTs' with a specific Tree Navigator
--
-- \(O(\log n)\)
joinWith               :: TreeNavigator k a -> Tree k a -> Tree k a -> Tree k a
joinWith Nav{..} tl tr
    | lh >= rh         = blacken $ joinL tl tr
    | otherwise        = blacken $ joinR tl tr
  where
    rh = height tr
    lh = height tl

    joinL Empty      _           = Empty
    joinL l          Empty       = l
    joinL l@(Leaf x) r@(Leaf y)  = red 2 l (extractKey x y) r
    joinL l@(Node c h ll k lr) r
      | h == rh                  = let lm = unsafeMax lr
                                       rm = unsafeMin r
                                   in balance Red (h+1) l (extractKey lm rm) r
      | otherwise                = balance c h ll k (joinL lr r)
        -- lh >= rh
    joinL _ _ = error "joinL. absurd"


    joinR _          Empty       = Empty
    joinR Empty      r           = r

    joinR l@(Leaf x) r@(Leaf y)  = red 2 l (extractKey x y) r
    joinR l r@(Node c h rl k rr)
      | h == lh                  = let lm = unsafeMax l
                                       rm = unsafeMin rl
                                   in balance Red (h+1) l (extractKey lm rm) r
      | otherwise                = balance c h (joinR l rl) k rr
        -- lh >= rh
    joinR _ _ = error "joinR absurd"


--------------------------------------------------------------------------------
-- | Splitting and extracting

-- | A pair that is strict in its first argument and lazy in the second.
data Pair a b = Pair { fst' :: !a
                     , snd' :: b
                     } deriving (Show,Eq,Functor,Foldable,Traversable)


collect        :: b -> [Pair a b] -> Pair [a] b
collect def [] = Pair [] def
collect _   xs = Pair (map fst' xs) (snd' $ last xs)


-- | Extract a prefix from the tree, i.e. a repeated 'minView'
--
-- \(O(\log n +k)\), where \(k\) is the size of the extracted part
extractPrefix                      :: BalBST k a -> [Pair a (Tree k a)]
extractPrefix (BalBST n@Nav{..} t) = extractPrefix' t
  where
    extractPrefix' Empty            = []
    extractPrefix' (Leaf x)         = [Pair x Empty]
    extractPrefix' (Node _ _ l _ r) = ls ++ extractPrefix' r
      where
        ls = map (fmap $ flip (joinWith n) r) $ extractPrefix' l

-- | Extract a suffix from the tree, i.e. a repeated 'minView'
--
-- \(O(\log n +k)\), where \(k\) is the size of the extracted part
extractSuffix                      :: BalBST k a -> [Pair a (Tree k a)]
extractSuffix (BalBST n@Nav{..} t) = extract t
  where
    extract Empty            = []
    extract (Leaf x)         = [Pair x Empty]
    extract (Node _ _ l _ r) = rs ++ extract l
      where
        rs = map (fmap $ joinWith n l) $ extract r

-- | Result of splititng a tree
data Split a b = Split a !b a deriving (Show,Eq)

-- | Splits the tree at x. Note that if x occurs more often, no guarantees are
-- given which one is found.
--
-- \(O(\log n)\)
split                        :: Eq a => a -> BalBST k a -> Split (Tree k a) (Maybe a)
split x (BalBST n@Nav{..} t) = split' t
  where
    split' Empty                  = Split Empty Nothing Empty
    split' l@(Leaf y)
      | x == y                    = Split Empty (Just y) Empty
      | goLeft x (extractKey x y) = Split l     Nothing  Empty
      | otherwise                 = Split Empty Nothing  l
    split' (Node _ _ l k r)
      | goLeft x k                = let Split l' mx r' = split' l
                                    in Split l' mx (joinWith n r' r)
      | otherwise                 = let Split l' mx r' = split' r
                                    in Split (joinWith n l l') mx r'

-- | split based on a monotonic predicate
--
-- \(O(\log n)\)
splitMonotone                        :: (a -> Bool) -> BalBST k a
                                     -> (BalBST k a, BalBST k a)
splitMonotone p (BalBST n@Nav{..} t) = bimap (BalBST n) (BalBST n) $ split' t
  where
    split' Empty        = (Empty,Empty)
    split' l@(Leaf y)
      | p y             = (Empty,l)
      | otherwise       = (l,Empty)
    split' (Node _ _ l _ r)
      | p (unsafeMin r) = let (l',m) = split' l in (l',joinWith n m r)
      | otherwise       = let (m,r') = split' r in (joinWith n l m, r')


-- | Splits at a given monotone predicate p, and then selects everything that
-- satisfies the predicate sel.
splitExtract           :: (a -> Bool) -> (a -> Bool) -> BalBST k a
                       -> Split (BalBST k a) ([a],[a])
splitExtract p sel bst = Split (BalBST n before) (reverse mid1,mid2) (BalBST n after)
  where
    n                = nav bst
    (before',after') = splitMonotone p bst

    extract def = collect def . L.takeWhile (sel . fst')

    Pair mid1 before = extract (toTree before') $ extractSuffix before'
    Pair mid2 after  = extract (toTree after')  $ extractPrefix after'


--------------------------------------------------------------------------------


data T k a = Internal !Color !Height !k | Val !a deriving (Show,Eq,Ord)

toRoseTree :: Tree k a -> Maybe (T.Tree (T k a))
toRoseTree Empty            = Nothing
toRoseTree (Leaf x)         = Just $ T.Node (Val x) []
toRoseTree (Node c h l k r) = Just $ T.Node (Internal c h k) (mapMaybe toRoseTree [l,r])


showTree :: (Show k, Show a) => BalBST k a -> String
showTree = maybe "Empty" T.drawTree . fmap (fmap show) . toRoseTree . toTree

-- | Get the minimum in the tree. Errors when the tree is empty
--
-- \(O(\log n)\)
unsafeMin                  :: Tree k a -> a
unsafeMin (Leaf x)         = x
unsafeMin (Node _ _ l _ _) = unsafeMin l
unsafeMin _                = error "unsafeMin: Empty"

-- | Get the maximum in the tree. Errors when the tree is empty
--
-- \(O(\log n)\)
unsafeMax                  :: Tree k a -> a
unsafeMax (Leaf x)         = x
unsafeMax (Node _ _ _ _ r) = unsafeMax r
unsafeMax _                = error "unsafeMax: Empty"

-- | Extract all elements in the tree
--
-- \(O(n)\)
toList :: BalBST k a -> [a]
toList = toList' . toTree

-- | Extract all elements in the tree
--
-- \(O(n)\)
toList'                  :: Tree k a -> [a]
toList' Empty            = []
toList' (Leaf x)         = [x]
toList' (Node _ _ l _ r) = toList' l ++ toList' r


--------------------------------------------------------------------------------
-- * Helper stuff

black :: Height -> Tree k a -> k -> Tree k a -> Tree k a
black = Node Black

red :: Height -> Tree k a -> k -> Tree k a -> Tree k a
red = Node Red


blacken                    :: Tree k a -> Tree k a
blacken (Node Red h l k r) = Node Black h l k r
blacken t                  = t

-- | rebalance the tree
balance  :: Color -> Height -> Tree k a -> k -> Tree k a -> Tree k a
balance Black h (Node Red _ (Node Red _ a x b) y c) z d = mkNode h a x b y c z d
balance Black h (Node Red _ a x (Node Red _ b y c)) z d = mkNode h a x b y c z d
balance Black h a x (Node Red _ (Node Red _ b y c) z d) = mkNode h a x b y c z d
balance Black h a x (Node Red _ b y (Node Red _ c z d)) = mkNode h a x b y c z d
balance co h a x b                                      = Node co h a x b

mkNode                 :: Height
                       -> Tree k a -> k -> Tree k a -> k -> Tree k a  -> k -> Tree k a
                       -> Tree k a
mkNode h a x b y c z d = red h (black h a x b) y (black h c z d)

height                  :: Tree k a -> Height
height Empty            = 0
height (Leaf _)         = 1
height (Node _ h _ _ _) = h
