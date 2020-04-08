{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.ZeroSet where

import           Algorithms.BinarySearch
import           Control.Lens (makeLenses, (^.), (%~), (.~), (&), (^?!), ix)
-- import           Control.Zipper
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine (PolyLine)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Tree as RoseTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Split
import           Data.Geometry.QuadTree
import qualified Data.Geometry.QuadTree.Tree as Tree


-- import           Debug.Trace

--------------------------------------------------------------------------------

data ZeroConfig r = ZeroConfig { _maxDepth     :: WidthIndex
                                 -- ^ smallest allowed cells in the quadTree
                               , _edgeTreshold :: r
                                 -- ^ treshold when to stop searching
                                 -- for a sign-change on an edge
                               } deriving (Show,Eq)
makeLenses ''ZeroConfig


defaultZeroConfig :: Fractional r => ZeroConfig r
defaultZeroConfig = ZeroConfig (-1) 0.001


traceZero       :: (Eq a, Num a, RealFrac r, Ord r)
                => ZeroConfig r
                -> (Point 2 r -> a)
                -> LineSegment 2 b r -- ^ somewhere on this segment there should
                -- be a single transition where the sign changes
                -> Rectangle c r -- ^ bounding box
                -> Maybe (PolyLine 2 () r)
traceZero cfg f = traceZero' cfg (fromSignum f) Zero


traceZero'                 :: (Eq sign, RealFrac r, Ord r)
                            => ZeroConfig r
                           -> (Point 2 r -> sign)
                           -> sign -- ^ zero value
                           -> LineSegment 2 b r -- ^ somewhere on this segment there should
                           -- be a single transition where the sign changes
                           -> Rectangle c r -- ^ bounding box
                           -> Maybe (PolyLine 2 () r)
traceZero' cfg f zero' s b = do startCell <- findLeaf startPoint qt
                                toPolyLineWith predicate $ trace startCell
  where
    findZero   = findZeroOnEdgeWith (cfg^.edgeTreshold) f
    predicate  = undefined

    s'         = s&endPoints %~ \(pt :+ _) -> pt :+ f pt -- annotate s with the sign
    qt         = fromZerosWith' (limitWidthTo $ cfg^.maxDepth) (fitsRectangle b) f
    startPoint = findZero s'

    trace startCell' = explorePathWith (const True) startCell' zCells

    -- cells that are zero
    zCells = NonEmpty.filter (\(p' :+ _) -> isZeroCell zero' p') . leaves . withCells $ qt


--------------------------------------------------------------------------------


-- -- | Computes the line segments representing the sides of the cells in the path
-- withEdges             :: Fractional r
--                       => Path CardinalDirection (v :+ Cell r)
--                       -> Path (LineSegment 2 v r) (v :+ Cell r)
-- withEdges (Path s xs) = Path s $ map (\x -> x&core .~ f x) xs
--   where




-- traceZeroFrom'            :: forall zero r b v. (Eq zero, Fractional r, Ord r)
--                          =>
--                          -> zero -- ^ the zero value
--                          -> Point 2 r -> QuadTree v (Either b zero)
--                          -> Maybe (Path CardinalDirection ((Either b zero) :+ Cell r))
-- traceZeroFrom' zero' p qt = trace <$> findLeaf p qt
--   where
--     trace startCell = explorePathWith (const True) startCell zCells
--     zCells = NonEmpty.filter (\(p' :+ _) -> isZeroCell zero' p') . leaves . withCells $ qt





--------------------------------------------------------------------------------
-- * Tracing cells

-- | Given a list of cells, computes for each cell its neighboring
withNeighbours    :: (Fractional r, Ord r)
                  => [p :+ Cell r] -> [(p :+ Cell r) :+ Sides [p :+ Cell r]]
withNeighbours cs = map (\c@(_ :+ me) -> c :+ neighboursOf me) cs
  where
    neighboursOf me = foldMap (`relationTo` me) cs


-- | Given a cell and a quadTree, finds the cells representing leaves
-- adjacent to the given cell.
leafNeighboursOf   :: (Fractional r, Ord r) => Cell r -> QuadTree v p -> Sides [p :+ Cell r]
leafNeighboursOf c = neighboursOf c . Tree.leaves . withCellsTree
  where
    neighboursOf me = foldMap (`relationTo` me)


relationTo        :: (Fractional r, Ord r) => (p :+ Cell r) -> Cell r -> Sides [p :+ Cell r]
c `relationTo` me = f <$> Sides b l t r <*> cellSides me
  where
    Sides t r b l = cellSides (c^.extra)
    -- f e e' | traceShow ("f ",e,e',e `sideIntersects` e') False = undefined
    f e e' | e `intersects` e' = [c]
           | otherwise         = []

--------------------------------------------------------------------------------

-- | edges of type e vertices of type v
data ExplorationTree e v = Root v [RoseTree.Tree (e :+ v)] deriving (Show,Eq)

-- | construct a tree traversal of the cells
exploreWith                :: forall p r. (Ord r, Fractional r)
                           => (p :+ Cell r -> Bool) -- ^ continue exploring?
                           -> p :+ Cell r -- ^ start
                           -> [p :+ Cell r] -- ^ all cells
                           -> ExplorationTree CardinalDirection (p :+ Cell r)
exploreWith p start' cells = go0 start'
  where
    cs   :: [(p :+ Cell r) :+ Sides [p :+ Cell r]]
    cs   = withNeighbours cells

    -- initially, just explore everyone
    go0      c = Root c [ go c n | n <- neighboursOf (c^.extra), p (n^.extra) ]

    -- explore only the nodes other than the one we just came from
    go pred' r@(_ :+ c) =
      RoseTree.Node r [ go c n | n <- neighboursOf (c^.extra), continue pred' (n^.extra)]

    continue pred' n = (n^.extra) /= (pred'^.extra) && p n

    neighboursOf   :: Cell r -> [CardinalDirection :+ (p :+ Cell r)]
    neighboursOf c = case List.find (\n -> n^.core.extra == c) cs of
                       Nothing       -> []
                       Just (_ :+ s) -> concat $ (\d -> map (d :+)) <$> sideDirections <*> s


-- | A path is a non-empty alternating sequence of vertices and edges. There
-- is one more vertex than there are edges.
data Path e v = Path v [e :+ v] deriving (Show,Eq)

instance Bifunctor Path where
  bimap = bimapDefault
instance Bifoldable Path where
  bifoldMap = bifoldMapDefault
instance Bitraversable Path where
  bitraverse f g (Path s es) = Path <$> g s <*> traverse (bitraverse f g) es



-- | computes the (start vertex, the edge sequence crossed, target vertex) if it exists
-- (and otherwise just returns the single vertex in the path)
pathFromTo :: Path e v -> Either v (v,NonEmpty e,v)
pathFromTo = \case
  Path s [] -> Left s
  Path s xs -> Right (s,NonEmpty.fromList $ map (^.core) xs, (List.last xs)^.extra)



-- | Explores into a path;
--
-- if there are multiple choices where to go to, just pick the first
-- one.
explorePathWith          :: (Ord r, Fractional r)
                         => ((p :+ Cell r) -> Bool)
                         -> (p :+ Cell r) -> [p :+ Cell r]
                         -> Path CardinalDirection (p :+ Cell r)
explorePathWith p start' = toPath . exploreWith p start'
  where
    toPath (Root s chs) = Path s $ onFirstChild toPath' chs
    toPath' (RoseTree.Node x chs) = (x:) $ onFirstChild toPath' chs
    onFirstChild f = \case
      []    -> []
      (c:_) -> f c

-- | Computes the line segments representing the sides of the cells in the path
withEdges             :: Fractional r
                      => Path CardinalDirection (v :+ Cell r)
                      -> Path (LineSegment 2 v r) (v :+ Cell r)
withEdges (Path s xs) = Path s $ map (\x -> x&core .~ f x) xs
  where
    f (d :+ (v :+ c)) = let e = (cellSides c)^?!ix (oppositeDirection d)
                        in first (const v) e -- store v's in the asociated data


-- | Turns a path into a polyline
toPolyLineWith            :: Fractional r
                          => (LineSegment 2 p r -> Point 2 r)
                          -> Path CardinalDirection (p :+ Cell r)
                          -> Maybe (PolyLine 2 () r)
toPolyLineWith findZero p = PolyLine.fromPoints . map ext . ptsOf $ p
  where
    ptsOf = concat' . pathFromTo . bimap findZero (\(_ :+ c) -> midPoint c) . withEdges
    concat' = \case
      Left s         -> [s]
      Right (s,es,t) -> [s] <> NonEmpty.toList es <> [t]


toPolyLine   :: (Fractional r, Ord r)
             => (Point 2 r -> Bool)
             -> Path CardinalDirection (Signs sign :+ Cell r)
             -> Maybe (PolyLine 2 () r)
toPolyLine p = toPolyLineWith f
  where
    f = undefined







-- relTest = let qt@(QuadTree _ t) = withCellRs $ completeTree 1
--               (Node (_ :+ c) _) = t
--               (Quadrants nw ne se sw) = splitCell c
--           in (not . null) <$> (() :+ ne) `relationTo` nw
--              -- should be west


-- eastNeighbours    :: [p :+ Cell] -> [(p :+ Cell) :+ [p :+ Cell]]
-- eastNeighbours cs = map (\(_ :+ c) -> Map.lookup c ns) cs
--   where




-- | Given a line segment with different endpoints, find the flipping point
findZeroOnEdgeWith         :: (Fractional r, Ord r, Eq sign)
                           => r
                           -> (Point 2 r -> sign)
                           -> LineSegment 2 sign r -> Point 2 r
findZeroOnEdgeWith eps f s = interpolate tStar s
  where
    p t   = s^.end.extra == f (interpolate t s)
    tStar = binarySearchUntil eps p 0 1
