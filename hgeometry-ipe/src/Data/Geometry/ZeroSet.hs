{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.ZeroSet where

import           Algorithms.BinarySearch
import           Control.Lens (makeLenses, (^.), (%~), (.~), (&), (^?!), ix, to)
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Directions
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine (PolyLine)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Properties
import           Data.Geometry.QuadTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Split
import qualified Data.Geometry.QuadTree.Tree as Tree
import           Data.Geometry.ZeroSet.AlternatingPath
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Tree as RoseTree
import           Data.Util

import           Data.Geometry.Ball
-- import           Debug.Trace
import qualified Debug.Trace as Debug



import           Data.Geometry.QuadTree.Draw
import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.ZeroSet
import           Data.RealNumber.Rational
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

data ZeroConfig r = ZeroConfig { _maxDepth     :: WidthIndex
                                 -- ^ smallest allowed cells in the quadTree
                               , _edgeTreshold :: r
                                 -- ^ treshold when to stop searching
                                 -- for a sign-change on an edge
                               } deriving (Show,Eq)
makeLenses ''ZeroConfig

defaultZeroConfig :: Fractional r => ZeroConfig r
defaultZeroConfig = ZeroConfig (-1) 0.001 -- (-1) 0.001

--------------------------------------------------------------------------------

-- traceFromPrefix        ::  forall r b c e. ( RealFrac r, Ord r
--                                            , IpeWriteText r, Show r
--                                       )
--                         => ZeroConfig r
--                         -> PolyLine 2 b r -> PolyLine 2 e r
--                         -> Rectangle c r
--                         -> Maybe (PolyLine 2 () r)
-- traceFromPrefix cfg s t = traceBisector cfg (f s) (f t)
--   where


-- traceBisectorDisks          :: forall r c b e. ( RealFrac r, Ord r
--                                                , IpeWriteText r, Show r
--                                       )
--                             => ZeroConfig r
--                            -> Disk b r -> Disk e r
--                            -> Rectangle c r
--                            -> Maybe (PolyLine 2 () r)
-- traceBisectorDisks cfg s t = traceBisector cfg (f s) (f t)
--   where
--     f                   :: Disk z r -> Point 2 r :+ Double
--     f (Disk (c :+ _) r) = c :+ (negate . sqrt $ realToFrac r)


class ToWeightedPoint t w where
  -- | Converts the t into a point and an additive weight on the
  -- Euclidean distance to this point. For example, a disk with center
  -- c and radius r would be converted into c :+ (-r)
  toWeightedPoint :: t -> Point 2 (NumType t) :+ w

instance (Floating w, Real r) => ToWeightedPoint (Disk a r) w where
  toWeightedPoint (Disk (c :+ _) r) = c :+ (negate . sqrt $ realToFrac r)

instance Num w => ToWeightedPoint (Point 2 r) w where
  toWeightedPoint = (:+ 0)

instance ToWeightedPoint (Point 2 r :+ w) w where
  toWeightedPoint = id

instance (Floating w, Real r) => ToWeightedPoint (PolyLine 2 p r) w where
  toWeightedPoint p = let v = p^.PolyLine.points.to LSeq.last in v&extra .~ euclideanLength p
    where
      euclideanLength = sum . fmap segmentLength . PolyLine.edgeSegments . second realToFrac

-- | Given two weigthed points, traces an approximation of their bisector.
--
-- Note that using the 'ToWeightedPoint' class this also allows us to
-- easily trace bisectors of other geometric objects.
--
traceBisector         ::  ( RealFrac r, Ord r
                          , Floating w, Ord w
                          , IpeWriteText r, Show r
                          )
                      => ZeroConfig r
                      -> Point 2 r :+ w -> Point 2 r :+ w
                      -> Rectangle c r
                      -> Maybe (PolyLine 2 () r)
traceBisector cfg s t = traceZero cfg f (ClosedLineSegment s t)
  where
    f q = dist s q - dist t q
    dist (p :+ w) q = w + euclideanDist (realToFrac <$> p) (realToFrac <$> q)

--------------------------------------------------------------------------------

traceZero       :: (Eq a, Num a, RealFrac r, Ord r
                   , Show r, IpeWriteText r)
                => ZeroConfig r
                -> (Point 2 r -> a)
                -> LineSegment 2 b r -- ^ somewhere on this segment there should
                -- be a single transition where the sign changes
                -> Rectangle c r -- ^ bounding box
                -> Maybe (PolyLine 2 () r)
traceZero cfg f = traceZero' cfg (fromSignum f) Zero

-- | Trace the zeroset
traceZero'                 :: (Eq Sign, RealFrac r, Ord r
                              , Show r, Show Sign, IpeWriteText r)
                            => ZeroConfig r
                           -> (Point 2 r -> Sign)
                           -> Sign -- ^ zero value
                           -> LineSegment 2 b r -- ^ somewhere on this segment there should
                           -- be a single transition where the sign changes
                           -> Rectangle c r -- ^ bounding box
                           -> Maybe (PolyLine 2 () r)
traceZero' cfg f zero' s b = do startCell <- findLeaf startPoint qt
                                toPolyLineWith findZero $ trace startCell
  where
    findZero   = findZeroOnEdgeWith (cfg^.edgeTreshold) f
    findZero0  = findZeroOnEdgeWith (cfg^.edgeTreshold / 1000000) f
                 -- to find the starting point it is really important
                 -- that we actually find a zero cell. Therefor, be
                 -- much more precise than normally when tracing the
                 -- curve. Ideally, we would have a better way of
                 -- doing this than just this hack.

    s'         = s&endPoints %~ \(pt :+ _) -> pt :+ f pt -- annotate s with the sign
    qt         = trX b $ fromZerosWith' (limitWidthTo $ cfg^.maxDepth) (fitsRectangle b) f
    startPoint = tr "startPoint" $ findZero0 s'

    trace startCell' = case alternatingFromTo . trace' $ startCell' of
                         Singleton _                    -> Singleton startPoint
                         FromTo (_ :+ st) es (_ :+ tgt) -> FromTo (midPoint st) es (midPoint tgt)

    trace' startCell' = withCorners $ explorePathWith (const True) startCell' zCells

    -- cells that are zero
    zCells = NonEmpty.filter (\(p' :+ _) -> isZeroCell zero' p') . leaves . withCells $ qt

--------------------------------------------------------------------------------
-- * Tracing cells

-- | Given a list of cells, computes for each cell its neighboring
withNeighbours    :: (Fractional r, Ord r)
                  => [p :+ Cell r] -> [(p :+ Cell r) :+ Sides [p :+ Cell r]]
withNeighbours cs = map (\c@(_ :+ me) -> c :+ neighboursOf me cs) cs

-- | Computes the neighbours of a given cell from the given list
neighboursOf    ::  (Foldable t, Fractional r, Ord r)
                =>  Cell r -> t (p :+ Cell r) -> Sides [p :+ Cell r]
neighboursOf me = foldMap (\c -> fmap maybeToList $ c `relationTo` me)

-- | Given a cell and a quadTree, finds the cells representing leaves
-- adjacent to the given cell.
leafNeighboursOf   :: (Fractional r, Ord r) => Cell r -> QuadTree v p r -> Sides [p :+ Cell r]
leafNeighboursOf c = neighboursOf c . Tree.leaves . withCellsTree

--------------------------------------------------------------------------------

-- | edges of type e vertices of type v
data ExplorationTree e v = Root v [RoseTree.Tree (e :+ v)] deriving (Show,Eq)


-- | construct a tree traversal of the cells
exploreWith                :: forall p r. (Ord r, Fractional r, Show r, Show p)
                           => (p :+ Cell r -> Bool) -- ^ continue exploring?
                           -> p :+ Cell r -- ^ start
                           -> [p :+ Cell r] -- ^ all cells
                           -> ExplorationTree CardinalDirection (p :+ Cell r)
exploreWith p start' cells = tr "tree: " $ go0 start'
  where
    cs   :: [(p :+ Cell r) :+ Sides [p :+ Cell r]]
    cs   = withNeighbours cells

    -- initially, just explore everyone
    go0      c = Root c [ go c n | n <- neighboursOf' (c^.extra), p (n^.extra) ]

    -- explore only the nodes other than the one we just came from
    go pred' r@(_ :+ c) =
      RoseTree.Node r [ go c n | n <- neighboursOf' (c^.extra), continue pred' (n^.extra)]

    continue pred' n = (n^.extra) /= (pred'^.extra) && p n

    neighboursOf'   :: Cell r -> [CardinalDirection :+ (p :+ Cell r)]
    neighboursOf' c = case List.find (\n -> n^.core.extra == c) cs of
                       Nothing       -> Debug.trace "empty" []
                       Just (_ :+ s) -> concat $ (\d -> map (d :+)) <$> sideDirections <*> s




-- | Explores into a maximal path; If there are somehow multiple
-- choices to take (except at the initial step) we take only the first
-- one.
explorePathWith          :: (Ord r, Fractional r, Show r, Show p)
                         => ((p :+ Cell r) -> Bool) -- ^ continue exploring?
                         -> (p :+ Cell r) -> [p :+ Cell r]
                         -> AlternatingPath CardinalDirection (p :+ Cell r)
explorePathWith p start' = toPath . exploreWith p start'
  where
    -- toPath (Root s chs) = AlternatingPath s $ onFirstChild toPath' chs
    toPath (Root s chs) = case (AlternatingPath s . toPath') <$> chs of
        []         -> AlternatingPath s []
        [p']       -> p'
        (c1:c2:_)  -> let AlternatingPath t ys  = first oppositeDirection $ reversePath c1
                          AlternatingPath _ ys' = c2
                      in AlternatingPath t (ys <> ys')

    toPath' (RoseTree.Node x chs) = (x:) $ onFirstChild toPath' chs
    onFirstChild f = \case
      []    -> []
      (c:_) -> f c

--------------------------------------------------------------------------------

-- | Labels the edges of an alternating path of edges and quadtree
-- cells with the signs of the corners.
withCorners                        :: forall r sign. (Fractional r, Ord r)
                                   => AlternatingPath CardinalDirection      (Signs sign :+ Cell r)
                                   -> AlternatingPath (LineSegment 2 sign r) (Signs sign :+ Cell r)
withCorners (AlternatingPath v xs) = AlternatingPath v $ map f xs
  where
    f (d :+ z@(ss :+ c)) = s :+ z
      where
        d' = oppositeDirection d
        s' = (cellSides c)^?!ix d'
        s  = case ss of
               Left cs -> let Two a b = cornersInDirection d' cs
                          in s'&endPoints.extra .~ a
                               &end.extra       .~ b
               Right p -> s'&endPoints.extra .~ p


-- | Computes the line segments representing the sides of the cells in the path
withEdges             :: Fractional r
                      => AlternatingPath CardinalDirection (v :+ Cell r)
                      -> AlternatingPath (LineSegment 2 v r) (v :+ Cell r)
withEdges (AlternatingPath s xs) = AlternatingPath s $ map (\x -> x&core .~ f x) xs
  where
    f (d :+ (v :+ c)) = let e = (cellSides c)^?!ix (oppositeDirection d)
                        in first (const v) e -- store v's in the asociated data




-- | Turns a path into a polyline
toPolyLineWith            :: Fractional r
                          => (LineSegment 2 p r -> Point 2 r)
                          -> FromTo (LineSegment 2 p r) (Point 2 r)
                          -> Maybe (PolyLine 2 () r)
toPolyLineWith findZero p = PolyLine.fromPoints . map ext . ptsOf $ p
  where
    ptsOf = concat' . first findZero
    concat' = \case
      Singleton s   -> [s]
      FromTo s es t -> [s] <> NonEmpty.toList es <> [t]

--------------------------------------------------------------------------------

-- | Given a line segment with different endpoints, find the flipping point
findZeroOnEdgeWith         :: (Fractional r, Ord r, Eq sign) -- , Show r, Show sign)
                           => r -- ^ treshold on when to give up
                           -> (Point 2 r -> sign)
                           -> LineSegment 2 sign r -> Point 2 r
findZeroOnEdgeWith eps f s = interpolate tStar s
  where
    p t   = s^.end.extra == f (interpolate t s)
    tStar = binarySearchUntil eps p 0 1


--------------------------------------------------------------------------------

tr s x = Debug.traceShow (s,x) x

-- tr ::  (IpeWriteText r, Fractional r, Ord r) =>
--        QuadTree v (Either (Corners Sign) Sign) r -> QuadTree v (Either (Corners Sign) Sign) r
trX b qt = unsafePerformIO $
        do
          writeIpeFile "/tmp/debug.ipe" . singlePageFromContent $
            [ iO $ drawQuadTreeWith (drawZeroCell) qt
            , iO $ defIO b
            ]
          pure qt





-- test' :: IO ()
-- test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent $
--         [ iO $ drawQuadTreeWith (drawZeroCell @R) qt ! attr SLayer "qt"
--           iO $ defIO d1 ! attr SLayer "d1"
--         , iO $ defIO d2 ! attr SLayer "d2"
--         , iO $ defIO pl ! attr SLayer "pl"
-- --        , iO $ quadTreeLevels drawCell' qt
--         ]
--   where
--     -- f   :: Point 2 R -> R
--     -- f q = (r^2) - squaredEuclideanDist origin (realToFrac <$> q)
--     -- r = 90.5 :: R -- draw circle of radius r

--     -- Just pl = traceZero' cfg (fromSignum f) Zero startSeg rect

--     d1 = Disk (ext origin)          400
--     d2 = Disk (ext $ Point2 100 50) 25

--     Just pl = traceBisectorDisks cfg d1 d2 rect

--     startSeg :: LineSegment 2 () R
--     startSeg = ClosedLineSegment (ext $ origin) (ext $ Point2 100 50)
--     rect     :: Rectangle () Double
--     -- rect     = box (ext $ Point2 64 0) (ext $ Point2 96 32)
--     rect     = box (ext $ Point2 (-100) (-100)) (ext $ Point2 100 100)
--     -- qt = fromZerosWith' (limitWidthTo $ cfg^.maxDepth) (fitsRectangle rect) (fromSignum f)

--     cfg = defaultZeroConfig
