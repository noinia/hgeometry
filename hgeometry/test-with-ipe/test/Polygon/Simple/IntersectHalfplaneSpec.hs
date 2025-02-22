{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec where


import           Control.Lens
import           Control.Monad.State
import           Data.Foldable (toList)
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, catMaybes, mapMaybe)
import           Data.Semigroup.Foldable (sequenceA1_)
import           Data.Semigroup.Traversable.Class (sequence1)
import           Data.Traversable
import           Golden
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.PointAndVector
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Vector
import           Ipe
import           System.OsPath
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5


--------------------------------------------------------------------------------
-- to ipe
instance (Fractional r, Ord r, Show r) => HasDefaultFromIpe (HalfLine (Point 2 r)) where
  type DefaultFromIpe (HalfLine (Point 2 r)) = Path
  defaultFromIpe = _asHalfLine

-- | Try to parse an Line segment with an arrow head as a HalfLine
_asHalfLine :: (Fractional r, Ord r, Show r)
            => Prism' (IpeObject r) (HalfLine (Point 2 r) :+ IpeAttributes Path r)
_asHalfLine = prism' (\(hl :+ ats) -> IpePath (ipeHalfLine hl ! ats)) objToHalfLine
  where
    objToHalfLine = \case
      IpePath (path' :+ ats) -> case path'^?_asClosedLineSegment  of
        Just (ClosedLineSegment s t) -> case (hasAttr SArrow ats, hasAttr SRArrow ats) of
                                          (True,False) -> Just $ HalfLine s (t .-. s) :+ ats
                                          (False,True) -> Just $ HalfLine s (s .-. t) :+ ats
                                          _            -> Nothing
        Nothing                      -> Nothing
      _                    -> Nothing

    hasAttr a = isJust . lookupAttr a

--------------------------------------------------------------------------------

type HalfPlane line = HalfSpaceF line



--------------------------------------------------------------------------------

-- | A single component of a HalfPlane SimplePolygon intersection
data HalfPlanePolygonIntersection f r vertex =
    HalfPlane_x_SimplePolygon_Vertex vertex
  | HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment vertex)
  | HalfPlane_x_SimplePolygon_Polygon (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))
  -- deriving (Show,Eq,Functor)

type instance Intersection (HalfPlane line) (SimplePolygonF f point) =
  [HalfPlanePolygonIntersection f (NumType point) point]

type instance Intersection (HalfPlane line) (ConvexPolygonF f point) =
  Maybe (HalfPlanePolygonIntersection f (NumType point) point)


instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (SimplePolygonF f point) where
  halfPlane `intersects` poly = anyOf (vertices.asPoint) (`intersects` halfPlane) poly


-- data CurrentState f r vertex =
--     First (NonEmpty vertex)
--   | Later { _lastVertex          :: vertex
--           , _currentComponent    :: Maybe (CurrentComponent r vertex)
--           , _completedComponents :: [HalfPlanePolygonIntersection f r vertex]
--           , _firstComponent      :: Maybe (FirstComponent r vertex)
--           }


-- data CurrentComponent r vertex = Current { _currentVertices :: NonEmpty vertex
--                                          -- ^ vertices of the current component
--                                          -- most recent one first
--                                          , _entryPoint :: Maybe (Point 2 r)
--                                          -- ^ the first point in our component
--                                          }

-- currentVertices = lens _currentVertices (\(Current _ p) vs -> Current vs p)

                                 -- Complete (NonEmpty vertex)
  --                              -- ^ current vertices of the component
  --                              | Partial { _currentVertices :: NonEmpty vertex
  --                                        -- ^ vertices of the current component
  --                                        -- most recent one first
  --                                        , _entryPoint :: Point 2 r
  --                                        -- ^ the first point in our component
  --                                        }

-- push v = \case
--   Complete vs  -> Complete $ v NonEmpty.<| vs
--   Partial vs p -> Partial (v NonEmpty.<| vs) p

-- data FirstComponent r vertex = FirstComponent { _exitPoint        :: Maybe (Point 2 r)
--                                               -- ^ the point where we exited the component
--                                               -- if it is not a vertex
--                                               , _interiorVertices :: NonEmpty vertex
--                                               }


-- withFalses :: NonEmpty (b, NonEmpty a) -> NonEmpty (b, b, b, NonEmpty a)


  -- forall p f. (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s



--------------------------------------------------------------------------------

-- FIXME: drop the dependency on indexing. That part is boring anyway.

-- | A traversal that associates every elemnt with its successor
withCyclicSuccessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                    => IndexedTraversal1 (i, i) (cyclic a) (cyclic b) (a,a) b
withCyclicSuccessor = conjoined theTrav (theITrav . indexed)
  where
    theTrav      :: Apply f
                 => ((a,a) -> f b) -> cyclic a -> f (cyclic b)
    theTrav f xs = let x0 :| xs' = toNonEmpty xs
                   in sequence1 $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs

    theITrav      :: Apply f
                  => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let x0 :| xs' = toNonEmpty $ imap ((,)) xs
                    in sequence1 $
                       izipWithList (\(j,s) i x -> f (i,j) (x,s)) (xs' <> [x0]) xs

-- | A traversal that associates every elemnt with its predecessor
withCyclicPredecessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                      => IndexedTraversal1 (i, i) (cyclic a) (cyclic b) (a,a) b
withCyclicPredecessor = conjoined theTrav (theITrav . indexed)
  where
    theTrav      :: Apply f
                 => ((a,a) -> f b) -> cyclic a -> f (cyclic b)
    theTrav f xs = let xs' = toList xs
                   in sequence1 $ zipWithList (\p x -> f (p,x)) (xs^.last1 : xs') xs

    theITrav      :: Apply f
                  => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs' = imap ((,)) xs
                    in sequence1 $
                       izipWithList (\(i,p) j x -> f (i,j) (p,x)) (xs'^.last1 : toList xs') xs

-- | Traverse a cyclic structure together with both its neighbors
withCyclicNeighbors :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                      => IndexedTraversal1 (i,Vector 2 i) (cyclic a) (cyclic b) (a, Vector 2 a) b
withCyclicNeighbors = conjoined theTrav (theITrav . indexed)
  where
    theTrav      :: Apply f
                 => ((a,Vector 2 a) -> f b) -> cyclic a -> f (cyclic b)
    theTrav f xs = let x0 :| xs' = toNonEmpty xs
                       ns        = zipWith Vector2 (xs^.last1 : x0 : xs') (xs' <> [x0])
                   in sequence1 $ zipWithList (\s x -> f (x,s)) ns xs

    theITrav      :: Apply f
                  => ((i,Vector 2 i) -> (a,Vector 2 a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs'       = imap ((,)) xs
                        y0 :| ys' = toNonEmpty xs'
                        ns        = zipWith Vector2 (xs'^.last1 : toList xs') (ys' <> [y0])
                    in sequence1 $ izipWithList (\(Vector2 (h,p) (j,s)) i x
                                                  -> f (i, Vector2 h j) (x,Vector2 p s)) ns xs


-- -- | A fold that associates every elemnt with its successor
-- withCyclicSuccessor :: forall cyclic i a. (Traversable1 cyclic, TraversableWithIndex i cyclic)
--                     => IndexedFold1 (i, i) (cyclic a) (a,a)
-- withCyclicSuccessor = conjoined theFold (theIFold . indexed)
--   where
--     theFold      :: (Apply f, Contravariant f)
--                  => ((a,a) -> f (a,a)) -> cyclic a -> f (cyclic a)
--     theFold f xs = let x0 :| xs' = toNonEmpty xs
--                    in phantom . sequenceA1_ $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs

--     theIFold      :: (Apply f, Contravariant f)
--                   => ((i,i) -> (a,a) -> f (a,a)) -> cyclic a -> f (cyclic a)
--     theIFold f xs = let x0 :| xs' = toNonEmpty $ imap ((,)) xs
--                     in phantom . sequenceA1_ $
--                        izipWithList (\(j,s) i x -> f (i,j) (x,s)) (xs' <> [x0]) xs


-- -- | A fold that associates every element with its predecessor
-- withCyclicPredecessor :: forall cyclic i a. (Traversable1 cyclic, TraversableWithIndex i cyclic)
--                       => IndexedFold1 (i, i) (cyclic a) (a,a)
-- withCyclicPredecessor = conjoined theFold (theIFold . indexed)
--   where
--     theFold      :: (Apply f, Contravariant f)
--                  => ((a,a) -> f (a,a)) -> cyclic a -> f (cyclic a)
--     theFold f xs = let x0 :| xs' = toNonEmpty xs
--                    in phantom . sequenceA1_ $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs

--     theIFold      :: (Apply f, Contravariant f)
--                   => ((i,i) -> (a,a) -> f (a,a)) -> cyclic a -> f (cyclic a)
--     theIFold f xs = let x0 :| xs' = toNonEmpty $ imap ((,)) xs
--                     in phantom . sequenceA1_ $
--                        izipWithList (\(j,s) i x -> f (i,j) (x,s)) (xs' <> [x0]) xs


test :: NonEmpty (Char,Char)
test = runIdentity $ withCyclicSuccessor pure (NonEmpty.fromList "abcde")

test2 :: NonEmpty Char
test2 = toNonEmptyOf traverse1 (NonEmpty.fromList "abcde")


test3 :: NonEmpty (Char,Vector 2 Char)
test3 = runIdentity $ withCyclicNeighbors pure (NonEmpty.fromList "abcde")

itest :: NonEmpty ((Int, Vector 2 Int), (Char, Vector 2 Char))
itest = runIdentity $ withCyclicNeighbors (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")


test4 = runIdentity $ (withCyclicNeighbors.withIndex) (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")

-- itest = toNonEmptyOf (withCyclicSuccessor.withIndex) (NonEmpty.fromList "abcde")

-- | pre: the f and the list have the same size
zipWithList      :: forall f a b c. Traversable f
                 => (a -> b -> c) -> [a] -> f b -> f c
zipWithList f xs = flip evalState xs . traverse go
  where
    go   :: b -> State [a] c
    go y = get >>= \case
      []      -> error "zipWithList. precondition failed, too few a's"
      (x:xs') -> do put xs'
                    pure $ f x y

-- | pre: the f and the list have the same size
izipWithList      :: forall f i a b c. TraversableWithIndex i f
                 => (a -> i -> b -> c) -> [a] -> f b -> f c
izipWithList f xs = flip evalState xs . itraverse go
  where
    go     :: i -> b -> State [a] c
    go i y = get >>= \case
      []      -> error "izipWithList. precondition failed, too few a's"
      (x:xs') -> do put xs'
                    pure $ f x i y

--------------------------------------------------------------------------------

-- outerBoundaryEdges :: IndexedFold1 (VertexIx polygon,VertexIx polygon) polygon
--                                      (Vertex polygon, Vertex polygon)


-- outerBoundaryWithNeighbours :: (HasOuterBoundary polygon, VertexIx polygon ~ Int) => IndexedFold1 (VertexIx polygon, (VertexIx polygon, VertexIx polygon)) polygon (Vertex polygon, (Vertex polygon, Vertex polygon))





-- | Groups the elements of a cyclic. this may regroup them
groupWith      :: (Foldable1 v, Eq b)
               => (a -> b) -> Cyclic v a -> Cyclic NonEmpty (b, NonEmpty a)
groupWith f xs = Cyclic $ case foldrMap1 initialize compute xs of
    Left res      -> NonEmpty.singleton res
    Right ((x, first), res@((y, current) :| completed))
      | x == y    -> (x, first <> current) :| completed
      | otherwise -> (x, first) NonEmpty.<| res
  where
    initialize x = Left (f x, NonEmpty.singleton x)
    compute x = \case
        Left (y,current)
          | b == y    -> Left (y, x NonEmpty.<| current)
          | otherwise -> Right ((y,current), NonEmpty.singleton (b, NonEmpty.singleton x))
        Right (first, res@((y,current):|completed))
          | b == y    -> Right (first, (y, x NonEmpty.<| current) :| completed)
          | otherwise -> Right (first, (b, NonEmpty.singleton x) NonEmpty.<| res)
      where
        b = f x

-- -- | For the falses we just want the first and last element
-- --
-- -- i.e. they represent runs of points outside the halfplane
-- flattenFalses :: Functor f => f (Bool, NonEmpty a) -> f (Either (Vector 2 a) (NonEmpty a))
-- flattenFalses = fmap (\(b,xs) -> if b then Right xs
--                                       else Left $ Vector2 (NonEmpty.head xs) (NonEmpty.last xs)
--                      )

  --   HalfPlane_x_SimplePolygon_Vertex vertex
  -- | HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment vertex)
  -- | HalfPlane_x_SimplePolygon_Polygon (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))
  -- -- deriving (Show,Eq,Functor)


collectComponents :: ( Point_ vertex 2 r, Ord r, Fractional r
                     , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                     , Traversable1 cyclic, TraversableWithIndex i cyclic
                     )
                  => LinePV 2 r
                  -> cyclic (Bool, NonEmpty vertex)
                  -> [HalfPlanePolygonIntersection f r vertex]
collectComponents l = foldMapOf withCyclicNeighbors f
  where
    f ((b, current@(v :| rest)), Vector2 (_,NonEmpty.last -> u) (_,w :| _))
      | not b     = []
      | otherwise = let vn     = NonEmpty.last current
                        extras = mapMaybe (intersectionPoint l) [(u,vn), (v,w)]
                    in pure $ case (NonEmpty.nonEmpty extras,NonEmpty.nonEmpty rest) of
                       (Nothing, Nothing)        -> HalfPlane_x_SimplePolygon_Vertex v
                       (Nothing, Just (p :| [])) -> HalfPlane_x_SimplePolygon_Edge
                                                  $ ClosedLineSegment p v
                       (extras',  _)             -> HalfPlane_x_SimplePolygon_Polygon poly
                         where
                           poly = uncheckedFromCCWPoints
                                $ (fmap Extra <$> extras') <<> (Original <$> current)


xs <<> ys = case xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys

intersectionPoint            :: (Point_ vertex 2 r, Ord r, Fractional r)
                             => LinePV 2 r -> (vertex, vertex) -> Maybe (Point 2 r)
intersectionPoint line (u,v) = case LinePV (u^.asPoint) (v .-. u) `intersect` line of
  Just (Line_x_Line_Point p) | p /= (u^.asPoint) -> Just p
  _                                              -> Nothing
-- TODO


flatten :: NonEmpty (NonEmpty (a, b)) -> NonEmpty (b, NonEmpty a)
flatten = fmap (\((x,b) :| xs) -> (b, x :| map fst xs))


instance ( Point_ vertex 2 r, Num r, Ord r, VertexContainer f vertex, HyperPlane_ line 2 r
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , Intersection (LinePV 2 r) line ~ Maybe (LineLineIntersection line')
         , NumType line' ~ r
         , IsIntersectableWith (LinePV 2 r) line
         , HasFromFoldable1 f
         ) => IsIntersectableWith (HalfPlane line) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly =
    undefined



    -- categorize
    --                          . flatten
    --                          . NonEmpty.groupWith1 snd
    --                          $ fmap (\v -> (v, v `intersects` halfPlane))
    --                          . toNonEmptyOf vertices poly
    -- where
    --   extractComponents = undefined
    --   categorize = undefined










--     case foldrMap1 initialize compute $ toNonEmptyOf vertices poly of
--       First _                         -> [HalfPlane_x_SimplePolygon_Polygon $ Original <$> poly]
--       Later v current completed first -> combine v current first <> completed
--     where
--       initialize v
--         | (v^.asPoint) `intersects` halfPlane = First (NonEmpty.singleton v)
--         | otherwise                           = Later v Nothing [] Nothing

--       -- we maintain the current component, if it exists and the completed components.
--       -- we have to handle the first component separately, as we may have to combine the first
--       -- and last component into one.
--       compute   :: vertex -> CurrentState f r vertex -> CurrentState f r vertex
--       compute v = \case
--         First vs@(u :| _)
--           | (v^.asPoint) `intersects` halfPlane -> First (v NonEmpty.<| vs)
--           | otherwise                           ->
--               Later v Nothing [] (Just $ FirstComponent exitPoint vs)
--               where
--                 exitPoint = intersectionPoint u v

--         Later u current completed first
--           | (v^.asPoint) `intersects` halfPlane -> Later v (Just current') completed  first
--           | otherwise                           -> Later v Nothing         completed' first
--            where
--              current'   = case current of
--                Nothing -> Current (NonEmpty.singleton v) entryPoint
--                  where
--                    entryPoint = intersectionPoint u v
--                Just current'' -> current''&currentVertices %~ (v NonEmpty.<|)

--              completed' = case current of
--                Nothing        -> completed
--                Just current'' -> complete (Just v) current'' : completed

--       -- | Given a points u and v that do not both lie on the same side of the line l
--       -- bounding the halfplane, computes the intersection point of the *open* edge (u,v)
--       -- with l (if such a point exists).
--       intersectionPoint u v = case LinePV (u^.asPoint) (v .-. u)
--                                      `intersect` (halfPlane^.boundingHyperPlane) of
--         Just (Line_x_Line_Point p) | p /= (u^.asPoint) -> Just p
--         _                          -> Nothing


--       -- | Given a vertex v that does *not* lie in the halfplane, and the current state.
--       -- finish up the current component.
--       complete   :: Maybe vertex -> CurrentComponent r vertex
--                  -> HalfPlanePolygonIntersection f r vertex
--       complete v = \case
--         Current (u :| [])  Nothing -> HalfPlane_x_SimplePolygon_Vertex u
--         Current (u :| [w]) Nothing -> HalfPlane_x_SimplePolygon_Edge (ClosedLineSegment w u)
--         Current (u :| ws)  mEntry  -> HalfPlane_x_SimplePolygon_Polygon
--                                     $ case catMaybes [mEntry, intersectionPoint u =<< v] of
--            extras -> uncheckedFromCCWPoints $ (Extra <$> extras) <<> (Original <$> (u :| ws))

--       combine v mCurrent = \case
--         Nothing    -> mCurrent >>= complete Nothing
--         Just first -> case mCurrent of
--           Nothing      -> _
--           Just current -> _








                         -- (HalfSpaceF (LinePV 2 (RealNumber 5)))
                         -- (SimplePolygonF
                         --    (HGeometry.Cyclic.Cyclic
                         --       Data.Vector.NonEmpty.Internal.NonEmptyVector)
                         --    (Point 2 R))


instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfPlane line) (ConvexPolygonF f point) where
  halfPlane `intersects` poly = halfPlane `intersects` (toSimplePolygon poly)
    -- TODO there is a better, O(log n) time implementation. use that instead ...


--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]
                 [osp|polygonHalfspaceIntersection.out.ipe|]

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (halfPlanes, polygons) <-  runIO $ do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (SimplePolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfpalne of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, pgs)

    for polygons $ \polygon ->
      for halfPlanes $ \halfPlane ->
        it ("intersects halfplane and polygon") $
          (halfPlane `intersects` polygon) `shouldBe` True -- TODO; fix
    pure ()
