{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec where


import           Control.Lens
import           Control.Monad.State
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe)
import           Data.Semigroup.Traversable.Class (sequence1)
import           Data.Traversable
import           Golden
import           HGeometry.Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
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
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

import           HGeometry.Instances ()


import           Debug.Trace
import Data.Functor.Classes
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

deriving instance ( Show vertex, Show r, HasFromFoldable1 f
                  , Point_ vertex 2 r, VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                  ) => Show (HalfPlanePolygonIntersection f r vertex)
deriving instance ( Eq vertex, Eq r, HasFromFoldable1 f, Eq1 f
                  , Point_ vertex 2 r, VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                  ) => Eq (HalfPlanePolygonIntersection f r vertex)


type instance Intersection (HalfPlane line) (SimplePolygonF f point) =
  [HalfPlanePolygonIntersection f (NumType point) point]

type instance Intersection (HalfPlane line :+ extra) (SimplePolygonF f point :+ extra') =
  Intersection (HalfPlane line) (SimplePolygonF f point)


-- FIX: we halfplane polygon intersection should again be a convex polygon
type instance Intersection (HalfPlane line) (ConvexPolygonF f point) =
  Maybe (HalfPlanePolygonIntersection f (NumType point) point)

type instance Intersection (HalfPlane line :+ extra) (ConvexPolygonF f point :+ extra') =
  [HalfPlanePolygonIntersection f (NumType point) point]

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

-- | A traversal that associates every elemnt with its successor
withCyclicSuccessor :: forall cyclic a b. Traversable1 cyclic
                    => Traversal1 (cyclic a) (cyclic b) (a,a) b
withCyclicSuccessor = \f xs -> let x0 :| xs' = toNonEmpty xs
                               in sequence1 $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs

-- | An indexed version of 'withCyclicSuccessor'
iWithCyclicSuccessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                     => IndexedTraversal1 (i,i) (cyclic a) (cyclic b) (a,a) b
iWithCyclicSuccessor = conjoined withCyclicSuccessor (theITrav . indexed)
  where
    theITrav      :: Apply f => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let x0 :| xs' = toNonEmpty $ imap ((,)) xs
                    in sequence1 $ izipWithList (\(j,s) i x -> f (i,j) (x,s)) (xs' <> [x0]) xs


  -- f xs -> let x0 :| xs' = toNonEmpty xs
  --                              in sequence1 $ zipWithList (\s x -> f (x,s)) (xs' <> [x0]) xs


  -- conjoined theTrav (theITrav . indexed)
  -- where
  --   theTrav      :: Apply f
  --                => ((a,a) -> f b) -> cyclic a -> f (cyclic b)
  --   theTrav f xs =


-- | A traversal that associates every elemnt with its predecessor
withCyclicPredecessor :: forall cyclic a b. Traversable1 cyclic
                      => Traversal1 (cyclic a) (cyclic b) (a,a) b
withCyclicPredecessor = \f xs -> let xs' = toList xs
                                 in sequence1 $ zipWithList (\p x -> f (p,x)) (xs^.last1 : xs') xs


-- | An indexed traversal that associates every elemnt with its predecessor
iWithCyclicPredecessor :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                       => IndexedTraversal1 (i,i) (cyclic a) (cyclic b) (a,a) b
iWithCyclicPredecessor = conjoined withCyclicPredecessor (theITrav . indexed)
  where
    theITrav      :: Apply f => ((i,i) -> (a,a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs' = imap ((,)) xs
                    in sequence1 $
                       izipWithList (\(i,p) j x -> f (i,j) (p,x)) (xs'^.last1 : toList xs') xs

-- | Traverse a cyclic structure together with both its neighbors
withCyclicNeighbours :: forall cyclic a b. Traversable1 cyclic
                     => Traversal1 (cyclic a) (cyclic b) (a, Vector 2 a) b
withCyclicNeighbours = \f xs -> let x0 :| xs' = toNonEmpty xs
                                    ns        = zipWith Vector2 (xs^.last1 : x0 : xs') (xs' <> [x0])
                                in sequence1 $ zipWithList (\s x -> f (x,s)) ns xs

-- | An indexed traversal that associates every elemnt with its neighbours
iWithCyclicNeighbours :: forall cyclic i a b. (Traversable1 cyclic, TraversableWithIndex i cyclic)
                      => IndexedTraversal1 (i,Vector 2 i) (cyclic a) (cyclic b) (a,Vector 2 a) b
iWithCyclicNeighbours = conjoined withCyclicNeighbours (theITrav . indexed)
  where
    theITrav      :: Apply f
                  => ((i,Vector 2 i) -> (a,Vector 2 a) -> f b) -> cyclic a -> f (cyclic b)
    theITrav f xs = let xs'       = imap ((,)) xs
                        y0 :| ys' = toNonEmpty xs'
                        ns        = zipWith Vector2 (xs'^.last1 : toList xs') (ys' <> [y0])
                    in sequence1 $ izipWithList (\(Vector2 (h,p) (j,s)) i x
                                                  -> f (i, Vector2 h j) (x,Vector2 p s)) ns xs

-- type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s


  -- forall p f. (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s

-- withCyclicNeighbours' :: forall cyclic a. Traversable1 cyclic
--                     => Fold1 (cyclic a) (a, Vector 2 a)
-- withCyclicNeighbours' = \aFa xs -> phantom $ withCyclicNeighbours aFa xs






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
test3 = runIdentity $ withCyclicNeighbours pure (NonEmpty.fromList "abcde")

-- itest :: NonEmpty ((Int, Vector 2 Int), (Char, Vector 2 Char))
-- itest = runIdentity $ withCyclicNeighbours (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")


-- test4 = runIdentity $ (withCyclicNeighbours.withIndex) (Indexed $ \i x -> pure (i,x)) (NonEmpty.fromList "abcde")

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






-- | Groups the elements of a cyclic. Note in particular that this may join the first and
-- last group, thereby changing the indices of the individual elements.
--
-- the items are reported in the same order as before.
groupWith      :: (Foldable1 cyclic, Eq b)
               => (a -> b) -> cyclic a -> Cyclic NonEmpty (b, NonEmpty a)
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


deriving newtype instance Arbitrary (f a) => Arbitrary (Cyclic f a)


groupSpec = describe "cyclic groupWith tests" $ do
              it "manual" $ do
                let
                  input = cyclic [0,2,1,2,2,3,3,4,4,5,5,5,5,66,666,00,2,10,1,20]
                  ans   = cyclic [ (True, NonEmpty.fromList [20,0,2])
                                 , (False, NonEmpty.fromList [1])
                                 , (True, NonEmpty.fromList [2,2])
                                 , (False, NonEmpty.fromList [3,3])
                                 , (True, NonEmpty.fromList [4,4])
                                 , (False, NonEmpty.fromList [5,5,5,5])
                                 , (True, NonEmpty.fromList [66,666,0,2,10])
                                 , (False, NonEmpty.fromList [1])
                                 ]
                groupWith even input `shouldBe` ans
              prop "sameOrder" $
                \(xs :: Cyclic NonEmpty (Int,Char)) ->
                  (flatten (groupWith (even . fst) xs)) `isShiftOf` xs

  where
    cyclic  = Cyclic . NonEmpty.fromList
    flatten = Cyclic . foldMap1 snd

--------------------------------------------------------------------------------

-- | Convert a traversal into a fold.
asFold1   :: Traversal1 s t a b -> Fold1 s a
asFold1 t = \aFa -> phantom . t (phantom . aFa)

-- | Collect the connected components
collectComponents   :: forall cyclic f vertex r. ( Point_ vertex 2 r, Ord r, Fractional r
                       , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                       , Traversable1 cyclic, HasFromFoldable1 f

                       , Show r, Show vertex
                       )
                    => LinePV 2 r -- ^ the bounding line of the halfplane
                    -> cyclic (Bool, NonEmpty vertex)
                    -> [HalfPlanePolygonIntersection f r vertex]
collectComponents l = foldMapOf (asFold1 withCyclicNeighbours) f
  where
    -- We go through the components with their neighbours. Each component
    -- is a non-empty list of vertices in CCW order along the polygon.
    --
    -- For each component [v1,..,vn] we may need to add two vertices; the intersection
    -- point of l with the edge between the last vertex um of the previous component and
    -- v1, and the intersection point of l with the edge between vn and the first vertex
    -- w1 of the next component
    --
    f :: ((Bool, NonEmpty vertex), Vector 2 (Bool, NonEmpty vertex))
      -> [HalfPlanePolygonIntersection f r vertex]
    f x | traceShow x False = undefined

    f ((b, current@(v1 :| rest)), Vector2 (_, NonEmpty.last -> um) (_, w1 :| _))
      | not b     = []
      | otherwise = let vn     = NonEmpty.last current
                        extras = mapMaybe (intersectionPoint l) [(vn,w1), (um,v1)]
                    in pure $ case (NonEmpty.nonEmpty extras,NonEmpty.nonEmpty rest) of
                       (Nothing, Nothing)        -> HalfPlane_x_SimplePolygon_Vertex v1
                       (Nothing, Just (p :| [])) -> HalfPlane_x_SimplePolygon_Edge
                                                  $ ClosedLineSegment p v1
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


-- flatten :: NonEmpty (NonEmpty (a, b)) -> NonEmpty (b, NonEmpty a)
-- flatten = fmap (\((x,b) :| xs) -> (b, x :| map fst xs))


instance ( Point_ vertex 2 r, Fractional r, Ord r, VertexContainer f vertex
         , VertexContainer f (OriginalOrExtra vertex (Point 2 r))
         , HasFromFoldable1 f
         , Show r, Show vertex
         ) => IsIntersectableWith (HalfPlane (LinePV 2 r)) (SimplePolygonF f vertex) where
  halfPlane `intersect` poly = collectComponents (halfPlane^.boundingHyperPlane)
                             . traceShowWith ("groups",)
                             . groupWith (\v -> (v^.asPoint) `intersects` halfPlane) . Cyclic
                             . traceShowWith ("vertices",)
                             $ toNonEmptyOf vertices poly



-- instance ( HasIntersectionWith (HalfPlane line) (SimplePolygonF f vertex)
--          ) => HasIntersectionWith (HalfPlane line :+ extra)
--                                   (SimplePolygonF f vertex :+ extra') where
--   (halfPlane :+ _) `intersects` (poly :+ _) = halfPlane `intersects` poly

instance ( IsIntersectableWith (HalfPlane line) (SimplePolygonF f vertex)
         , HasIntersectionWith (HalfPlane line :+ extra) (SimplePolygonF f vertex :+ extra')
         ) => IsIntersectableWith (HalfPlane line :+ extra)
                                  (SimplePolygonF f vertex :+ extra') where
  (halfPlane :+ _) `intersect` (poly :+ _) = halfPlane `intersect` poly





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
                 [osp|polygonHalfspaceIntersection.out|]
         testIpe [osp|polygonHalfspaceIntersection1.ipe|]
                 [osp|polygonHalfspaceIntersection1.out|]
         testIpe [osp|convexHalfspaceIntersection.ipe|]
                 [osp|convexHalfspaceIntersection.out|]

         groupSpec


loadInputs      :: OsPath -> IO ( NonEmpty (HalfPlane (LinePV 2 R) :+ _)
                                , NonEmpty (SimplePolygon (Point 2 R) :+ _)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _))     = NonEmpty.fromList $ readAll page
            (pgs  :: NonEmpty (SimplePolygon (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfpalne of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, pgs)

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (halfPlanes, polygons) <-  runIO $ loadInputs inFp

    for_ polygons $ \polygon ->
      for_ halfPlanes $ \halfPlane -> do
        it ("intersects halfplane and polygon") $
          (halfPlane `intersects` polygon) `shouldBe` True -- TODO; fix
        it ("intersect halfplane and polygon propper polygons") $ do
           for_ (halfPlane `intersect` polygon) $ \case
             HalfPlane_x_SimplePolygon_Vertex _     -> pure ()
             HalfPlane_x_SimplePolygon_Edge _       -> pure ()
             HalfPlane_x_SimplePolygon_Polygon poly ->
               fromPoints @(SimplePolygon _) (toNonEmptyOf vertices poly)
               `shouldSatisfy` isJust

    let forMap'  = flip foldMap
        content' = forMap' polygons $ \polygon ->
                      forMap' halfPlanes $ \halfPlane ->
                         [ iO' polygon
                         , iO $ defIO (halfPlane^.core)
                         , iO $ ipeGroup [ renderComponent comp
                                         | comp <- halfPlane `intersect` polygon
                                         ]
                         ]
    describe "compute intersection" $
      goldenWith [osp|data/test-with-ipe/Polygon/Simple/|]
                 (ipeContentGolden { name = outFp })
                 content'
    pure ()


renderComponent :: forall vertex f r.
                   ( Point_ vertex 2 r, Foldable1 f
                   , HasVertices (SimplePolygonF f (OriginalOrExtra vertex (Point 2 r)))
                                 (SimplePolygonF f (Point 2 r))
                   --  VertexContainer f (OriginalOrExtra vertex (Point 2 r))
                   -- , VertexContainer f (Point 2 r)
                   )
                => HalfPlanePolygonIntersection f r vertex -> IpeObject r
renderComponent = \case
    HalfPlane_x_SimplePolygon_Vertex v     -> iO $ defIO (v^.asPoint)
                                                 ! attr SStroke red
    HalfPlane_x_SimplePolygon_Edge e       -> iO $ defIO (view asPoint <$> e)
                                                 ! attr SStroke red
    HalfPlane_x_SimplePolygon_Polygon poly -> iO $ ipeSimplePolygon @f (poly&vertices %~ getPt)
                                                 ! attr SFill red
  where
    getPt = \case
      Original v -> v^.asPoint
      Extra p    -> p


instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (HalfSpaceF (LinePV 2 r)) where
  type DefaultIpeOut (HalfSpaceF (LinePV 2 r)) = Group
  defIO = ipeHalfPlane

ipeHalfPlane :: (Show r, Fractional r, Ord r) => IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlane = ipeHalfPlaneIn defaultBox

ipeHalfPlaneIn          :: (Ord r, Fractional r, Show r)
                        => Rectangle (Point 2 r) -> IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlaneIn rect' hl = ipeGroup [ iO $ ipeLineIn rect' (hl^.boundingHyperPlane)
                                   ]
  -- TDOO: I think we also want to display the interior
