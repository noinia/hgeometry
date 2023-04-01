{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Class
  ( HasOuterBoundary(..)
  , signedArea2X
  , minimumVertexBy, maximumVertexBy
  , outerBoundaryEdgeSegments
  , outerBoundaryWithNeighbours

  , Polygon_(..)
  ) where

import           Control.Lens
import           Control.Lens.Internal.Fold (NonEmptyDList(..))
import           Data.Function (on)
import           Data.Functor.Apply (Apply)
import qualified Data.Functor.Apply as Apply
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup (First(..))
import           Data.Semigroup.Foldable
import           HGeometry.LineSegment
import           HGeometry.Point.Class
import           HGeometry.Vector
import           Hiraffe.Graph


--------------------------------------------------------------------------------
-- ^ A class for items that have an outer boundary.
class HasVertices polygon polygon => HasOuterBoundary polygon where
  {-# MINIMAL outerBoundaryVertexAt, ccwOuterBoundaryFrom, cwOuterBoundaryFrom #-}

  -- | A fold over all vertices of the outer boundary, the
  -- vertices are traversed in CCW order.
  --
  -- running time :: \(O(n)\)
  outerBoundary :: IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
  -- default outerBoundary :: Num (VertexIx polygon)
  --                       => IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
  -- outerBoundary = ccwOuterBoundaryFrom 0

  -- | A particular vertex of the outer polygon
  --
  -- running time: \(O(1)\)
  outerBoundaryVertexAt   :: VertexIx polygon
                          -> IndexedGetter (VertexIx polygon) polygon (Vertex polygon)

  -- | A fold over all edges in the polygon. The edges are folded over
  -- in CCW order, and each edge is associated with the index of its start vertex
  -- outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  --
  --
  -- running time :: \(O(n)\)
  outerBoundaryEdges :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)

  -- | A CCW-fold over all vertices of the outer boundary, starting
  -- from the vertex with the given index.
  --
  -- running time :: \(O(n)\)
  ccwOuterBoundaryFrom :: VertexIx polygon -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)

  -- | A CW-fold over all vertices of the outer boundary, starting
  -- from the vertex with the given index.
  --
  -- running time :: \(O(n)\)
  cwOuterBoundaryFrom        :: VertexIx polygon
                             -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
  -- cwOuterBoundaryFrom s paFa' = ifoldring outerBoundary (f)


 -- ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
  --   where
  --     f     :: VertexIx polygoon
  --           -> Vertex polygon
  --           -> f (polygon)
  --     f i v = indexed paFa i v

      -- f      :: (Indexable (VertexIx polygon) p, Contravariant f, Apply f)
      --        => p (Vertex polygon) (f (Vertex polygon))
      --        -> p (Vertex polygon) (f (Vertex polygon))
      -- f paFa =

        -- paFa

  --   where
      -- f :: (Indexable (VertexIx polygon) p, Contravariant f, Apply f)
      --   => p (Vertex polygon) (f (Vertex polygon))
      -- f = conpaFa

      --   indexed
    -- =
    -- where
    --   f :: (VertexIx polygon -> Vertex polygon -> f (Vertex polygon) -> f (Vertex polygon))
    --     -> f (Vertex polygon) -> polygon -> f (Vertex polygon)
    --   f = undefined

  --



  --   ifoldl'
  --   where
  --     n = numVertices pg
  --     ixes = [s..s+n]

-- type IndexedFold1 i s a =
--     forall p f . (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s

-- type Over p f s t a b = p a (f b) -> s -> f t

  default outerBoundaryEdges
    :: Enum (VertexIx polygon)
    => IndexedFold1 (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges = ifolding1 $
    \pg -> ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
         <$> itoNonEmptyOf outerBoundary pg
    -- \pg -> fmap ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
    --      . NonEmpty.fromList
    --      $ itoListOf outerBoundary pg
    -- this feels much more clunky than it should be somehow.
    -- I would like an 'itoNonEmptyOf'

  -- | Get the edge that has the given vertex as its starting edge
  --
  -- running time: \(O(1)\)
  outerBoundaryEdgeAt   :: VertexIx polygon
                        -> IndexedGetter (VertexIx polygon) polygon
                                         (Vertex polygon, Vertex polygon)
  -- default implementation of outerBoundaryEdge. It achieves the
  -- desired running time when indexing is indeed constant.
  default outerBoundaryEdgeAt :: Enum (VertexIx polygon)
                              => VertexIx polygon
                              -> IndexedGetter (VertexIx polygon) polygon
                                               (Vertex polygon, Vertex polygon)
  outerBoundaryEdgeAt i = ito $
    \pg -> (i, (pg^.outerBoundaryVertexAt i, pg^.outerBoundaryVertexAt (succ i)))


--------------------------------------------------------------------------------
-- end of te HasOuterBoundary class
--------------------------------------------------------------------------------


data MyState partial result = MyState (partial -> result)
                                      -- the continuation we've built that will finish the result
                              partial -- the partial result we've built so far.

firstScanF :: Ord i
           => i -- ^ starting index
           -> (i -> a -> partial) -- ^ the function we use to build a partial
           -> (partial -> partial -> partial) -- ^ the extend function
           -- the rest is kind of the real stuff
           -> i -> MyState partial result -> a -> MyState partial result
firstScanF si f extend
           i (MyState k partialRes) a
  | i < si    = MyState (\partial -> k $ partial `extend` f i a) partialRes
  | i == si   = MyState k                                        (f i a) -- initialize the partial state
  | otherwise = MyState k                                        (partialRes `extend` f i a)


-- traversal: forall applicative f => (a -> f b) -> s -> f t

traverseFrom                :: i -- ^ starting index
                            -> IndexedTraversal i s t a b
                            -> Traversal s t a b
traverseFrom si trav paFb s = mkFt partial
  where
    MyState mkFt partial = ifoldlOf trav
                                 (firstScanF si mkPartial extend)
                                 initialState
                                 s

    -- mkPartial     :: i -> a -> f b -> f t
    -- mkPartial i a = indexed paFb i a

    initialState :: MyState (a -> f b)
                            (f t)
    initialState = MyState (\g -> trav g s)
                           (error "no partial result yet")

    -- extend :: (->) a (f b) -> (->) a (f b) -> (->) a (f b)
    -- -- extend :: (a -> (f b))
    --        -- -> (a -> (f b)) -> a -> (f b)
    -- extend f g a = fmap (g ) f a


--     liftFinal = id







--     extend :: partial -> partial -> partial
--     extend res1 res2 = res1 <.>


--   case imapAccumLOf trav accF (TODO id) s of
--                                 (acc', s') ->  undefined
--   where
--     accF                  :: i -> MyState b
--                                ->
--     accF i acc v | i < si = ( case acc of
--                                 DONE r

--                      acc'
--                             , TODO v
--                             )
--                  |


-- data MyState b = DONE b
--                | TODO (b -> b)



-- data Cycl f a = Done (f a)
--               | Todo a

-- instance Functor f => Functor (Cycl f) where
--   fmap f = \case
--     Done fa -> Done (fmap f fa)
--     Todo a  -> Todo $ f a

-- instance Applicative f => Apply (Cycl f) where
--   Done f <.> Done fa = Done $ f <*> fa
--   Done f <.> Todo a  = -- seems one would need s.t. like pure, so that
-- --    we can do :
--     Done $ f <*> pure a
--     -- f :: f (a -> b)
--   Todo f <.> Done fa = Done $ fmap f fa
--     -- f :: a -> b
--     --
--   Todo f <.> Todo a = Todo $ f a


-- traverseFrom          :: i
--                       -> IndexedTraversal i s t a b
--                       -> IndexedTraversal i s t a b
-- traverseFrom s t paFa s = case itraverseOf t applyPrefix s of
--                             Todo s' -> undefined
--                             Done
--   where
--     applyPrefix :: i -> a -> Cycl f a




--                     t wrap  :: Cycl f s
--   where
--     wrap   :: a -> Cycl f a
--     wrap a = -- index < s then Todo a  -- else Done $ applyTheF a


-- data Cycl f a = Done (f a)
--               | More (f a -> f a)



-- applyF :: (Int -> a -> f a)
--        -> Int -> a -> Cycl f a
-- applyF f i v | i < s     = More $ \pref -> fmap (\x -> \y -> \x)

-- pref -- we need some way of lifting the f a to a
--                                                 -- f (a -> a)
--                                       Apply.<.> f i v


--              | otherwise = Done $ f i v




-- traverseFrom        :: Apply f => Int -> (a -> f b) -> NonEmpty a -> f (NonEmpty b)
-- traverseFrom 0 f xs = traverse1 f xs
-- traverseFrom s f (x:|xs) = case NonEmpty.fromList <$> List.splitAt (s-1) xs of
--                              (suff,Nothing)   -> traverse1 f (x:|suff)
--                              (suff,Just pref) -> combine <$> traverse1 f pref
--                                                          Apply.<.> traverse1 f (x:|suff)
--   where
--     combine pref suff = suff <> pref


-- cwOuterBoundaryFrom'   :: forall polygon. (Ord (VertexIx polygon), HasOuterBoundary polygon)
--                        => VertexIx polygon
--                        -> IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
-- cwOuterBoundaryFrom' s = outerBoundary <> outerBoundary


-- cwOuterBoundaryFrom'        :: forall polygon. (Ord (VertexIx polygon), HasOuterBoundary polygon)
--                             => VertexIx polygon
--                             -> IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
-- cwOuterBoundaryFrom' s paFb = combine
--                             . ifoldrOf outerBoundary k (undefined,undefined)
--   where
--     f                 :: VertexIx polygon
--                       -> Vertex polygon
--                       -> ( f (Vertex polygon), f (Vertex polygon) )
--                       -> ( f (Vertex polygon), f (Vertex polygon) )
--     f i v (suff,pref) = ( indexed paFb i v -- combine with suff
--                         , pref
--                         )
--           | otherwise = ( suff
--                         , indexed paFb i v
--                         )

  -- runIndexed `rmap` outerBoundary f
  -- where
  --   f :: Indexed (VertexIx polygon)
  --                (Vertex polygon)
  --                (p (Vertex polygon) (f (Vertex polygon)))
  --   f = Indexed $ \i v ->

  --   buildTraversal :: Indexed (VertexIx polygon)
  --                             (Vertex polygon)
  --                             (  IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
  --                             -> IndexedTraversal1' (VertexIx polygon) polygon (Vertex polygon)
  --                             )


-- indxedtraversal1 :: (Indexable i p, Apply f) => p a (f b) -> s -> f t




-- iadjoin
--   :: (Is k A_Traversal, Is l A_Traversal, is `HasSingleIndex` i)
--   => Optic' k is s a
--   -> Optic' l is s a
--   -> IxTraversal' i s a
-- iadjoin o1 o2 = conjoined (adjoin o1 o2) (combined % traversed % itraversed)
--   where
--     combined = traversalVL $ \f s0 ->
--       (\r1 r2 ->
--          let s1 = evalState (traverseOf o1 update s0) r1
--              s2 = evalState (traverseOf o2 update s1) r2
--          in s2
--       )
--       <$> f (itoListOf (castOptic @A_Traversal o1) s0)
--       <*> f (itoListOf (castOptic @A_Traversal o2) s0)

--     update a = get >>= \case
--       (_, a') : as' -> put as' >> pure a'
--       []            ->            pure a
-- infixr 6 `iadjoin` -- Same as (<>)
-- {-# INLINE [1] iadjoin #-}

-- iadjoin       :: ReifiedIndexedTraversal' i s a
--               -> ReifiedIndexedTraversal' i s a
--               -> ReifiedIndexedTraversal' i s a
-- iadjoin lt rt = undefined


-- TODO: upstream these to lens

                               -- -> IndexedFold (VertexIx polygon) polygon (Vertex polygon)

cwOuterBoundaryFrom''   :: forall polygon.
                          ( Ord (VertexIx polygon)
                          , HasOuterBoundary polygon
                          , Int ~ VertexIx polygon
                          )
                       => VertexIx polygon
                       -> IndexedTraversal' (VertexIx polygon) polygon (Vertex polygon)
cwOuterBoundaryFrom'' s = runIndexedTraversal $
                           (dropping s outerBoundary')
                           `iadjoin`
                           (indexedTraversal' $ taking   s outerBoundary')
  where
    outerBoundary' :: IndexedTraversal' (VertexIx polygon) polygon (Vertex polygon)
    outerBoundary' paFa = Apply.unwrapApplicative
                        . outerBoundary (rmap Apply.WrapApplicative paFa)

    indexedTraversal' = IndexedTraversal @(VertexIx polygon)




-- TODO: upstream these to lens

                               -- -> IndexedFold (VertexIx polygon) polygon (Vertex polygon)

cwOuterBoundaryFrom'   :: forall polygon. (Ord (VertexIx polygon), HasOuterBoundary polygon)
                       => VertexIx polygon
                       -> IndexedFold (VertexIx polygon) polygon (Vertex polygon)
cwOuterBoundaryFrom' s = runIndexedFold $
                           (indexedFold' $ idroppingWhile (\i _ -> i < s) outerBoundary')
                           <>
                           (indexedFold' $ itakingWhile   (\i _ -> i < s) outerBoundary')
  where
    outerBoundary' :: IndexedFold (VertexIx polygon) polygon (Vertex polygon)
    outerBoundary' paFa = Apply.unwrapApplicative
                        . outerBoundary (rmap Apply.WrapApplicative paFa)

    indexedFold' = IndexedFold @(VertexIx polygon)


instance Contravariant f => Contravariant (Apply.WrappedApplicative f) where
  contramap f (Apply.WrapApplicative x) = Apply.WrapApplicative (contramap f $ x)

-- type Fold s a = forall f. Contravariant f, Applicative f => (a -> f a) -> s -> f s

-- myFolding :: Foldable f => (s -> f a) -> Fold s a
-- -- myFolding :: Foldable f => (s -> f a) -> forall g. contravariant applicateve g. (a -> g a) -> s -> g s
-- myFolding sfa aga s = phantom . Data.Foldable.traverse_ aga $ sfa s

-- f (g a)

-- g (f a)

-- g s
-- g


--   (\(suff,pref) -> contramap suff

-- --                                   )

--   ifoldrOf outerBoundary (f paFa) (Nothing,Nothing) pg
--   where
--     f         :: p (Vertex polygon) (f (Vertex polygon))
--               -> VertexIx polygon
--               -> Vertex polygon
--               -> (f polygon, f polygon)
--               -> (f polygon, f polygon)
--     f _ i v r = undefined


-- data CCWFrom f polygon = Prefix

--
-- taken and modified directly from lens

-- | construct a Fold1 from a function that produces a Foldable1
folding1         :: Foldable1 f => (s -> f a) -> Fold1 s a
folding1 sfa agb = phantom . traverse1_ agb . sfa
{-# INLINE folding1 #-}

-- | Version of ifolding to build an 'IndexedFold1'
ifolding1       :: (Foldable1 f, Indexable i p, Contravariant g, Apply g)
                => (s -> f (i, a)) -> Over p g s t a b
ifolding1 sfa f = phantom . traverse1_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding1 #-}

-- | indexed version of 'toNonEmptyOf'
itoNonEmptyOf   :: IndexedGetting i (NonEmptyDList (i,a)) s a -> s -> NonEmpty (i,a)
itoNonEmptyOf l = flip getNonEmptyDList [] . ifoldMapOf l (\i a -> NonEmptyDList ((i,a) :|))
{-# INLINE itoNonEmptyOf #-}

--------------------------------------------------------------------------------
-- * HasOuterBoundary Helpers

-- IndexedFold i s a = (Indexable i p, Contravariant f, Applicative f)
--                   => p a (f a) -> s -> f s

-- conjoined:: (p ~ (->) => q (a -> b) r) -> q (p a b) r -> q (p a b) r



-- | Yield the minimum  vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)

-- | Yield the minimum  vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
minimumVertexBy     :: forall polygon. (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
minimumVertexBy cmp = conjoined go igo
  where
    go :: Fold1 polygon (Vertex polygon)
    go = folding1 $ f outerBoundary cmp

    igo :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
    igo = ifolding1 $ f (outerBoundary.withIndex) (cmp `on` snd)

    f boundary cmp' = maybe (error "minimumVertexBy: absurd") First
                    . minimumByOf boundary cmp'

-- | Yield the maximum vertex of a polygon according to the given comparison function.
--
-- running time \( O(n) \)
maximumVertexBy     :: forall polygon. (HasOuterBoundary polygon)
                    => (Vertex polygon -> Vertex polygon -> Ordering)
                    -> IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
maximumVertexBy cmp = conjoined go igo
  where
    go :: Fold1 polygon (Vertex polygon)
    go = folding1 $ f outerBoundary cmp

    igo :: IndexedFold1 (VertexIx polygon) polygon (Vertex polygon)
    igo = ifolding1 $ f (outerBoundary.withIndex) (cmp `on` snd)

    f boundary cmp' = maybe (error "maximumVertexBy: absurd") First
                    . maximumByOf boundary cmp'

-- | Compute the signed area times 2 of a simple polygon. The the
-- vertices are in clockwise order, the signed area will be negative,
-- if the verices are given in counter clockwise order, the area will
-- be positive.
--
-- running time: \(O(n)\)
signedArea2X      :: (Num r, HasOuterBoundary simplePolygon
                     , Point_ point 2 r
                     , Vertex simplePolygon ~ point
                     ) => simplePolygon -> r
signedArea2X poly = sum [ p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord
                        | (p,q) <- poly ^..outerBoundaryEdges ]

-- | Get the line segments representing the outer boundary of the polygon.
outerBoundaryEdgeSegments :: forall polygon point r.
                             ( HasOuterBoundary polygon
                             , Vertex polygon ~ point
                             , Point_ point 2 r
                             )
                          => IndexedFold1 (VertexIx polygon) polygon (ClosedLineSegment point)
outerBoundaryEdgeSegments = outerBoundaryEdges . to (uncurry ClosedLineSegment)


-- | A fold that associates each vertex on the boundary with its
-- predecessor and successor (in that order) along the boundary.
outerBoundaryWithNeighbours :: ( HasOuterBoundary polygon
                               , Enum (VertexIx polygon)
                               )
                            =>  IndexedFold1 (VertexIx polygon)
                                        polygon
                                        (Vertex polygon, (Vertex polygon, Vertex polygon))
outerBoundaryWithNeighbours = ifolding1 $
    \pg -> (\(i,u) -> (i, f pg i u)) <$> itoNonEmptyOf outerBoundary pg
  where
    f pg i u = let v = pg^.outerBoundaryVertexAt (pred i)
                   w = pg^.outerBoundaryVertexAt (succ i)
               in (u, (v, w))


--------------------------------------------------------------------------------

-- class HasHoles face face' where
--   type HoleIx face
--   type Hole face
--   holes :: IndexedTraversal (HoleIx face) face face' (Hole face) (Hole face')

--------------------------------------------------------------------------------

-- | A class representing (planar) polygons. The edges of the polygon
-- may not intersect.
class ( HasOuterBoundary polygon
      , Vertex      polygon ~ point
      , Point_ point 2 r
      ) => Polygon_ polygon point r where

  -- | The area of a polygon
  --
  -- running time: \(O(n)\)
  area :: Fractional r => polygon -> r


  -- | Finds the extreme points, minimum and maximum, in a given direction
  --
  -- running time: \(O(n)\)
  extremes      :: (Num r, Ord r, Point_ point 2 r)
                => Vector 2 r -> polygon -> (point, point)
  extremes u pg = ( first1Of (minimumVertexBy (cmpExtreme u)) pg
                  , first1Of (maximumVertexBy (cmpExtreme u)) pg
                  )

--------------------------------------------------------------------------------
-- end of te Polygon_ class
--------------------------------------------------------------------------------

-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpExtreme       :: (Num r, Ord r, Point_ point 2 r)
                 => Vector 2 r -> point -> point -> Ordering
cmpExtreme u p q = u `dot` (p .-. q) `compare` 0

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- data MultiPG
-- data SimplePG

-- class DerivingStrategyPolygon strat polygon point r | polygon point r -> strat where
--   derivedArea :: Fractional r => polygon point r -> r

-- instance (SimplePolygon_ polygon point r) =>  DerivingStrategyPolygon SimplePG polygon point r where
--   derivedArea = signedArea -- abs . signedArea
--                 -- since the polygon is stored in CCW order, there is no need to actually
--                 -- use the absolute value.


{-
-}
