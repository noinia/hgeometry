{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection
  ( CommonIntersection(..)
  , Chain(..)
  , commonIntersection
  , lowerBoundary
  -- , LowerBoundary(..)
  ) where

import           Control.Lens hiding (Empty)
import           Data.Default.Class
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..), ViewL(..))
import qualified Data.Sequence as Seq
import           Data.These
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LowerEnvelope
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector

import           Debug.Trace
--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | Bounded (ConvexPolygon (Point 2 r :+ halfPlane))
  | Slab halfPlane halfPlane
    -- ^ two parallel halfPlanes l and u that form a slab;
  | Unbounded (Chain Seq halfPlane r)
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  deriving (Show,Eq)

-- | A polygonal chain bounding an unbounded convex region, in CCW order.
newtype Chain f halfPlane r = Chain (Alternating f (Point 2 r) halfPlane)

deriving instance ( Show halfPlane, Show (f (Point 2 r, halfPlane))
                  ) => Show (Chain f halfPlane r)

deriving instance ( Eq halfPlane, Eq (f (Point 2 r, halfPlane))
                  ) => Eq (Chain f halfPlane r)

deriving instance ( Ord halfPlane, Ord (f (Point 2 r, halfPlane))
                  ) => Ord (Chain f halfPlane r)

instance Functor f => Functor (Chain f r) where
  fmap f = bimap id f
instance Functor f => Bifunctor (Chain f) where
  bimap f g = bimap' f (over coordinates g)

-- | slightly more general version of bimap so we can easily flip the plane.
bimap'                 :: Functor f
                       => (halfPlane -> halfPlane')
                       -> (Point 2 r -> Point 2 s)
                       -> Chain f halfPlane r -> Chain f halfPlane' s
bimap' f g (Chain alt) = Chain $ bimap g f alt

--------------------------------------------------------------------------------

-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: ( Foldable1 f, Functor f
                          , HalfPlane_ halfPlane r
                          , Fractional r, Ord r

                          , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME
                          , Show halfPlane, Show r
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = case partitionEithersNE . fmap classifyHalfPlane' $ toNonEmpty hs0 of
    This onlyBelows     -> Unbounded $ upperBoundary onlyBelows
    That onlyAboves     -> Unbounded $ lowerBoundary onlyAboves
    These belows aboves -> let bb = lowerBoundary aboves
                               ub = upperBoundary belows
                           in undefined -- somehow combine them
  where
    classifyHalfPlane' h = case h^.halfSpaceSign of
                             Negative -> Left  h
                             Positive -> Right h

data HalfPlaneCategory halfPlane r = LeftVertical  r halfPlane
                                   | RightVertical r halfPlane
                                   | Lower (LineEQ r) halfPlane
                                   | Upper (LineEQ r) halfPlane
                                   deriving (Show)

partitionhalfPlanes :: (Foldable1 f
                       , HalfPlane_ halfPlane r, Ord r, Fractional r
                       ) => f halfPlane -> NonEmpty (HalfPlaneCategory halfPlane r)
partitionhalfPlanes = undefined





--------------------------------------------------------------------------------

combine                :: Maybe (r, halfPlane) -- ^ rightmost vertical positive haflplane (left vertical boundary)
                       -> Maybe (r, halfPlane) -- ^ left vertical negative haflplane (right vertical boundary)
                       -> Chain Seq halfPlane r -- ^ upper boundary of the (non-vertical) negative halfplanes
                       -> Chain Seq halfPlane r -- ^ lower boundary of the (non-vertical) positive halfplanes
                       -> CommonIntersection halfPlane r
combine ml mr mup mlow = undefined


-- lowerBelowUpper :: r
--                 -> Chain Seq halfPlane r -- ^ lower boundary of the (non-vertical) positive halfplanes
--                 -> Chain Seq halfPlane r -- ^ upper boundary of the (non-vertical) negative halfplanes
--                 -> Bool
-- lowerBelowUpper x lower upper = case evalChain x lower



-- clipLowerLeft :: VerticalOrLineEQ r
--               -> Chain Seq halfPlane r -- ^ lower boundary of the (non-vertical) positive halfplanes
--               ->

-- (Chain lower) (Chain upper) =  case (unconsAlt lowerB, unconsAlt upperB) of
--       (Left (l :+ hl),            Left (u :+ hu))             -> undefined
--       (Left (l :+ hl),            Right ((u :+ hu,q), upper)) -> case l `intersect` u of
--         Nothing ->


--           | evalAt' 0 l <= evalAt' 0 u -> Slab hl hu
--                 | otherwise                  -> EmptyIntersection
--         Just (Line_x_Line_Line _)            -> Slab hl hu -- degenerate slab
--         Just (Line_x_Line_Point p)



--       (Right ((l :+ hl,p),lower), Left (u :+ hu))             -> undefined
--       (Right ((l :+ hl,p),lower), Right ((u :+ hu,q), upper)) -> undefined



-- -- |
-- combineNonV :: ( Fractional r, Ord r
--                ) => Chain Seq (LineEQ r :+ halfPlane) r
--                -- ^ lower boundary of the (non-vertical) positive halfplanes
--             -> Chain Seq (LineEQ r :+ halfPlane) r
--             -- ^ upper boundary of the (non-vertical) negative halfplanes
--             -> CommonIntersection halfPlane r
-- combineNonV (Chain lower0) (Chain upper0) = go0 lower0 upper0
--   where
--     go0 lowerB upperB = case (unconsAlt lowerB, unconsAlt upperB) of
--       (Left (l :+ hl), Left (u :+ hu)) -> case l `intersect` u of
--         Nothing | evalAt' 0 l <= evalAt' 0 u -> Slab hl hu
--                 | otherwise                  -> EmptyIntersection
--         Just (Line_x_Line_Line _)            -> Slab hl hu -- degenerate slab
--         Just (Line_x_Line_Point p)
--                 | evalAt' x l < evalAt' x u   -> Unbounded . Chain $
--                                                  Alternating hl $ Seq.singleton (p,hu)
--                                               -- diverging
--                 | otherwise                   -> Unbounded . Chain $
--                                                  Alternating hu $ Seq.singleton (p,hl)
--                                           -- converging
--           where x = p^.xCoord + 1
--       (lower, upper) -> trimLeft (lower,upper)

--     -- recursive case
--     trimLeft = \case
--       (Left (l :+ hl),            Left (u :+ hu))             -> undefined
--       (Left (l :+ hl),            Right ((u :+ hu,q), upper)) -> case l `intersect` u of
--         Nothing ->


--           | evalAt' 0 l <= evalAt' 0 u -> Slab hl hu
--                 | otherwise                  -> EmptyIntersection
--         Just (Line_x_Line_Line _)            -> Slab hl hu -- degenerate slab
--         Just (Line_x_Line_Point p)



--       (Right ((l :+ hl,p),lower), Left (u :+ hu))             -> undefined
--       (Right ((l :+ hl,p),lower), Right ((u :+ hu,q), upper)) -> undefined


-- case
-- l `intersect` u of
--         Nothing ->


--           | evalAt' 0 l <= evalAt' 0 u -> Slab hl hu
--                 | otherwise                  -> EmptyIntersection
--         Just (Line_x_Line_Line _)            -> Slab hl hu -- degenerate slab
--         Just (Line_x_Line_Point p)
--                 | evalAt' x l < evalAt' x u   -> Unbounded . Chain $
--                                                  Alternating hl $ Seq.singleton (p,hu)
--                                               -- diverging
--                 | otherwise                   -> Unbounded . Chain $
--                                                  Alternating hu $ Seq.singleton (p,hl)
--                                           -- converging
--           where x = p^.xCoord + 1



--         undefined



-- data ConvergingOrDiversing = Converging | Diverging
--                            deriving (Show,Eq)

-- combine                               :: Chain Seq halfPlane r -> Chain Seq halfPlane r
--                                       -> CommonIntersection halfPlane r
-- combine (Chain lowerB0) (Chain upperB0) = go lowerB0 upperB0
--   where



--   case (lowerB, upperB) of
--    (Alternating l Empty) (Alternating u Empty) ->
--        case (asGeneralLine $ l^.boundingHyperPlane, asGeneralLine $ u^.boundingHyperPlane) of
--          (VerticalLineThrough lx , VerticalLineThrough ux)
--                                         | lx > ux   -> EmptyIntersection
--                                         | otherwise -> Slab l u
--          (VerticalLineThrough lx, NonVertical ul)         ->
--          (NonVertical ll,         VerticalLineThrough ux) ->
--          (NonVertical ll,         NonVertical ul)         ->


--          (VerticalLineThrouy lx)

--        case (l^.boundingHyperPlane) `intersect` (u^.boundingHyperPlane) of
--          Nothing                    -> Slab l u
--              -- the two bounding lines don't intersect, so they form a slab
--          Just (Line_x_Line_Line _)  -> Slab l u -- degenerate slab
--          Just (Line_x_Line_Point p) -> Unbounded $ case classify (p^.xCoord + 1) l u of
--            Converting -> Alternating u $ Seq.singleton (p,l)
--            Diverging  -> Alternating l $ Seq.singleton (p,u)
--    _                                           -> undefined

-- classify x l u = case (evalAt'' x l > evalAt'' x u)  of
--     False -> Converging
--     True  ->  Diverging
--  where
--    evalAt'' q h =
--      VerticalLineThrough x' ->                     evalAt' q $



--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from below,
-- i.e. all halfplanes have positive sign, computes their common intersection.
--
-- running time: O(n\log n)
lowerBoundary     :: forall f halfPlane r.
                     ( HalfPlane_ halfPlane r
                     , Foldable1 f, Functor f, Fractional r, Ord r
                     , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME
                     , Show r, Show halfPlane
                     )
                  => f halfPlane -> Chain Seq halfPlane r
lowerBoundary hs0 = Chain $ case partitionEithersNE . fmap classifyHalfPlane $ toNonEmpty hs0 of
    This onlyVerticals           -> let (_ :+ h) = rightMostPlane onlyVerticals
                                    in Alternating h mempty
    That onlyNonVerticals        -> let env = view extra <$> upperEnvelope' onlyNonVerticals
                                    in env^._Alternating
    These verticals nonVerticals -> let (minX :+ h) = rightMostPlane verticals
                                        env         = clipLeft minX $ upperEnvelope' nonVerticals
                                        alt         = consElemWith (flip $ intersectVertical minX)
                                                                   (dummy :+ h)
                                                                   (env^._Alternating)
                                    in view extra <$> alt
       -- Generally a symmetric implementation to 'upperBoundary'.
  where
    dummy = LineEQ 0 0

    -- finds the leftMost vertical halfplane
    rightMostPlane = maximumBy (comparing (^.core))

    upperEnvelope' = over _Alternating (mapF Seq.fromList) . upperEnvelope


-- | We use the same type as the lower envelope
type UpperEnvelopeF = LowerEnvelopeF

-- | To compute the upper envelope we simply flip the plane, and compute the lower
-- envelope instead.
--
-- \(O(n\log n)\)
upperEnvelope :: forall g f line r.
                    ( NonVerticalHyperPlane_ line 2 r
                    , Fractional r, Ord r
                    , Foldable1 f, Functor f
                    , IsIntersectableWith line line
                    , Intersection line line ~ Maybe (LineLineIntersection line)
                    , HasFromFoldable g, Functor g
                    , Default line -- TODO hack
                    )
                 => f line -> UpperEnvelopeF g (Point 2 r) line
upperEnvelope = bimap (over yCoord negate) flipY . lowerEnvelope . fmap flipY
  where
    flipY :: line -> line
    flipY = over (hyperPlaneCoefficients.traverse) negate


  -- = undefined -- bimap' flipY (over yCoord negate) . lowerBoundary . fmap flipY
  -- --               -- by flipping the plane, and using the existing lowerBoundary machinery

-- -- | Given the bounding lines of a bunch of halfplanes that are all bounded from above,
-- -- computes their common intersection.
-- --
-- -- running time: O(n\log n)
-- upperBoundary :: ( NonVerticalHyperPlane_ boundingLine 2 r
--                  , Fractional r, Ord r, Foldable f, Functor f
--                  )
--               => f boundingLine -> LowerBoundary (LowerChain boundingLine r)
-- upperBoundary =



--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from above,
-- i.e. all halfplanes have negative sign, computes their common intersection.
--
-- running time: O(n\log n)
upperBoundary     :: ( HalfPlane_ halfPlane r
                     , Foldable1 f, Fractional r, Ord r
                     , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME

                     , Show halfPlane, Show r
                     )
                  => f halfPlane -> Chain Seq halfPlane r
upperBoundary hs0 = Chain $ case partitionEithersNE . fmap classifyHalfPlane $ toNonEmpty hs0 of
    This onlyVerticals           -> let (_ :+ h) = leftMostPlane onlyVerticals
                                    in Alternating h mempty
    That onlyNonVerticals        -> let env = view extra <$> lowerEnvelope' onlyNonVerticals
                                    in env^._Alternating
    These verticals nonVerticals -> let (maxX :+ h) = leftMostPlane verticals
                                        env         = clipRight maxX $ lowerEnvelope' nonVerticals
                                        alt         = snocElemWith (intersectVertical maxX)
                                                                   (env^._Alternating)
                                                                   (dummy :+ h)
                                    in view extra <$> alt
       -- We clip the lower envelope of the non-vertical planes at the leftmost
       -- vertical plane, throwing away any vertices whose x-coord is at most maxX
       --
       -- We then snoc the new element onto the alternating list. We create a dummy
       -- non-vertical line (thatwe will next immediately) throw away anyway. To construct
       -- the separator we will simply evaluate the bounding line at the rightmost
       -- position.
  where
    dummy = LineEQ 0 0

    -- finds the leftMost vertical halfplane
    leftMostPlane = minimumBy (comparing (^.core))

    lowerEnvelope' = over _Alternating (mapF Seq.fromList) . lowerEnvelope

--------------------------------------------------------------------------------

-- | Given an x-coordiante, a bounding line (and its hyperplane), evaluate the value of
-- the line.
intersectVertical               :: Num r => r -> LineEQ r :+ extra -> extra' -> Point 2 r
intersectVertical x (l' :+ _) _ = Point2 x (evalAt' x l')


-- | Classify the halfplane as either having a vertical bounding line or a general
-- non-vertical line.
classifyHalfPlane   :: (HalfPlane_ halfPlane r, Fractional r, Eq r)
                    => halfPlane
                    -> Either (r :+ halfPlane) (LineEQ r :+ halfPlane)
classifyHalfPlane h = case h^.boundingHyperPlane.to asGeneralLine of
  VerticalLineThrough x -> Left  (x :+ h)
  NonVertical l         -> Right (l :+ h)

-- | Convert to a general line.
asGeneralLine :: (HyperPlane_ hyperPlane 2 r, Fractional r, Eq r)
              => hyperPlane -> VerticalOrLineEQ r
asGeneralLine = hyperPlaneFromEquation . hyperPlaneEquation

--------------------------------------------------------------------------------


--clipLower


--------------------------------------------------------------------------------

-- | Given a value x, Clip the lower envelope to the interval \((-\infty,x]\)
clipRight      :: (Ord r, Point_ vertex 2 r)
                => r -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipRight maxX = clipRightWhen $ \(v, _) -> v^.xCoord >= maxX

-- | Given a value x, Clip the lower envelope to the interval \([x,\infty)\)
clipLeft      :: (Ord r, Point_ vertex 2 r)
               => r -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipLeft minX = clipLeftWhen $ \(v, _) -> v^.xCoord <= minX

-- | Clip on the right by a line
clipRightLine       :: (Ord r, Num r, Point_ vertex 2 r)
                    => LineEQ r -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipRightLine right = clipRightWhen (above right)

-- | Clip the left by a line
clipLeftLine      :: (Ord r, Num r, Point_ vertex 2 r)
                  => LineEQ r -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipLeftLine left = clipLeftWhen (above left)

-- | Test if the given vertex lies above the line
above            :: (Ord r, Num r, Point_ vertex 2 r) => LineEQ r -> (vertex,a) -> Bool
above line (v,_) = (v^.yCoord) >= evalAt' (v^.xCoord) line

-- | Clip the lower envelope on the right
clipRightWhen   :: ((vertex, line) -> Bool)
                -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipRightWhen p = over _Alternating $
                 \(Alternating h0 hs) -> Alternating h0 $ Seq.dropWhileR p hs

-- | Clip the lower envelope on the left
clipLeftWhen   :: ((vertex, line) -> Bool)
               -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipLeftWhen p = over _Alternating $
                 \alt@(Alternating _ hs) -> case Seq.spanl p hs of
                   (Empty,         _)    -> alt
                   (_ :|> (_,h0'), kept) -> Alternating h0' kept

--------------------------------------------------------------------------------
