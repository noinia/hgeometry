{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-binds #-} -- TODO: disable later
module HGeometry.HalfPlane.CommonIntersection
  ( CommonIntersection(..)
  , SubLine(..)
  , Chain(..)
  , commonIntersection
  -- , lowerBoundary
  -- , LowerBoundary(..)
  ) where


import           Control.Lens hiding (Empty)
import           Data.Bifunctor (first)
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
-- import qualified Data.Sequence as Seq
import           Data.These
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfPlane.CommonIntersection.Chain
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LowerEnvelope
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | SingletonPoint    (Point 2 r)                     (Vector 3 halfPlane)
  -- ^ Common intersection is a singleton point, defined by the three given halfplanes
  | InSubLine (VerticalOrLineEQ r) (Vector 2 halfPlane) (SubLine halfPlane r)
  -- ^ The two halfPlanes that define the line, and the other halfplanes furthe
  -- restricitng the line.
  | Slab halfPlane halfPlane
    -- ^ two parallel halfPlanes l and u that form a slab;
  | BoundedRegion (ConvexPolygon (Point 2 r :+ halfPlane))
  | UnboundedRegion (Chain Seq halfPlane r)
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  deriving (Show,Eq)

-- | Part of the line
data SubLine halfPlane r = EntireLine
                         | InHalfLine (HalfLine (Point 2 r)) halfPlane -- ^ the third halfPlane
                         | InSegment (ClosedLineSegment (Point 2 r)) halfPlane halfPlane
                           -- ^ the remaining two halfplanes
                         deriving (Show,Eq)

--------------------------------------------------------------------------------

-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: forall f halfPlane r.
                          ( Foldable1 f, Functor f
                          , HalfPlane_ halfPlane r
                          , Fractional r, Ord r

                          , Show halfPlane, Show r
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = case bimap extremes boundaries $ partitionhalfPlanes hs0 of
    -- we only have vertical halfpalnes planes
    This verticals               -> case verticals of
            Negatives (_ :+ l)            -> UnboundedRegion $ Chain (Alternating l mempty)
            Positives (_ :+ u)            -> UnboundedRegion $ Chain (Alternating u mempty)
            BothSigns (x :+ l) (x' :+ u)  -> case x `compare` x' of
              LT -> EmptyIntersection
              EQ -> InSubLine (VerticalLineThrough x) (Vector2 l u) EntireLine
              GT -> Slab l u

    -- we only have non-vertical halfpalnes planes
    That nonVerticals            -> withNonVerticals nonVerticals

    -- we have both vertical, and non-vertical halfPlanes
    These verticals nonVerticals -> case nonVerticals of
      -- all non-vertical halfplanes are negative halfplanes though. So we can
       Negatives upperBoundary               -> upperBoundary `clipAndPushBy` verticals

      -- all non-vertical halfplanes are postiive halfplanes though
       Positives lowerBoundary               -> lowerBoundary `clipAndPushBy` verticals

       -- We have both positive and negative non-vertical halfplanes. So compute their
       -- common intersection, and then clip the result to the vertical slab determined by
       -- the verticals
       BothSigns _upperBoundary _lowerBoundary ->
         undefined -- TODO: somehow combine them, and clip
         -- withNonVerticals nonVerticals `clipBy` verticals
  where
    withNonVerticals              :: These2 (Chain Seq (LineEQ r :+ halfPlane) r)
                                  -> CommonIntersection halfPlane r
    withNonVerticals nonVerticals = case nonVerticals of
      -- we only have halfplanes with negative signs
       Negatives upperBoundary               -> unboundedRegion upperBoundary
      -- we only have halfplanes with positives signs
       Positives lowerBoundary               -> unboundedRegion lowerBoundary
       -- we have both positive and negative halfplanes
       BothSigns _upperBoundary _lowerBoundary -> undefined -- TODO: somehow combine them.

    -- -- clip the chain by the vertical boundaries
    -- clipBy inters = \case
    --   Negatives (x  :+ l)             -> undefined
    --   Positives (x' :+ u)             -> undefined
    --   BothSigns (x :+ l)  (x' :+ u)
    --     | x < x'                 -> EmptyIntersection
    --     | otherwise              -> undefined


-- | Helper function to construct an unbounded region
unboundedRegion :: Chain Seq (core :+ halfPlane) r -> CommonIntersection halfPlane r
unboundedRegion = UnboundedRegion . first (view extra)

-- | clip the chain by the given vertical halfplanes, and add the clipping halfPlanes to
-- the chains.
clipAndPushBy        :: (Ord r, Num r, HalfPlane_ halfPlane r)
                     => Chain Seq (LineEQ r :+ halfPlane) r
                     -> These2 (r :+ halfPlane)
                     -> CommonIntersection halfPlane r
clipAndPushBy inters = \case
    Negatives hl    -> unboundedRegion . clipRight' hl $ inters
    Positives hu    -> unboundedRegion . clipLeft'  hu $ inters
    BothSigns hl hu -> case (hl^.core) `compare` (hu^.core) of
      LT -> EmptyIntersection
      EQ -> let x           = hl^.core
                verticals   = Vector2 (hl^.extra) (hu^.extra)
                  -- the two vertical halfplanes defining x
                theHalfLine = HalfLine (Point2 x y) (Vector2 0 vy)
                (y :+ h)    = evalChainAt x inters
                  -- the non-vertical halfplane bounding the intersection
                vy          = case h^.halfSpaceSign of
                                Positive -> 1
                                Negative -> -1
            in InSubLine (VerticalLineThrough x) verticals (InHalfLine theHalfLine h)
      GT -> unboundedRegion . clipLeft' hl . clipRight' hu $ inters
  where

    dummy = LineEQ 0 0

    clipLeft' (x' :+ u) inters' = let env   = clipLeft x' inters'
                                      cons' = consElemWith (flip $ intersectVertical x')
                                 in env&_ChainAlternating %~ cons' (dummy :+ u)

    clipRight' (x :+ l) inters' = let env   = clipRight x inters'
                                      snoc' = flip (snocElemWith (intersectVertical x))
                                  in env&_ChainAlternating %~ snoc' (dummy :+ l)




data LeftIntersection halfPlane r =
    LowerAboveUpper
  | Intersection (Point 2 r) (Chain Seq (LineEQ r :+ halfPlane) r)
                             (Chain Seq (LineEQ r :+ halfPlane) r)
  | NoIntersection (Chain Seq (LineEQ r :+ halfPlane) r)
                                       (Chain Seq (LineEQ r :+ halfPlane) r)

-- | Given the lower  chain and the upper chain, computes the first intersection as seen from the left; i.e. the first point where the remainder of the lower chain indeed lies below the remainder of the upper chain
leftIntersection               :: (Ord  r, Fractional r
                                  )
                               => Chain Seq (LineEQ r :+ halfPlane) r
                               -> Chain Seq (LineEQ r :+ halfPlane) r
                               -> LeftIntersection halfPlane r
leftIntersection lower0 upper0 = case dropWhile lowerAboveUpper $ zipLR lower0 upper0 of
    []                     -> LowerAboveUpper
    (_, lower, upper) : _  -> case leftMost lower `intersect` leftMost upper of
      Nothing                    -> NoIntersection lower upper -- parallel
      Just (Line_x_Line_Point p) -> Intersection p lower upper
      Just (Line_x_Line_Line _)  -> undefined -- TODO: think about this....
       -- should be a degenerate slab, which is just a line (segment?) I guess
  where
    lowerAboveUpper (x, lower, upper) = evalAt' x (leftMost lower) > evalAt' x (leftMost upper)



-- | simultaneously scan the chains, zipping up their remainders. i.e.  returns a list of
-- triples (x_i,lower_i,upper_i), so that on (x_{i-1},x_i] lower_i and upper_i are the
-- remaining alternating chains. (x_{-1} = -\infty).
zipLR             :: Ord r
                  => Chain Seq halfPlane r -> Chain Seq halfPlane r
                  -> [(r, Chain Seq halfPlane r, Chain Seq halfPlane r )]
zipLR lower upper = case firstIntersection lower upper of
    None                           -> []
    Upper p upper'                 -> (p^.xCoord, lower, upper) : zipLR lower  upper'
    Lower p lower'                 -> (p^.xCoord, lower, upper) : zipLR lower' upper
    Simultaneous p _ lower' upper' -> (p^.xCoord, lower, upper) : zipLR lower' upper'

data FirstVertex halfPlane r = None
                 | Upper (Point 2 r) (Chain Seq  halfPlane r)
                 -- ^ first vertex in the upper chain, and the rest of the upper chain
                 | Lower (Point 2 r) (Chain Seq  halfPlane r)
                 | Simultaneous (Point 2 r)  -- ^ point in the lower chian
                                (Point 2 r) -- ^ point in the upper chain
                                (Chain Seq  halfPlane r) -- ^ remainder of the lower chain
                                (Chain Seq  halfPlane r) -- ^ remainder of the upper chain
                 deriving (Show,Eq)

firstIntersection             :: Ord r
                              => Chain Seq halfPlane r -> Chain Seq halfPlane r
                              -> FirstVertex halfPlane r
firstIntersection (Chain lower) (Chain upper) = case unconsAlt lower of
  Left _               -> case unconsAlt upper of
                            Left _               -> None
                            Right ((_,q),upper') -> Upper q (Chain upper')
  Right ((_,p),lower') -> case unconsAlt upper of
    Left _               -> Lower p (Chain lower')
    Right ((_,q),upper') -> case (p^.xCoord) `compare` (q^.xCoord) of
                              LT -> Lower p (Chain lower')
                              EQ -> Simultaneous p q (Chain lower') (Chain upper')
                              GT -> Upper q (Chain upper')




-- | A these where both values just have the same type.  in particular, we will use This:
-- for Negative signs halfplanes, and That for Positive signed half planes.
type These2 a = These a a
-- Some pattern synonyms so that the rest is easier to read
pattern Negatives :: a -> These a b
pattern Negatives x = This x
pattern Positives :: b -> These a b
pattern Positives x = That x
pattern BothSigns :: a -> b -> These a b
pattern BothSigns x y = These x y
{-# COMPLETE Negatives, Positives, BothSigns #-}

-- | Vertical halfplanes
type Verticals halfPlane r    = These2 (NonEmpty (r        :+ halfPlane))
-- | Non-vertical halfplanes
type NonVerticals halfPlane r = These2 (NonEmpty (LineEQ r :+ halfPlane))

-- | Classify the halfplanes by their bounding lines into Vertical/NonVertical,
-- and then on their Sign
partitionhalfPlanes     :: forall f halfPlane r.
                           ( Foldable1 f
                           , HalfPlane_ halfPlane r, Ord r, Fractional r
                           ) => f halfPlane
                        -> These (Verticals halfPlane r) (NonVerticals halfPlane r)
partitionhalfPlanes = bimap partition' partition'
                    . partitionEithersNE . fmap classifyHalfPlane
                    . toNonEmpty
  where
    -- partition the halfplanes by their sign.
    partition' :: NonEmpty (core :+ halfPlane) -> These2 (NonEmpty (core :+ halfPlane))
    partition' = partitionEithersNE . fmap classifyBySign

    classifyBySign h = case h^.extra.halfSpaceSign of
                         Negative -> Left  h
                         Positive -> Right h

-- | From all the vertical halfplanes with negative sign we compute the leftmost one,
-- and from the positive halfplanes we compute the rightmost one.
extremes :: Ord r => Verticals halfPlane r -> These2 (r :+ halfPlane)
extremes = bimap leftMostPlane rightMostPlane
  where
    rightMostPlane = maximumBy (comparing (^.core))
    leftMostPlane  = minimumBy (comparing (^.core))

-- | Computes the upper boundary of the halfplanes that have negative sign, and the upper
-- boundary of the halfplanes that have negative sign
boundaries :: ( HalfPlane_ halfPlane r
              , Ord r, Fractional r


              , Show r, Show halfPlane
              ) => NonVerticals halfPlane r -> These2 (Chain Seq (LineEQ r :+ halfPlane) r)
boundaries = bimap upperBoundary lowerBoundary
  where
    upperBoundary hs = let LowerEnvelope alt = lowerEnvelope hs in Chain alt
    lowerBoundary hs = let LowerEnvelope alt = upperEnvelope hs in Chain alt

--------------------------------------------------------------------------------




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
                    )
                 => f line -> UpperEnvelopeF g (Point 2 r) line
upperEnvelope = bimap (over yCoord negate) flipY . lowerEnvelope . fmap flipY
  where
    flipY :: line -> line
    flipY = over (hyperPlaneCoefficients.traverse) negate

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
