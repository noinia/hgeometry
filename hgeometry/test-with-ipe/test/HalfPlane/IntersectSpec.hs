{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use ++" -}
module HalfPlane.IntersectSpec
  ( commonIntersection
  , myMain, testMain
  , spec
  ) where

import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import qualified Data.Text as Text
import           HGeometry.HalfPlane.CommonIntersection.Chain
import           HGeometry.HalfPlane.CommonIntersection (CommonIntersection(..))
import           HGeometry.HalfSpace
import           HGeometry.Cyclic
import           HGeometry.HyperPlane.Class
import           HGeometry.Unbounded
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Line
import           HGeometry.Kernel
import           Control.Lens hiding (below)
import           HGeometry.Sequence.Alternating
import           Data.Coerce
import           Data.Foldable1
import           Data.Ord (Down (..))
import           HGeometry.Foldable.Sort (sortOnCheap)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector as Vector
import           Ipe
import           Ipe.Color
import           HGeometry.HalfLine
import           HGeometry.Ext
import           Witherable (mapMaybe)
import           System.OsPath
import qualified Data.List.NonEmpty as NonEmpty
import           R
import           Test.Hspec.WithTempFile
import           Golden
import           HalfPlane.CommonIntersectionSpec ()
import           HGeometry.Polygon
import           HGeometry.Cone
import           Test.Hspec
import           Ipe.Color (gray)
import           HGeometry.Cone.Intersection
import           Test.Hspec.QuickCheck
import           Test.QuickCheck (counterexample, (===), suchThat, discard, Arbitrary(..))
import           HGeometry.Slab
-- import HalfPlane.Intersection
import           HGeometry.Ipe.Instances ()
import           Debug.Trace
--------------------------------------------------------------------------------

type HalfPlane r = HalfSpaceF (LinePV 2 r)

allPoints' :: IpeObject r -> [Point 2 r]
allPoints' = \case
  IpeGroup gr    -> foldOf (core.groupItems.folded.to allPoints') gr
  IpeImage _     -> []
  IpeTextLabel _ -> []
  IpeMiniPage _  -> []
  IpeUse _       -> []
  IpePath path   -> foldOf (core.to allPointsPath) path

allPointsPath :: Path r -> [Point 2 r]
allPointsPath = foldOf (pathSegments.folded.to allPoints'')
  where
    allPoints'' = \case
      PolyLineSegment poly        -> poly^..vertices
      PolygonPath _ poly          -> poly^..vertices
      CubicBezierSegment bez      -> bez^..vertices
      QuadraticBezierSegment bez  -> bez^..vertices
      EllipseSegment _            -> []
      ArcSegment                  -> []
      SplineSegment               -> []
      ClosedSplineSegment         -> []

ipeSpec = describe "halfplane drawn correctly" $ do
            prop "constraint drawing" $ \(h :: HalfPlane R) ->
              let vs = drawAsConstraint gray h ^.core.groupItems.folded.to allPoints'
                  flipSign = \case
                    Positive -> Negative
                    Negative -> Positive
              in all (`intersects` (h&halfSpaceSign %~ flipSign)) vs
              -- in the constraint drawing the vertices of the polygon should
              -- either lie on the bounding line of h, or outside the halfplane.
              -- so; they should lie in the flipped halfplane.
            prop "toHalfPlane correct"   $ \(ray :: HalfLine (Point 2 r) :+ ()) ->
              isRightHalfPlane (toHalfPlane ray)

spec :: Spec
spec = describe "Cone properties" $ do
         prop "bisector in cone" $ \(cone :: Cone R (Point 2 R) ()) ->
           let b@(HalfLine p v) = coneBisector cone
           in counterexample (show b) $ ((p^.asPoint) .+^ v) `intersects` cone
         prop "extraPoints correct" $ \(cone :: Cone R (Point 2 R) ()) ->
           let rect = boundingBox $ defaultBox :| [boundingBox (cone^.apex) ]
               pts  = extraPoints (leftBoundary  cone ^.core)
                                  (rightBoundary cone ^.core)
                                  rect
           in all (`intersects` cone) pts

         prop "toConvexIn correct" $ \(cone :: Cone R (Point 2 R) ()) ->
           let rect = boundingBox $ defaultBox :| [boundingBox (cone^.apex) ]
               pts  = toConvexPolygonIn rect cone
           in allOf vertices (`intersects` cone) pts

         prop "intersection correct" $ \(h1 :: HalfPlane R) (h2 :: HalfPlane R) ->
           case h1 `intersect` h2 of
             (Just (HalfPlane_x_HalfPlane_Cone cone)) ->
               let rect = boundingBox $ defaultBox :| [boundingBox (cone^.apex) ]
                   pts  = toConvexPolygonIn rect cone
               in allOf vertices (`intersects` cone) pts
             _                                        -> discard
         halfPlaneIntersectionTests inFile
                                    [osp|halfPlaneIntersection|]

         ipeSpec

halfPlaneIntersectionTests :: IO OsPath -> OsPath -> Spec
halfPlaneIntersectionTests inFile theName = describe "HalfPlane x HalfPlane tests"  $ do
  halfPlanes' <- runIO (fmap toHalfPlane <$> (readAllFrom =<< inFile))
  let pages' = [ let res = h1 `intersect` h2
                 in fromContent . concat $
                    [ [ iO $ defIO res ! attr SLayer "intersection"]
                    , [ iO $ defIO h   ! attr SLayer "input"
                      | h <- [h1,h2]
                      ]
                    ]
               | h1 <- halfPlanes', h2 <- halfPlanes'
               ]
  case ipeFile <$> NonEmpty.nonEmpty pages' of
    Nothing   -> error "error no halfplanes"
    Just file -> goldenWith [osp|data/test-with-ipe/golden/HalfPlane/|]
                   (ipeFileGolden { name = theName  })
                   (file&styles %~ (opacitiesStyle:))

toHalfPlane :: HalfLine (Point 2 R) :+ attrs -> HalfPlane R
toHalfPlane = rightHalfPlane . asOrientedLine . view core

inFile :: IO OsPath
inFile  = getDataFileName [osp|test-with-ipe/golden/HalfPlane/intersection.ipe|]

--- >>> show outPath
outPath :: IO OsPath
outPath = getDataFileName [osp|test-with-ipe/golden/HalfPlane/intersection.out.ipe|]


--------------------------------------------------------------------------------






debug = let h1 = HalfSpace Negative (LinePV (Point2 0 3) (Vector2 0 (-2.33334)))
            h2 = HalfSpace Positive (LinePV (Point2 0 (-1.33334)) (Vector2 2.33333 0.33333))
              -- HalfSpace Negative (LinePV (Point2 64 240) (Vector2 160 (-16)))
            -- h2 = HalfSpace Negative (LinePV (Point2 192 272) (Vector2 48 (-128)))
        in h1 `intersect` h2


myCone2 :: Intersection (HalfSpaceF (LineEQ R)) (HalfSpaceF (LineEQ R))
myCone2 = let hRight = above (LineEQ 0 100) :: HalfSpaceF (LineEQ R)
              hLeft  = below (LineEQ 1 0)   :: HalfSpaceF (LineEQ R)
          in hLeft `intersect` hRight

myCone = Cone (Point2 100 100) (Vector2 1 1 :+ ()) (Vector2 1 0 :+ ())




testMain = do halfPlanes'' <- fmap toHalfPlane <$> (readAllFrom =<< inFile)
              let halfPlanes' = take 4 halfPlanes''
              -- let halfPlanes' :: [HalfPlane R]
              --     halfPlanes' = [ HalfSpace Negative (LinePV (Point2 0 3) (Vector2 0 (-2.33334)))
              --                   , HalfSpace Positive (LinePV (Point2 0 (-1.33334)) (Vector2 2.33333 0.33333))
              --                   ]
              traverse print halfPlanes'
              case NonEmpty.nonEmpty halfPlanes' of
                Nothing         -> print "error; no halfplanes"
                Just halfPlanes -> case commonIntersection halfPlanes of
                  Nothing  -> putStrLn "no intersection?"
                  Just res -> do print res
                                 outPath' <- outPath

                                 writeIpeFile outPath' . addStyleSheet opacitiesStyle
                                                       . singlePageFromContent . concat $
                                   [ [iO $ defIO res ! attr SLayer "intersection"
                                     ]
                                   , [ iO $ drawAsConstraint gray h ! attr SLayer "input"
                                     | h <- halfPlanes'
                                     ]
                                   ]


              -- let pages' = [ let res = h1 `intersect` h2
              --                in fromContent . concat $
              --                     [ [ iO $ defIO res ! attr SLayer "intersection"]
              --                     , [ iO $ defIO h   ! attr SLayer "input"
              --                       | h <- [h1,h2]
              --                       ]
              --                     ]
              --              | h1 <- halfPlanes', h2 <- halfPlanes'
              --              ]
              -- case ipeFile <$> NonEmpty.nonEmpty pages' of
              --   Nothing    -> print "error no halfplanes"
              --   Just file -> do outPath' <- outPath
              --                   writeIpeFile outPath' $ file&styles %~ (opacitiesStyle:)



-- >>> myMain
--
myMain :: IO ()
myMain = do halfPlanes' <- fmap toHalfPlane <$> (readAllFrom =<< inFile)
            case NonEmpty.nonEmpty halfPlanes' of
              Nothing         -> print "error; no halfplanes"
              Just halfPlanes -> case commonIntersection halfPlanes of
                Nothing  -> putStrLn "no intersection?"
                Just res -> do print res
                               outPath' <- outPath
                               writeIpeFile outPath' . singlePageFromContent . concat $
                                 [ [iO $ defIO res ! attr SLayer "output"
                                   ]
                                 , [ iO $ drawAsConstraint gray h ! attr SLayer "constraint"
                                   | h <- halfPlanes'
                                   ]
                                 ]


--------------------------------------------------------------------------------


data State r halfPlane = OneChain (Chain Seq r halfPlane)
                       | TwoChains (Chain Seq r halfPlane) (Chain Seq r halfPlane)
                       deriving (Show)

timChains pref suf = (pref,suf) -- FIXME!!

-- | Given two chains;  try to join them in a combined convex chain
--
--
joinChains          :: Chain Seq r halfPlane
                    -- ^ The suffix of the chain; given from right to left
                    -> Chain Seq r halfPlane
                    -- ^ The prefix of the chain; given from left to right
                    -> Maybe (CommonIntersection halfPlane r)
joinChains suf pref = Just $ UnboundedRegion pref -- FIXME!!!

-- data PossiblyDegenIntersection halfPlane = Subline halfPlane halfPlane
--                                          | Slab' halfPlane halfPlane
--                                          | OneDim
--                                          | Regular

-- -- | Tests if the halfplanes form a slab
-- -- returns a Nothing if there is no intersection at all
-- -- returns a Just (Just h1 h2) if there are two halfplanes intersecting in a slab
-- -- reutnrs a Just Nothing if there are more than two non-reduncant non-parallel halfplanes
-- asSlab                     :: NonEmptyVector (HalfPlaneType r, halfPlane)
--                            -> Maybe (PossiblyDegenerateIntersection halfPlane)
-- asSlab hs | length hs /= 2 = Just Regular
--           | otherwise      = case (NonEmptyVector.head hs, NonEmptyVector.last hs) of
--               ((Rightward x,h1), (Leftward x', h2)) -> case x `compare` x' of
--                 LT -> Just $ Slab' h1 h2
--                 EQ -> Just $ SubLine h1 h2
--                 GT -> Nothing


-- instance (

--          ) =>


--------------------------------------------------------------------------------





--------------------------------------------------------------------------------


    -- ipeGroup [ iO $ defIO (h1^.boundingHyperPlane)
    --                              , iO $ defIO (h2^.boundingHyperPlane)
    --                              ]
    -- -- TODO: do this property; i.e. compute the intersection with the bounding box



--------------------------------------------------------------------------------




--------------------------------------------------------------------------------





--------------------------------------------------------------------------------

--------------------------------------------------------------------------------



-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: forall f halfPlane r.
                          ( Foldable1 f, Functor f
                          , HalfPlane_ halfPlane r
                          , Fractional r, Ord r
                          , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                          , Show halfPlane, Show r
                          , HasIntersectionWith (Point 2 r) halfPlane
                          , AsLine (BoundingHyperPlane halfPlane 2 r)
                          , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r) (BoundingHyperPlane halfPlane 2 r)
                          , Intersection (BoundingHyperPlane halfPlane 2 r)
                                         (BoundingHyperPlane halfPlane 2 r)
                            ~ Maybe (LineLineIntersection (BoundingHyperPlane halfPlane 2 r))
                          , IsIntersectableWith halfPlane halfPlane
                          , GetDirection (BoundingHyperPlane halfPlane 2 r)
                          , Intersection halfPlane halfPlane ~
                              Maybe (HalfPlaneIntersection r halfPlane)
                          , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                          )
                       => f halfPlane -> Maybe (CommonIntersection halfPlane r)
commonIntersection = (toCommonIntersection =<<)
                   . foldlMap1' (Just . OneChain . singleton . snd) extend
                   . dropRedundants
                   . NonEmptyVector.unsafeFromVector
                   . sortOnCheap fst
                   . fmap (\h -> (halfPlaneType h, h))
  where
    toCommonIntersection :: State r halfPlane -> Maybe (CommonIntersection halfPlane r)
    toCommonIntersection = \case
      OneChain chain     -> Just $ UnboundedRegion chain
        -- FIXME: it is not clear that the chain does not just intersect itself!
      TwoChains suf pref -> joinChains suf pref
      -- FIXME: this is incorrect, but for now it may be ok
      -- i.e. it may also be a polygon

    extend           :: Maybe (State r halfPlane)
                     -> (HalfPlaneType r, halfPlane)
                     -> Maybe (State r halfPlane)
    extend mst (t,h) = mst >>= \case
        OneChain chain       -> extendOne (clipLeftLine h chain)
        TwoChains suff chain -> extendTwo suff (clipLeftLine h chain)
          -- TODO; combine with the existing chain
      where
        extendTwo suff chain = case extendOne chain of
          Nothing                -> Nothing
          Just (OneChain chain') -> Just (TwoChains suff chain')
          Just (TwoChains _ _)   -> Nothing
                                    -- I think this would mean we have an empty intersection
                                    -- TODO: verify this.

        extendOne chain = case unconsAlt $ chain^._ChainAlternating of
            Left h'          -> case h `intersect` h' of
              Nothing -> Nothing -- no intersection already
              Just i  -> Just $ case i of
                HalfPlane_x_HalfPlane_Line line   -> error "line; not implemented yet"
                HalfPlane_x_HalfPlane_Slab _      -> TwoChains (singleton h') (singleton h)
                                                  -- h' forms the suffix
                HalfPlane_x_HalfPlane_Cone cone
                  | isCCWTurn h' a h -> OneChain $ consChain (h,a) chain
                  | otherwise        -> TwoChains (singleton h') (singleton h)
                  where
                    a = cone^.apex
                  -- if we actually turn CCW; then we have one chain; otherwise
                  -- we found the opening
                HalfPlane_x_HalfPlane_HalfPlane _ -> error "absurd; duplicate halfplane"

            Right ((h',u),_) -> case (h^.boundingHyperPlane)
                                     `intersect`
                                     (h'^.boundingHyperPlane) of
              Nothing -> undefined -- TODO
                -- slabOrNothing -- FIXME: whether its a slab or not depends on whether chain
                                       -- still has other items as well.
              Just i  -> Just $ traceShowWith ("inters ",h',u,) $ case i of
                Line_x_Line_Line l  -> subLine -- orPoint? or duplicate constraint
                Line_x_Line_Point v
                  | orderedCorrectly u v h' -> OneChain $ consChain (h,v) chain
                    -- should this be orderedCorrectly && isCCWTurn' u v h?
                  | otherwise               -> traceShowWith ("twoChains",isCCWTurn' u v h,) $
                    TwoChains chain (singleton h)
          where
            h' = chain^.leftMost
            p = pointInteriorTo t

            slabOrNothing | p `intersects` h' = Just undefined -- slab
                          | otherwise         = Nothing

            subLine = undefined

            -- test if we make a left turn at a when going from h' to h
            isCCWTurn  h' a h = let u = a .+^ rightBoundingVector a h'
                                      -- the vector should have the hyperplane to its left
                                in isCCWTurn' u a h

            -- | given the previous vertex u, and the current vertex a
            -- (that both lie on the boundary of the halfplane h'),
            -- and so that h' is to the right of the vector from u to
            -- a., computes whether we make a counter-clockwise (i.e. left) turn
            -- at a when going to (the bounding line of) h
            isCCWTurn' u  a h = let v = a .+^ leftBoundingVector a h
                                in ccw u a v == CCW

            -- | Given the previous vertex u, and the current vertex
            -- a, test if the vector going from u to a has h to its left
            orderedCorrectly u a h = let Vector2 x y = a .-. u
                                         w           = Vector2 (-y) x
                                     in  a .+^ w `intersects` h




-- intersectBoundaries h = \case
--   Rightward (Down x) ->
--   Leftward x         ->

-- | Helper data type
data Helper a b = Helper { previous   :: !b
                         , getBuilder :: (Builder.Builder a)
                         }

-- | Drop redundant constraints.
dropRedundants :: Eq r
               => NonEmptyVector (HalfPlaneType r, halfPlane)
               -> NonEmptyVector (HalfPlaneType r, halfPlane)
dropRedundants = NonEmptyVector.unsafeFromVector
               . Builder.build
               . getBuilder
               . foldrMap1 (\x -> Helper (fst x) (Builder.singleton x)) extend
  where
    extend z@(t,_) (Helper t' res) = Helper t $ case (t,t') of
      (Leftward _,   Leftward _)                -> res
      (Rightward _,  Rightward _)               -> res
      (Downward a _, Downward a' _) | a == a'   -> res
                                    | otherwise -> Builder.singleton z <> res
      (Upward a _,   Upward a' _)   | a == a'   -> res
                                    | otherwise -> Builder.singleton z <> res
      _                                         -> Builder.singleton z <> res

-- | Helper type to compare halfplanes to produce an ordering that we
-- can use to compute their common intersection. The order should
-- correspond to a CCW ordering starting from the "upper right"; i.e.
--
-- we first have the downward halfplanes, then the vertical rightward halfplanes,
-- then the upward haflplanes, and finally the vertical leftward halfplanes.
--
-- the downward halfplanes are ordered by increasing slope
-- the rightward halfplanes are ordered on increasing x-coordinate,
-- the upward halfplanes are ordered by increasing slope
-- the leftward halfplanes are ordered on decreasing x-coordinate
data HalfPlaneType r = Downward r (Down r) -- slope, intercept
                     | Rightward r -- x-coord
                     | Upward r r -- slope, intercept
                     | Leftward (Down r)
  deriving (Show,Eq,Ord)


-- | compute a point on the bounding line
instance Num r => HasPickInteriorPoint (HalfPlaneType r) 2 r where
  pointInteriorTo = \case
    Downward _ (Down b) -> Point2 0 b
    Leftward (Down x)   -> Point2 x 0
    Rightward x         -> Point2 x 0
    Upward _ b          -> Point2 0 b




-- instance AsLine (LinePV 2 r) where
--   asLine l = case toLinearFunction l of
--                Just

-- | Test whether the halfplane is a left or a right halfplane
isLeftHalfPlane   :: ( HalfPlane_ halfPlane r, Ord r, Num r
                     , HasIntersectionWith (Point 2 r) halfPlane
                     , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                     , GetDirection (BoundingHyperPlane halfPlane 2 r)
                     ) => halfPlane -> Bool
isLeftHalfPlane h = let l             = h^.boundingHyperPlane
                        a             = pointInteriorTo l
                        (Vector2 x y) = inLineVector l
                        w = Vector2 (-y) x -- perpendicular to v; pointing left
                    in (a .+^ w) `intersects` h

-- | Test hether the halfplane is a right halfplane
isRightHalfPlane :: ( HalfPlane_ halfPlane r, Ord r, Num r
                    , HasIntersectionWith (Point 2 r) halfPlane
                    , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                    , GetDirection (BoundingHyperPlane halfPlane 2 r)
                    ) => halfPlane -> Bool
isRightHalfPlane = not . isLeftHalfPlane


-- | Compute the halfplane type
halfPlaneType   :: (HalfPlane_ halfPlane r
                   , Ord r, Num r
                   , AsLine (BoundingHyperPlane halfPlane 2 r)
                   , HasIntersectionWith (Point 2 r) halfPlane
                   , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                   , GetDirection (BoundingHyperPlane halfPlane 2 r)
                   )
                => halfPlane -> HalfPlaneType r
halfPlaneType h = case h^.boundingHyperPlane.to asLine of
    VerticalLineThrough x
      | isLeftHalfPlane h -> Leftward (Down x)
      | otherwise         -> Rightward x
    NonVertical (LineEQ a b)
      | isLeftHalfPlane h -> Downward a (Down b)
      | otherwise         -> Upward a b



  -- case bimap extrems order $ partitionhalfPlanes hs0 of








--------------------------------------------------------------------------------
-- The rest was stuff I tried to use for teh sweep line thing



  -- case bimap extremes boundaries $ partitionhalfPlanes hs0 of


-- -- | Given the upper
-- combine                         :: forall halfPlane line r.
--                                    ( HalfPlane_ halfPlane r
--                                    , NonVerticalHyperPlane_ line 2 r
--                                    , line ~ BoundingHyperPlane halfPlane 2 r
--                                    , Intersection line line ~ LineLineIntersection line
--                                    , Num r, Ord r)
--                                 => Chain Seq r halfPlane
--                                 -> Chain Seq r halfPlane
--                                 -> Chain Seq r halfPlane

-- combine upper lower = u


-- -- we
-- data Status r halfPlane = Status { leftMost :: Maybe (Point 2 r)
--                                  , upper    :: Chain Seq r halfPlane
--                                  , lower    :: Chain Seq r halfPlane
--                                  }

--                                 -> Maybe (CommonIntersection halfPlane r)
-- combine upperChain lowerChain = case u `intersect` l of
--     Nothing
--       | eval u < eval l  -> Nothing -- no intersection
--       | otherwise        -> undefined -- we start with a slab
--                                   -- in this case u must be strictly above l
--     Just (Line_x_Line_Point p) -> case (restUpper,restLower) of
--       (Nothing, Nothing) -> -- solution is a rightward cone with apex p bounded by l and u
--       (Nothing, Just v)  -> case p^.xCoord `compare` v^.xCoord of
--                               LT -> -- kill the leftmost chain


--       sweep p upperChain lowerChain
--     Just (Line_x_Line_Line _)  -> undefined -- we start with a degenerate slab
--       --  |  isNothing restUpper && isNothing restLower ->
--       --     InSubLine undefined (Vector2 uh lh) undefined

--   where
--     eval :: line -> r
--     eval = evalAt (origin :: Point 1 r)

--     (uh,restUpper) = unconsChain upperChain
--     (ul,restLower) = unconsChain upperChain
--     u = uh^.boundingHyperPlane
--     l = ul^.boundingHyperPlane

--     firstEvent = let f = view (re _TopMaybe)
--                  in minBy' fst <$> f restUpper <*> f restLower

-- -- go p upperChain lowerChain =

--     minBy' f x y = if f x <= f y then (x,y) else (y,x)





-- unconsChain    :: Chain f r halfPlane -> (halfPlane, Maybe (Point 2 r, Chain f r halfPlane))
-- unconsChain c = case unconsAlt $ c^._ChainAlternating of
--   Left h             -> (h, Nothing)
--   Right ((h,v),rest) -> (h, Just (v, coerce rest))


-- sweep = undefined

-- --   sweep Nothing

-- -- upperChain0 lowerChain0 = sweep $ Status Nothing

-- -- upperChain0 lowerChain0


-- -- sweep :: Maybe (Status r halfPlane) ->









--------------------------------------------------------------------------------

-- class IpeDraw

-- ipeDraw        :: (NumType geom ~ r
--                   ) => [IpeAttributes i r] -> geom -> [IpeObject r]
-- ipeDraw ats g = [ iO $ defIO g

--   mkIpeObject $ () :+ moncat ats

--                 ]

below :: LineEQ r -> HalfSpaceF (LineEQ r)
below = HalfSpace Negative

above :: LineEQ r -> HalfSpaceF (LineEQ r)
above = HalfSpace Positive





--------------------------------------------------------------------------------


instance (Ord r, Fractional r)
         => HasDefaultIpeOut (CommonIntersection (HalfPlane r) r) where
  type DefaultIpeOut (CommonIntersection (HalfPlane r) r)  = Group
  defIO = drawIntersection

--
drawIntersection :: forall r.
                    (Fractional r, Ord r)
                 => IpeOut (CommonIntersection (HalfPlane r) r) Group r
drawIntersection = \case
  SingletonPoint p _hs  -> ipeGroup [iO $ defIO p]
  InSubLine _l _hs _sl  -> ipeGroup [] -- TODO
  InSlab _hl            -> ipeGroup [] -- TODO
  BoundedRegion pg      -> ipeGroup [iO $ defIO (pg&vertices %~ view core
                                                  :: ConvexPolygon (Point 2 r)
                                                ) ! attr SFill blue
                                                  ! attr SOpacity "10%"
                                    ]
  UnboundedRegion chain -> ipeGroup [iO $ defIO chain ! attr SFill green
                                                      ! attr SOpacity "10%"
                                    ]
