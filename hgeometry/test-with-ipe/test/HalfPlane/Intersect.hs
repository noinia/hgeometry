{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore "Use ++" -}
module HalfPlane.Intersect
  ( commonIntersection
  , myMain, testMain
  , spec
  ) where

import           HGeometry.HalfPlane.CommonIntersection.Chain
import           HGeometry.HalfPlane.CommonIntersection (CommonIntersection(..))
import           HGeometry.HalfSpace
import           HGeometry.Cyclic
import           HGeometry.HyperPlane.Class
import           HGeometry.Unbounded
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           HGeometry.Point
import           HGeometry.Line
import           HGeometry.Kernel
import           Control.Lens
import           HGeometry.Sequence.Alternating
import           Data.Coerce
import           Data.Foldable1
import           Data.Ord (Down (..))
import           HGeometry.Foldable.Sort (sortOnCheap)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.NonEmpty as NonEmptyVector
import qualified Data.Vector as Vector
import           Ipe
import           Ipe.Color
import           HGeometry.HalfLine
import           HGeometry.Ext
import           Witherable (mapMaybe)
import           System.OsPath
import qualified Data.List.NonEmpty as NonEmpty
import           R
import           Golden (getDataFileName)
import           HalfPlane.CommonIntersectionSpec ()
import           HGeometry.Polygon
import           HGeometry.Cone
import           Test.Hspec
import           HalfPlane.Cone.IntersectRect

import           Debug.Trace
--------------------------------------------------------------------------------

type HalfPlane r = HalfSpaceF (LinePV 2 r)


-- class AsOrientedLine line where
--   -- | Convert the given line into an canonical oriented line
--   asOrientedLine :: (d ~ Dimension line, r ~ NumType line) => line -> LinePV d r

-- instance AsOrientedLine (LinePV d r) where
--   asOrientedLine = id


spec :: Spec
spec = describe "rightHalfPlane correct" $ do
         it "vertical" $
           (origin :: Point 2 R)
           `intersects`
           (rightHalfPlane (LinePV (Point2 1 10) (Vector2 0 (-10))) :: HalfPlane R)
         -- it "testz"
         it "normalVec" $
           normalVector (LinePV (Point2 224 256) (Vector2 0 (-176)))
           `shouldBe`
           Vector2 (-1) 0


toHalfPlane :: HalfLine (Point 2 R) :+ attrs -> HalfPlane R
toHalfPlane = traceShowId . rightHalfPlane . traceShowId
              . asOrientedLine . view core

inFile :: IO OsPath
inFile  = getDataFileName [osp|test-with-ipe/golden/Halfplane/intersection.ipe|]

--- >>> show outPath
outPath :: IO OsPath
outPath = getDataFileName [osp|test-with-ipe/golden/Halfplane/intersection.out.ipe|]


testMain = do halfPlanes' <- fmap toHalfPlane <$> (readAllFrom =<< inFile)
              case halfPlanes' of
                (h1:h2:_) -> do let res = h1 `intersect` h2
                                print res
                                outPath' <- outPath
                                writeIpeFile outPath' . singlePageFromContent . concat $
                                  [ [iO $ defIO res ]
                                  , iO . drawAsConstraint gray <$> [h1,h2]
                                  ]

              -- case NonEmpty.nonEmpty halfPlanes' of
              --   Nothing         -> print "error; no halfplanes"
              --   Just halfPlanes -> case commonIntersection halfPlanes of
              --     Nothing  -> putStrLn "no intersection?"
              --     Just res -> do print res
              --                    outPath' <- outPath
              --                    writeIpeFile outPath' . singlePageFromContent . concat $
              --                      [ [iO $ defIO res ]
              --                      , iO . drawAsConstraint gray <$> halfPlanes'
              --                      ]


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
                                 [ [iO $ defIO res ]
                                 , iO . drawAsConstraint gray <$> halfPlanes'
                                 ]

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
  Slab _hl _hr          -> ipeGroup [] -- TODO
  BoundedRegion pg      -> ipeGroup [iO $ defIO (pg&vertices %~ view core
                                                  :: ConvexPolygon (Point 2 r)
                                                )
                                    ]
  UnboundedRegion chain -> ipeGroup [iO $ defIO chain]

--------------------------------------------------------------------------------


data State r halfPlane = OneChain (Chain Seq r halfPlane)
                       | TwoChains (Chain Seq r halfPlane) (Chain Seq r halfPlane)

joinChains pref suf = pref -- FIXME!!!

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

data Slab' halfPlane = Slab' halfPlane halfPlane
  deriving (Show,Eq)

type instance NumType   (Slab' halfPlane) = NumType halfPlane
type instance Dimension (Slab' halfPlane) = 2


--------------------------------------------------------------------------------

instance (HalfPlane_ halfPlane r
         ) => HasDefaultIpeOut (Slab' halfPlane) where
  type DefaultIpeOut (Slab' halfPlane) = Group
  defIO (Slab' h1 h2) = ipeGroup []

    -- ipeGroup [ iO $ defIO (h1^.boundingHyperPlane)
    --                              , iO $ defIO (h2^.boundingHyperPlane)
    --                              ]
    -- -- TODO: do this property; i.e. compute the intersection with the bounding box


instance (Point_ point 2 r, Fractional r, Ord r
         ) => HasDefaultIpeOut (Cone r point edge) where
  type DefaultIpeOut (Cone r point edge) = Group
  defIO c = ipeGroup [ iO $ defIO poly ]
    where
      poly :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 r)
      poly = toConvexPolygonIn rect c'&vertices %~ (^.asPoint)
      c' :: Cone r (Point 2 r) edge
      c' = c&apex %~ (^.asPoint)
      rect :: Rectangle (Point 2 r)
      rect = boundingBox $ defaultBox :| [boundingBox (c'^.apex) ]
      -- we have to include the bounding box of the apex since toConvexPolygonIn
      -- requires the apex to be in the box.

--------------------------------------------------------------------------------


-- | The non-empty intersection of two halfPlanes
data HalfPlaneIntersection r halfPlane =
    HalfPlane_x_HalfPlane_Line (BoundingHyperPlane halfPlane 2 r)
  | HalfPlane_x_HalfPlane_Slab (Slab' halfPlane)
  | HalfPlane_x_HalfPlane_Cone (Cone r (Point 2 r) halfPlane)
  | HalfPlane_x_HalfPlane_HalfPlane halfPlane

deriving instance (Show r, Show halfPlane, Show (BoundingHyperPlane halfPlane 2 r)
                  ) => Show (HalfPlaneIntersection r halfPlane)
deriving instance (Eq r, Eq halfPlane, Eq (BoundingHyperPlane halfPlane 2 r)
                  ) => Eq (HalfPlaneIntersection r halfPlane)

type instance NumType   (HalfPlaneIntersection r halfPlane) = r
type instance Dimension (HalfPlaneIntersection r halfPlane) = 2

data LineHalfPlaneIntersection r line = Line_x_HalfPlane_Line     line
                                      | Line_x_HalfPlane_HalfLine (HalfLine (Point 2 r))
  deriving (Show,Eq)


type instance NumType   (LineHalfPlaneIntersection r line) = r
type instance Dimension (LineHalfPlaneIntersection r line) = 2

--------------------------------------------------------------------------------

instance HasDefaultIpeOut line => HasDefaultIpeOut (Maybe line)  where
  type DefaultIpeOut (Maybe line) = Group
  defIO = \case
    Nothing -> ipeGroup []
    Just g  -> ipeGroup [iO $ defIO g]

instance ( HasDefaultIpeOut line, Fractional r, Ord r, Show r
         , DefaultIpeOut line ~ Path
         , NumType line ~ r
         ) => HasDefaultIpeOut (LineHalfPlaneIntersection r line) where
  type DefaultIpeOut (LineHalfPlaneIntersection r line) = Path
  defIO = \case
    Line_x_HalfPlane_Line line   -> defIO line
    Line_x_HalfPlane_HalfLine hl -> defIO hl


instance ( HasDefaultIpeOut halfPlane
         , HasDefaultIpeOut (BoundingHyperPlane halfPlane 2 r)
         , HalfPlane_ halfPlane r
         , Fractional r, Ord r
         , Show r
         -- , DefaultIpeOut halfPlane ~ Path
         , NumType halfPlane ~ r
         , Dimension halfPlane ~ 2
         , Dimension (BoundingHyperPlane halfPlane 2 r) ~ 2
         , NumType (BoundingHyperPlane halfPlane 2 r) ~ r
         ) => HasDefaultIpeOut (HalfPlaneIntersection r halfPlane) where
  type DefaultIpeOut (HalfPlaneIntersection r halfPlane) = Group
  defIO = \case
    HalfPlane_x_HalfPlane_Line line           -> ipeGroup [iO $ defIO line]
    HalfPlane_x_HalfPlane_Slab slab           -> ipeGroup [iO $ defIO slab]
    HalfPlane_x_HalfPlane_Cone cone           -> ipeGroup [iO $ defIO cone]
    HalfPlane_x_HalfPlane_HalfPlane halfPlane -> ipeGroup [iO $ defIO halfPlane ]

--------------------------------------------------------------------------------
class GetDirection line where
  -- | Get a vector v that lies in the line; i.e. given a point
  -- p that lies on the line; the point p .+^ v lies also in the line.
  inLineVector :: (r ~ NumType line, d ~ Dimension line) => line -> Vector d r

instance HasDirection line => GetDirection line where
  inLineVector = view direction
instance {-# OVERLAPPING #-} Num r => GetDirection (LineEQ r) where
  inLineVector (LineEQ a _) = Vector2 1 a
  -- not sure why it thinks this is overlapping, but whatever
instance Num r => GetDirection (VerticalOrLineEQ r) where
  inLineVector = \case
    VerticalLineThrough _ -> Vector2 0 1
    NonVertical l         -> inLineVector l
--------------------------------------------------------------------------------

instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfPlane r) where
  intersects = intersectsLineHalfplane

-- | test if a line and a halfplane intersect
intersectsLineHalfplane     :: ( HalfPlane_ halfPlane r
                               , Num r, Ord r
                               , HyperPlane_ line 2 r
                               , HasPickInteriorPoint line 2 r
                               , HasIntersectionWith line (BoundingHyperPlane halfPlane 2 r)
                               , HasIntersectionWith (Point 2 r) halfPlane
                               ) => line -> halfPlane -> Bool
intersectsLineHalfplane l h = (l `intersects` (h^.boundingHyperPlane)) ||
                              (pointInteriorTo l `intersects` h)

--------------------------------------------------------------------------------

type instance Intersection (LinePV 2 r) (HalfPlane r) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfPlane r) where
  intersect = intersectLineHalfplane

--------------------------------------------------------------------------------

-- | Insertsect a line and a halfplane
intersectLineHalfplane   :: ( HalfPlane_ halfPlane r
                            , Fractional r, Ord r
                            , HyperPlane_ line 2 r
                            , HasPickInteriorPoint line 2 r
                            , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                            , IsIntersectableWith line (BoundingHyperPlane halfPlane 2 r)
                            , GetDirection line
                            , HasIntersectionWith (Point 2 r) halfPlane
                            , Intersection line (BoundingHyperPlane halfPlane 2 r)
                              ~ Maybe (LineLineIntersection line)
                            ) => line -> halfPlane -> Maybe (LineHalfPlaneIntersection r line)
intersectLineHalfplane l h = case l `intersect` (h^.boundingHyperPlane) of
    Nothing
      | pointInteriorTo l `intersects` h -> Just $ Line_x_HalfPlane_Line l
      | otherwise                        -> Nothing
    Just i                               -> Just $ case i of
      Line_x_Line_Line _  -> Line_x_HalfPlane_Line l
      Line_x_Line_Point p -> Line_x_HalfPlane_HalfLine (HalfLine p v)
        where
          v' = inLineVector l
          v = if p .+^ v' `intersects` h then v' else negated v'

--------------------------------------------------------------------------------

type instance Intersection (HalfPlane r) (HalfPlane r) =
  Maybe (HalfPlaneIntersection r (HalfPlane r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (HalfPlane r) (HalfPlane r) where
  intersect = intersectTwo

--------------------------------------------------------------------------------

-- | Computes the intersection of two halfplanes
intersectTwo :: ( HalfPlane_ halfPlane r
                , Fractional r, Ord r
                , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                , HasIntersectionWith (Point 2 r) halfPlane
                , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                , GetDirection (BoundingHyperPlane halfPlane 2 r)
                , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r) (BoundingHyperPlane halfPlane 2 r)
                , Intersection (BoundingHyperPlane halfPlane 2 r)
                  (BoundingHyperPlane halfPlane 2 r)
                  ~ Maybe (LineLineIntersection (BoundingHyperPlane halfPlane 2 r))
                ) => halfPlane -> halfPlane -> Maybe (HalfPlaneIntersection r halfPlane)
intersectTwo h1 h2 = case l1 `intersect` l2 of
    Nothing -> case (pointInteriorTo l1 `intersects` h2, pointInteriorTo l2 `intersects` h1) of
      (False,False) -> Nothing
      (True, False) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h1
      (False, True) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h2
      (True,True)   -> Just $ HalfPlane_x_HalfPlane_Slab (Slab' h1 h2)
        -- todo; make slab into a better type
    Just i  -> case i of
      Line_x_Line_Line l  -> Just $ HalfPlane_x_HalfPlane_Line l
      Line_x_Line_Point a -> Just $ HalfPlane_x_HalfPlane_Cone
                                  $ Cone a (v Positive h1 l1)
                                           (v Negative h2 l2)
  where
    l1 = h1^.boundingHyperPlane
    l2 = h2^.boundingHyperPlane

    f s s' | s' == s   = id
           | otherwise = negated
    v s h l = f s (h^.halfSpaceSign) (inLineVector l) :+ h

  -- case (h1^.boundingHyperPlane) `intersect` h2 of
  --   Nothing
  --     | (h2^.boundingHyperPlane) `intersects` h1 -> Just $ HalfPlane_x_HalfPlane_HalfPlane h2
  --     | otherwise                                -> Nothing
  --   Just i  -> Just $ case i of
  --     Line_x_HalfPlane_Line _
  --       | (h2^.boundingHyperPlane) `intersects` h1 -> HalfPlane_x_HalfPlane_Slab h1 h2
  --       | otherwise                               ->








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
                          )
                       => f halfPlane -> Maybe (CommonIntersection halfPlane r)
commonIntersection = fmap toCommonIntersection
                   . foldlMap1' (Just . OneChain . singleton . snd) extend
                   . NonEmptyVector.unsafeFromVector
                   . dropRedundants
                   . sortOnCheap fst
                   . fmap (\h -> (halfPlaneType h, h))
  where
    toCommonIntersection :: State r halfPlane -> CommonIntersection halfPlane r
    toCommonIntersection = \case
      OneChain chain     -> UnboundedRegion chain
      TwoChains suf pref -> UnboundedRegion $ joinChains pref suf
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
        extendTwo suff chain = undefined
        extendOne chain = case unconsAlt $ chain^._ChainAlternating of
            Left h'          -> case (h^.boundingHyperPlane)
                                     `intersect`
                                     (h'^.boundingHyperPlane) of
              Nothing -> undefined
              Just i  -> Just $ case i of
                Line_x_Line_Line l  -> undefined -- subLine -- orPoint? or duplicate constraint
                Line_x_Line_Point v -> OneChain $ consChain (h,v) chain
                -- TODO: extract this into the intersection type of two halfpsaces

            Right ((h',u),_) -> case (h^.boundingHyperPlane)
                                     `intersect`
                                     (h'^.boundingHyperPlane) of
              Nothing -> undefined -- TODO
                -- slabOrNothing -- FIXME: whether its a slab or not depends on whether chain
                                       -- still has other items as well.
              Just i  -> Just $ case i of
                Line_x_Line_Line l  -> subLine -- orPoint? or duplicate constraint
                Line_x_Line_Point v -> OneChain $ consChain (h,v) chain
                  -- something must bem missing;
          where
            h' = chain^.leftMost
            p = pointInteriorTo t

            slabOrNothing | p `intersects` h' = Just undefined -- slab
                          | otherwise         = Nothing

            subLine = undefined



          --                 case t of
          --                    Leftward (Down x) -> evalAt



-- intersectBoundaries h = \case
--   Rightward (Down x) ->
--   Leftward x         ->


dropRedundants hs = Vector.catMaybes $ Vector.zipWith f hs (Vector.tail hs)
  where
    f z@(t,_) (t',_) = case (t,t') of
      (Leftward _,   Leftward _)                -> Nothing
      (Rightward _,  Rightward _)               -> Nothing
      (Downward a _, Downward a' _) | a == a'   -> Nothing
                                    | otherwise -> Just z
      (Upward a _,   Upward a' _)   | a == a'   -> Nothing
                                    | otherwise -> Just z
      _                                         -> Just z

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



-- | Compute the halfplane type
halfPlaneType   :: (HalfPlane_ halfPlane r, AsLine (BoundingHyperPlane halfPlane 2 r))
                => halfPlane -> HalfPlaneType r
halfPlaneType h = case h^.boundingHyperPlane.to asLine of
                    VerticalLineThrough x    -> case h^.halfSpaceSign of
                                                  Negative -> Leftward (Down x)
                                                  Positive -> Rightward x
                    NonVertical (LineEQ a b) -> case h^.halfSpaceSign of
                                                  Negative -> Downward a (Down b)
                                                  Positive -> Upward a b



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
