{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Use ++" -}
module HalfPlane.Intersect
  ( commonIntersection
  , myMain, testMain
  , myCone, myCone2, debug
  , spec
  ) where

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
import           Test.Hspec.QuickCheck
import           Test.QuickCheck(counterexample, (===), suchThat, discard, Arbitrary(..))

import           Debug.Trace
--------------------------------------------------------------------------------

type HalfPlane r = HalfSpaceF (LinePV 2 r)


--------------------------------------------------------------------------------

-- TODO move to kernel-quickcheck
instance (Arbitrary r, Arbitrary point, Arbitrary edge, Num r, Ord r, Point_ point 2 r
         ) => Arbitrary (Cone r point edge) where
  arbitrary = do a <- arbitrary
                 l <- arbitrary `suchThat` (/= zero)
                 r <- arbitrary `suchThat`
                        (\r' -> r' /= zero &&
                                ccw (origin @(Point 2 r)) (Point l) (Point r') == CW)
                 Vector2 x y <- arbitrary
                 pure $ Cone a (l :+ x) (r :+ y)

--------------------------------------------------------------------------------

type instance Intersection (LinePV 2 r) (LineEQ r) =
  Maybe (LineLineIntersection (LinePV 2 r))

instance (Eq r, Num r) => HasIntersectionWith (LinePV 2 r) (LineEQ r) where
  linePV `intersects` lineEQ = lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (LinePV 2 r) (LineEQ r) where
  linePV `intersect` lineEQ = (linePV <$) <$> lineEQ `intersect` linePV
   -- we use the LineEQ x LinePV instance to compute the intersection. If
   -- the lines are the same; then we just replace the line in the LineLineIntersection
   -- by the linePV

--------------------------------------------------------------------------------

type instance Intersection (VerticalOrLineEQ r) (LinePV 2 r) =
  Maybe (LineLineIntersection (VerticalOrLineEQ r))

instance (Eq r, Num r) => HasIntersectionWith (VerticalOrLineEQ r) (LinePV 2 r) where
  line `intersects` linePV = case line of
    VerticalLineThrough x -> linePV^.direction.xComponent /= 0 ||
                             linePV^.anchorPoint.xCoord == x
    NonVertical lineEQ    -> lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (VerticalOrLineEQ r) (LinePV 2 r) where
  line `intersect` linePV = line `intersect` toLinearFunction linePV
  {-# INLINE intersect #-}

----------------------------------------
-- * the symmetric instances

type instance Intersection (LinePV 2 r) (VerticalOrLineEQ r) =
  Maybe (LineLineIntersection (LinePV 2 r))

instance (Eq r, Num r) => HasIntersectionWith (LinePV 2 r) (VerticalOrLineEQ r) where
  linePV `intersects` lineEQ = lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (LinePV 2 r) (VerticalOrLineEQ r) where
  linePV `intersect` lineEQ = (linePV <$) <$> lineEQ `intersect` linePV
   -- we use the LineEQ x LinePV instance to compute the intersection. If
   -- the lines are the same; then we just replace the line in the LineLineIntersection
   -- by the linePV


----------------------------------------
-- * the symmetric instances

type instance Intersection (VerticalOrLineEQ r) (LineEQ r) =
  Maybe (LineLineIntersection (VerticalOrLineEQ r))

instance (Eq r, Num r) => HasIntersectionWith (VerticalOrLineEQ r) (LineEQ r) where
  line `intersects` lineEQ = lineEQ `intersects` line
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (VerticalOrLineEQ r) (LineEQ r) where
  line `intersect` lineEQ = (line <$) <$> lineEQ `intersect` line

--------------------------------------------------------------------------------

-- class AsOrientedLine line where
--   -- | Convert the given line into an canonical oriented line
--   asOrientedLine :: (d ~ Dimension line, r ~ NumType line) => line -> LinePV d r

-- instance AsOrientedLine (LinePV d r) where
--   asOrientedLine = id

--------------------------------------------------------------------------------
-- move to Point.Either

instance (HasIntersectionWith point geom, HasIntersectionWith extra geom

         ) => HasIntersectionWith (OriginalOrExtra point extra) geom where
  q `intersects` geom = case q of
    Original q' -> q' `intersects` geom
    Extra    q' -> q' `intersects` geom


type instance Intersection (OriginalOrExtra point extra) geom =
  Maybe (OriginalOrExtra point extra)

instance (HasIntersectionWith point geom, HasIntersectionWith extra geom
         ) => IsIntersectableWith (OriginalOrExtra point extra) geom where
  q `intersect` geom
    | q `intersects` geom = Just q
    | otherwise           = Nothing

--------------------------------------------------------------------------------


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
         spec'


spec' :: Spec
spec' = describe "Cone properties" $ do
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





toHalfPlane :: HalfLine (Point 2 R) :+ attrs -> HalfPlane R
toHalfPlane = rightHalfPlane
              . asOrientedLine . view core

inFile :: IO OsPath
inFile  = getDataFileName [osp|test-with-ipe/golden/HalfPlane/intersection.ipe|]

--- >>> show outPath
outPath :: IO OsPath
outPath = getDataFileName [osp|test-with-ipe/golden/HalfPlane/intersection.out.ipe|]


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


testMain = do halfPlanes' <- fmap toHalfPlane <$> (readAllFrom =<< inFile)
              -- let halfPlanes' :: [HalfPlane R]
              --     halfPlanes' = [ HalfSpace Negative (LinePV (Point2 0 3) (Vector2 0 (-2.33334)))
              --                   , HalfSpace Positive (LinePV (Point2 0 (-1.33334)) (Vector2 2.33333 0.33333))
              --                   ]
              traverse print halfPlanes'
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
                Nothing    -> print "error no halfplanes"
                Just file -> do outPath' <- outPath
                                writeIpeFile outPath' $ file&styles %~ (opacitiesStyle:)

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
                                 [ [iO $ defIO res ! attr SLayer "output"
                                   ]
                                 , [ iO $ drawAsConstraint gray h ! attr SLayer "constraint"
                                   | h <- halfPlanes'
                                   ]
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
                                                ) ! attr SFill blue
                                                  ! attr SOpacity "10%"
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


instance (Point_ point 2 r, Fractional r, Ord r, Show r
         ) => HasDefaultIpeOut (Cone r point edge) where
  type DefaultIpeOut (Cone r point edge) = Group
  defIO c = ipeGroup [ iO $ defIO poly ! attr SFill blue
                     , iO $ ipeRay hl
                     , iO $ ipeRay hr
                     ]
    where
      hl = leftBoundary c'  ^.core
      hr = rightBoundary c' ^.core

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
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersects = intersectsLineHalfplane

instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersects = intersectsLineHalfplane

instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (LineEQ r)) where
  intersects = intersectsLineHalfplane
instance ( Num r, Ord r
         ) => HasIntersectionWith (VerticalOrLineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
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

type instance Intersection (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))
type instance Intersection (LinePV 2 r) (HalfSpaceF (LineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))
type instance Intersection (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LinePV 2 r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (LineEQ r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LinePV 2 r) (HalfSpaceF (VerticalOrLineEQ r)) where
  intersect = intersectLineHalfplane


type instance Intersection (LineEQ r) (HalfSpaceF (LinePV 2 r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))
type instance Intersection (LineEQ r) (HalfSpaceF (LineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))
type instance Intersection (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) =
  Maybe (LineHalfPlaneIntersection r (LineEQ r))

instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (LineEQ r)) where
  intersect = intersectLineHalfplane
instance ( Fractional r, Ord r
         ) => IsIntersectableWith (LineEQ r) (HalfSpaceF (VerticalOrLineEQ r)) where
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

type instance Intersection (HalfSpaceF (LinePV 2 r)) (HalfSpaceF (LinePV 2 r)) =
  Maybe (HalfPlaneIntersection r (HalfSpaceF (LinePV 2 r)))

instance ( Fractional r, Ord r
         , Show r
         ) => IsIntersectableWith (HalfSpaceF (LinePV 2 r)) (HalfSpaceF (LinePV 2 r)) where
  intersect = intersectTwo

type instance Intersection (HalfSpaceF (LineEQ r)) (HalfSpaceF (LineEQ r)) =
  Maybe (HalfPlaneIntersection r (HalfSpaceF (LineEQ r)))

instance ( Fractional r, Ord r
         , Show r
         ) => IsIntersectableWith (HalfSpaceF (LineEQ r)) (HalfSpaceF (LineEQ r)) where
  intersect = intersectTwo

--------------------------------------------------------------------------------



-- | Get the bisector of the cone
coneBisector   :: (Point_ point 2 r, Num r) => Cone r point edge -> HalfLine point
coneBisector c = HalfLine (c^.apex)
                         ((c^.leftBoundaryVector.core) ^+^ (c^.rightBoundaryVector.core))


instance ( Point_ point 2 r, Num r, Ord r
         ) => Point 2 r `HasIntersectionWith` Cone r point edge where
  q `intersects` cone = all (q `intersects`) (intersectingHalfplanes cone)
  {-# INLINE intersects #-}


-- | Get the two halfplanes so that the cone is the intersection of the two halfplanes.
-- the first halfplane is the plane right of the left boundary, whereas the
-- second halfplane is the plane left of the right boundary.
intersectingHalfplanes   :: ( Point_ point 2 r, Num r, Ord r)
                         => Cone r point edge -> Vector 2 (HalfSpaceF (LinePV 2 r))
intersectingHalfplanes c = Vector2 (rightHalfPlane $ LinePV a leftB)
                                   (leftHalfPlane  $ LinePV a rightB)
  where
    a      = c^.apex.asPoint
    leftB  = c^.leftBoundaryVector.core
    rightB = c^.rightBoundaryVector.core
{-# INLINE intersectingHalfplanes #-}

--------------------------------------------------------------------------------

-- debug2 = let a = Point2 100 100
--              rect = defaultBox
--          in extraPoints (HalfLine a left) (HalfLine right)
-- -- TODO: write a property check that extraPoints retursn only points inside the cone!




-- | Computes the intersection of two halfplanes
intersectTwo :: forall halfPlane r.
                ( HalfPlane_ halfPlane r
                , Fractional r, Ord r
                , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                , HasIntersectionWith (Point 2 r) halfPlane
                , HasPickInteriorPoint (BoundingHyperPlane halfPlane 2 r) 2 r
                , GetDirection (BoundingHyperPlane halfPlane 2 r)
                , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r) (BoundingHyperPlane halfPlane 2 r)
                , Intersection (BoundingHyperPlane halfPlane 2 r)
                  (BoundingHyperPlane halfPlane 2 r)
                  ~ Maybe (LineLineIntersection (BoundingHyperPlane halfPlane 2 r))


                , Show r
                ) => halfPlane -> halfPlane -> Maybe (HalfPlaneIntersection r halfPlane)
intersectTwo h1 h2 = case l1 `intersect` l2 of
    Nothing -> case (pointInteriorTo l1 `intersects` h2, pointInteriorTo l2 `intersects` h1) of
      (False,False) -> Nothing
      (True, False) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h1
      (False, True) -> Just $ HalfPlane_x_HalfPlane_HalfPlane h2
      (True,True)   -> Just $ HalfPlane_x_HalfPlane_Slab (Slab' h1 h2)
        -- todo; make slab into a better type
    Just i  -> Just $ case i of
      Line_x_Line_Line l  -> let n = normalVector l
                                 q = pointInteriorTo l .+^ n
                             in if q `intersects` h1 == q `intersects` h2
                                then HalfPlane_x_HalfPlane_HalfPlane h1 -- same halfplane
                                else HalfPlane_x_HalfPlane_Line l -- oppositive halfplanes
      Line_x_Line_Point a -> HalfPlane_x_HalfPlane_Cone $ Cone a left right
        where
          v1 = inLineVector l1
          v2 = inLineVector l2

          (left,right)
            | isLeftHalfPlane v1 h1 = case ccw (origin :: Point 2 r) (Point v1) (Point v2) of
                CCW | isLeftHalfPlane v2 h2 -> (negated v1 :+ h1, v2 :+ h2)
                    | otherwise             -> (v2 :+ h2,         v1 :+ h1)
                CW  | isLeftHalfPlane v2 h2 -> (negated v2 :+ h2, v1 :+ h1)
                    | otherwise             -> (negated v1 :+ h1, negated v2 :+ h2)
                CoLinear -> error "absurd"
            | otherwise            = case ccw (origin :: Point 2 r) (Point v1) (Point v2) of
                CCW | isLeftHalfPlane v2 h2 -> (negated v2 :+ h2, negated v1 :+ h1)
                    | otherwise             -> (v1 :+ h1,         negated v2 :+ h2)
                CW  | isLeftHalfPlane v2 h2 -> (v1 :+ h1,         v2 :+ h2)
                    | otherwise             -> (v2 :+ h2,         negated v1 :+ h1)
                CoLinear -> error "absurd"

          isLeftHalfPlane (Vector2 x y) h = let w = Vector2 (-y) x
                                                -- perpendicular to v; pointing left
                                            in (a .+^ w) `intersects` h
          --   = case


          --   (False,False) -> traceShow "woei" $ (negated v2 :+ h2, negated v1 :+ h1)
          --   (False,True)  -> traceShow (v1,v2) $
          --     (negated v1 :+ h1,         v2 :+ h2)
          --   (True,False)  -> (v1 :+ h1,         negated v2 :+ h2)
          --   (True,True)   -> (v2         :+ h2,         v1 :+ h1)


          --   |
          --   | otherwise = undefined


          --   = case (h1^.halfSpaceSign)


          -- cones = [ Cone a (negated v1 :+ h1) (negated v2 :+ h2)
          --         , Cone a (negated v1 :+ h1) (v2 :+ h2)
          --         , Cone a (v1 :+ h1)         (negated v2 :+ h2)
          --         , Cone a (v1 :+ h1)         (v2 :+ h2)
          --         ]



          -- (left,right) = case ((a .+^ v1) `intersects` h2, a .+^ v2 `intersects` h1) of
          --   (False,False) -> traceShow "woei" $ (negated v2 :+ h2, negated v1 :+ h1)
          --   (False,True)  -> traceShow (v1,v2) $
          --     (negated v1 :+ h1,         v2 :+ h2)
          --   (True,False)  -> (v1 :+ h1,         negated v2 :+ h2)
          --   (True,True)   -> (v2         :+ h2,         v1 :+ h1)

          -- v1 = leftVec h1
          -- v2 = leftVec h2

  where
    l1 = h1^.boundingHyperPlane
    l2 = h2^.boundingHyperPlane

    -- -- | Given a halfplane; compute the vector v so that the halfplane is to the left
    -- -- of the vector.
    -- leftVec h = let v = inLineVector (h^.boundingHyperPlane)
    --             in case h^.halfSpaceSign of
    --                  Negative -> v
    --                  Positive -> negated v





    -- regular  h = leftVec h           :+ h
    -- negated' h = negated (leftVec h) :+ h




    -- -- the following assignment assumes that the vectors v1 and v2 point rightward
    -- (left,right) = case (h1^.halfSpaceSign, h2^.halfSpaceSign) of
    --   (Negative,Negative) -> (regular  h1, negated' h2)
    --   (Negative,Positive) -> (negated' h2, negated' h1)
    --   (Positive,Negative) -> (negated' h2, regular  h1)
    --   (Positive,Positive) -> (negated' h1, regular  h2)


  -- FIXME: I guess I can compute whether a + v1 lies inside h2
  -- to decide left right?



    -- f s s' | s' == s   = negated
    --        | otherwise = id

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

below :: LineEQ r -> HalfSpaceF (LineEQ r)
below = HalfSpace Negative

above :: LineEQ r -> HalfSpaceF (LineEQ r)
above = HalfSpace Positive
