{-# LANGUAGE QuasiQuotes #-}
module Plane.ConvexHullDemoSpec where

import           Data.Foldable1
import           System.Random
import           Data.Foldable
import           Golden
import           Test.Hspec
import           Plane.BruteForce
import           Plane.Randomized2
import           Control.Lens hiding (Prism)
import           System.OsPath
import qualified Wavefront
import qualified Codec.Wavefront.Object as Wavefront
import           Codec.Wavefront.Object (objLocations, objFaces)
import           Codec.Wavefront.Location (Location(..))
import           Codec.Wavefront.Face (Face(..),FaceIndex(..))
import           Codec.Wavefront (ElementF(..), Element)
import           R
import qualified Data.Vector as Vector
import qualified Data.Vector.NonEmpty as NonEmptyVector
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import           HGeometry.Triangle
import           HGeometry.Point
import           HGeometry.Ext
import           HGeometry.Duality
import           Plane.ObjEncoder (writeFile)
import           Prelude hiding (writeFile)

import Ipe
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Convex hull via lower envelope" $ do
         pure ()


runCHDemo :: IO ()
runCHDemo = chDemo [osp|test-with-ipe/golden/Render/suzanne/suzanne.obj|]

bruteForceUpperHull :: ( Point_ point 3 r, Ord r, Fractional r, Ord point
                       , Functor set, Foldable set
                       )
                    => set point -> Set.Set (Triangle point)
bruteForceUpperHull = upperHullWith $ bruteForceVerticesIn boundedDomain

-- | The randomized upper hull algorithm (with a fixed seed )
upperHull :: ( Point_ point 3 r, Ord r, Fractional r, Ord point
             , Functor set, Foldable1 set

             , Show point, Show r, IpeWriteText r
             )
          => set point -> Set.Set (Triangle point)
upperHull = upperHullWith $ verticesIn 0 gen boundedDomain . foldMap1 NESet.singleton
  where
    gen = mkStdGen 1234


upperHullWith loweEnv = foldMap toTriangles
                      . loweEnv
                      . fmap (\p -> dualHyperPlane p :+ p)

-- | Our bounded domain
boundedDomain :: Num r => Triangle (Point 2 r)
boundedDomain =
      Triangle (Point2 (negate maxX) (negate maxY))
               (Point2 (negate maxX) maxY)
               (Point2 (10 * maxX) 0)
  where
    maxX = 10000
    maxY = 20000


seed = 10000


toTriangles v = let EnvVertex p q r rest _ _ = view extra <$> v
                in Set.singleton (Triangle p q r)
                   <> case rest of
                        [] -> mempty
                        _  -> mempty -- FIXME: do something with the remaining points
                          -- can we just form a triangle with to p and q?
                        -- error $ "more than three points !? " <> show (length rest)

chDemo       :: OsPath -> IO ()
chDemo path = do fp <- getDataFileName path
                 Wavefront.fromFile fp >>= \case
                   Left err  -> print err
                   Right obj -> do
                     let -- we label them with the locations
                         pts :: Vector.Vector (Point 3 R :+ Int)
                         pts = Vector.imap (\i p ->
                                              over coordinates realToFrac $
                                                Point3 (locX p) (negate $ locZ p) (locY p)
                                                -- somehow the coord system is weird here as well
                                                :+ (i+1)
                                                -- in the OBJ file, indices are 1 indexed
                                           ) (Wavefront.objLocations obj)

                         pts' = NonEmptyVector.unsafeFromVector
                              $ Vector.take 50 pts

                           -- the triangles of the hull
                         tris = upperHull pts'
                         outObj = obj { objLocations = Wavefront.objLocations obj
                                      , objFaces     = fromTriangles tris
                                      }

                     print (length pts)

                     writeFile [osp|/tmp/out.obj|] outObj
                     -- traverse_ print pts'
                     -- traverse_ print tris

fromTriangles :: Foldable set => set (Triangle (point :+ Int))
              -> Vector.Vector (Element Face)
fromTriangles = Vector.fromList
              . fmap (\t -> let Triangle i j k = fmap (^.extra.to fi) t
                            in Element Nothing [] Nothing 0 $ Face i j k []
                     )
              . toList
  where
    fi i = FaceIndex i Nothing Nothing

-- toObject :: (Point_ point 3 r) => Set.Set (Triangle (point :+ Int)) -> WavefrontOBJ
-- toObject tris = WavefrontOBJ
--                  { objLocations = mempty
--                  , objTexCoords = mempty
--                  , objNormals  = mempty
--                  , objPoints   = mempty
--                  , objLines    = mempty
--                  , objFaces    = faces
--                  , objMtlLibs  = mempty
--                  }
--   where
--     locs = mempty
--     faces = mempty

--------------------------------------------------------------------------------
