{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
  (main) where

import System.OsPath
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad.IO.Class
import Data.Default
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Functor.Classes
import Data.Semigroup.Foldable
import Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import GHC.Generics (Generic)
import HGeometry.Box
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Polygon
import HGeometry.Polygon.WithHoles
import HGeometry.Vector
import HGeometry.Matrix
import HGeometry.BezierSpline
import HGeometry.Vector.NonEmpty.Util ()
import Hiraffe.Graph
import Data.Kind (Type)
import Data.Coerce
import Data.Distributive
import Ipe
import HGeometry.Number.Real.Rational
import CatmulRomSpline
import System.Random
import System.Random.Stateful
import HGeometry.LineSegment
import Ipe.Draw
import Prelude hiding (sqrt)
import HGeometry.Number.Radical
import HGeometry.PlaneGraph.Class
import Hachuring
import Data.Sequence as Seq
import Debug.Trace
import Ipe.Color
-- import Data.Functor.Apply (WrappedApplicative(..))
import HGeometry.Foldable.Util
import Ipe.Color
import Data.Functor.Contravariant
import Handy
import HGeometry.Miso.Svg.Draw
import Miso.Svg (svg_)
import Miso.Html.Property(width_, height_)
import HGeometry.Miso.Svg(renderSvgToFile)

--------------------------------------------------------------------------------

type R = Double -- RealNumber 5
-- type R = RealNumber 5


--------------------------------------------------------------------------------

spline :: CatmulRomSegment (Point 2 R)
spline = CatmulRomSegment (Point2 (-1) 1) (Point2 0 0) (Point2 10 0) (Point2 11 1)



--------------------------------------------------------------------------------





--------------------------------------------------------------------------------0

main :: IO ()
main = do -- print $ coordinateWise (prefix :: Vector 4 R -> Vector 2 R)
          --                        (Vector4 (Point3 1 2 3 :: Point 3 R)
          --                                 (Point3 4 5 6 :: Point 3 R)
          --                                 (Point3 7 8 9 :: Point 3 R)
          --                                 (Point3 1 2 3 :: Point 3 R)
          --                        )
          -- printAsIpeSelection [toCubicBezier spline]
          -- (v :: Vector 2 Int) <- uniformIn globalStdGen 10

          -- printAsIpeSelection $ foldMap (draw @(Ipe R) [])
          --                     $ catmulRom origin (Point2 50 5) (Point2 75 5) (Point2 100 0)

          let handyCfg = def :: HandyConfig R
              seg :: ClosedLineSegment (Point 2 R)
              seg = ClosedLineSegment (Point2 0 0) (Point2 100 10)

              poly :: SimplePolygon (Point 2 R)
              Just poly = fromPoints
                [ Point2 32 112
                , Point2 160 304
                , Point2 192 176
                , Point2 320 240
                , Point2 336 64
                , Point2 160 32
                , Point2 224 112
                , Point2 96 64
                , Point2 48 80
                ]
              h = hachuring (Vector2 1 (-10)) poly
              res = concat
                    [ draw @(Ipe R) [] poly
                    , draw @(Ipe R) [stroke ?~ blue] h
                    ]
          -- print h
          res <- draw @(Handy (Ipe R) R (AtomicGenM StdGen) IO)
                      [ stroke ?~ black
                      , fill   ?~ blue
                      ] poly handyCfg globalStdGen
          printAsIpeSelection (res :: [IpeObject R])

          content <- draw @(Handy SVG R (AtomicGenM StdGen) IO)
                            [ stroke ?~ black
                            , fill   ?~ blue
                            ] poly handyCfg globalStdGen



          renderSvgToFile [osp|/tmp/out.svg|] $
            svg_ [ width_  "800"
                 , height_ "600"
                 ] (content
                    <> draw @(SVG) [
                                   ] (Label "foo" (Point2 200 500))

                   )

          -- mapM_ print $ poly^..outgoingDartsOf 3.withIndex
          -- traverseOf_ (darts.withIndex) print poly
