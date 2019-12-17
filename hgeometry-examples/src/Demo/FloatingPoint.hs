{-# LANGUAGE LambdaCase #-}
module Demo.FloatingPoint where

import Control.Lens
import Data.Data
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Ipe
import Data.Geometry.Ipe.Attributes
import Data.Geometry.Ipe.Color
import Data.Geometry.Ipe.IpeOut
import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.Vector
import Options.Applicative
import Prelude hiding (Float,Double)
import qualified Prelude as Prelude
import Foreign.C.Types(CDouble,CFloat)

--------------------------------------------------------------------------------

data FType = Float | Double deriving (Show,Read,Eq,Data)

data Options = Options FType Int FilePath deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Draws floating point comparison"
               <> header   "FloatingPoint"
               )
  where
    parser = Options <$> option auto (help "floating number type"
                                      <> short 't'
                                      <> metavar "TYPE"
                                     )
                     <*> option auto (help "precision"
                                      <> short 'n'
                                      <> metavar "PRECISION"
                                     )
                     <*> strOption (help "output file"
                                    <> short 'o'
                                    <> metavar "OUTPUT"
                                   )

type Pixel = Point 2 Int

draw                         :: Pixel :+ IpeAttributes Path Int -> IpeObject Int
draw (p@(Point2 x y) :+ ats) = let rect = box (ext p) (ext $ Point2 (x+1) (y+1))
                               in iO $ ipeRectangle rect ! ats

grid     :: (Int -> Int -> extra) -> Int -> [Point 2 Int :+ extra]
grid f n = [ Point2 x y :+ f x y | x <- [1..n], y <- [1..n]]

comp   :: Fractional r => r -> Int -> Int -> Int -> SideTestUpDown
comp _ (fromIntegral -> n) (fromIntegral -> x) (fromIntegral -> y) =
    let l = Line (Point2 24 24) (Vector2 (-12) (-12))
        p = Point2 ((1/2) + x*delta) ((1/2) + y*delta)
        delta = 1/n'
        n' = 2^53
    in p `onSideUpDown` l

colorPixel   :: SideTestUpDown -> IpeAttributes Path Int
colorPixel s = attr SFill $ case s of
    On    -> yellow
    Below -> blue
    Above -> red

mainWith                  :: Options -> IO ()
mainWith (Options t n fp) = writeIpeFile fp . singlePageFromContent . map draw $ grid f n
  where
    f x y = colorPixel $ myF n x y
    myF = case t of
            Float  -> comp (undefined :: Prelude.Float)
            Double -> comp (undefined :: CDouble)


-- sideTest p q r =
