{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.WorldSpec where

import           Control.Lens hiding (elements)
import           Data.Maybe
import           Golden
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances (shrinkPolygon)
import           HGeometry.Polygon.Triangulation
import           HGeometry.Properties
import           Ipe
import           Ipe.Reader
import           System.IO.Unsafe
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
--------------------------------------------------------------------------------

type R = RealNumber 5

worldPath :: OsPath
worldPath = [osp|test-with-ipe/Polygon/Triangulation/world.ipe|]

{-# NOINLINE allCountries #-}
allCountries :: [Country]
allCountries = unsafePerformIO $
                do worldFile <- getDataFileName worldPath
                   polies <- readAllDeepFrom @(SimplePolygon (Point 2 R)) worldFile
                   let polies' = filter (hasNoSelfIntersections . (^.core)) polies
                   return $ (Country . (^.core)) <$> polies'

readAllDeepFrom    :: forall g r. (HasDefaultFromIpe g, r ~ NumType g, Coordinate r, Eq r)
                   => OsPath -> IO [g :+ IpeAttributes (DefaultFromIpe g) r]
readAllDeepFrom fp = foldMap readAllDeep <$> readSinglePageFile fp

newtype Country = Country (SimplePolygon (Point 2 R))
             deriving (Show,Eq)



instance Arbitrary Country where
  arbitrary = elements allCountries
  shrink (Country pg) = Country <$> shrinkPolygon pg


toPG :: SimplePolygon (Point 2 R) -> SimplePolygon (Point 2 R)
toPG = fromJust . fromPoints . toNonEmptyOf outerBoundary



newtype MyPoly = MyPoly (SimplePolygon (Point 2 R))
             deriving (Eq)

instance Show MyPoly where
  show (MyPoly pg) = show (numVertices pg, pg)

instance Arbitrary MyPoly where
  arbitrary = elements [MyPoly myPoly]
  shrink (MyPoly pg) = MyPoly <$> shrinkPolygon pg


spec :: Spec
spec = describe "triangulate World" $ do
         prop "all polygons are CCW" $
           \(Country pg) -> isCounterClockwise pg
         prop "all polygons are CCW after toPolygoning" $
           \(Country pg) -> isCounterClockwise $ toPG pg
         prop "triangulate yields only triangles" $
           \(Country pg) -> let subdivision = triangulate pg
                                triangs = subdivision^..interiorFacePolygons
                            in all isTriangle' triangs

         -- describe "debugging myPoly" $ do
         --   it "small" $
         --     let poly = myPoly in
         --   -- prop "shrinking myPoly" $
         --   --   \(MyPoly poly) ->
         --   --     let n = numVertices poly
         --   --         f = if (n `mod` 10) == 0 then traceShow (n,poly) else id
         --   --     in f $
         --          let subdivision = triangulate poly
         --              triangs = subdivision^..interiorFacePolygons
         --              out = [iO' poly]
         --                <> map (\triang ->
         --                           iO' $ (triang&vertices %~ view core
         --                                   :: SimplePolygon (Point 2 R)
         --                                 )
         --                       ) triangs
         --          in length triangs > 0
         --   -- goldenWith [osp|data/test-with-ipe/Polygon/Triangulation/|]
         --   --       (ipeFileGolden { name = [osp|myPoly|] })
         --   --       (addStyleSheet opacitiesStyle $ singlePageFromContent out)


         -- describe "debugging myPoly Prime" $ do
         --   let subdivision = triangulate myPoly'
         --       triangs = subdivision^..interiorFacePolygons
         --       out = [iO' myPoly']
         --         <> map (\triang ->
         --                                    iO' $ (triang&vertices %~ view core
         --                                          :: SimplePolygon (Point 2 R)
         --                                          )
         --                                 ) triangs
         --   goldenWith [osp|data/test-with-ipe/Polygon/Triangulation/|]
         --         (ipeFileGolden { name = [osp|myPolyPrime|] })
         --         (addStyleSheet opacitiesStyle $ singlePageFromContent out)

         --   it "myPolyPrime is CCW" $
         --     isCounterClockwise myPoly' `shouldBe` True




isTriangle'    :: SimplePolygon_ simplePolygon point r => simplePolygon  -> Bool
isTriangle' pg = numVertices pg == 3

isCounterClockwise :: (Num r, Eq r, SimplePolygon_ simplePolygon point r)
                   => simplePolygon -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea2X






myPoly :: SimplePolygon (Point 2 R)
myPoly = fromJust . fromPoints $
  read  @[Point 2 R] "[Point2 0 0,Point2 26 37.1,Point2 7.1 45.2,Point2 (-6.6) 39,Point2 (-1.9) 15.1,Point2 (-1.4) 12.7,Point2 0 0]"



-- myPoly :: SimplePolygon (Point 2 R)
-- myPoly = read "SimplePolygon [Point2 0 0,Point2 26 37.1,Point2 7.1 45.2,Point2 (-6.6) 39,Point2 (-2.5) 17.6,Point2 (-1.9) 15.1,Point2 (-1.4) 12.7,Point2 0.6 4.1,Point2 0.5 2.6,Point2 0 0]"



  -- read "SimplePolygon [Point2 0 0,Point2 2.6 3.71,Point2 0.71 4.52,Point2 (-0.66) 3.9,Point2 (-0.25) 1.76,Point2 (-0.19) 1.51,Point2 (-0.14) 1.27,Point2 0.06 0.41,Point2 0.05 0.26,Point2 0 0]"


  -- read "SimplePolygon [Point2 4207.72 2000.33,Point2 4210.32 2004.04,Point2 4208.43 2004.85,Point2 4207.06 2004.23,Point2 4207.47 2002.09,Point2 4207.53 2001.84,Point2 4207.58 2001.6,Point2 4207.78 2000.74,Point2 4207.77 2000.59,Point2 4207.72 2000.33]"



  -- read "SimplePolygon [Point2 4207.72 2000.33,Point2 4215.47 2003.24,Point2 4212.47 2003.94,Point2 4210.82 2003.98,Point2 4210.32 2004.04,Point2 4208.43 2004.85,Point2 4208.23 2007.62,Point2 4207.85 2008.3,Point2 4207.67 2008.39,Point2 4206.84 2007.54,Point2 4206.78 2007.46,Point2 4206.68 2007.25,Point2 4206.66 2006.64,Point2 4207.06 2004.23,Point2 4207.47 2002.09,Point2 4207.53 2001.84,Point2 4207.58 2001.6,Point2 4207.78 2000.74,Point2 4207.77 2000.59,Point2 4207.72 2000.33]"


-- (10,

  -- myPoly'
-- myPoly = uncheckedFromCCWPoints . NonEmpty.reverse . toNonEmptyOf outerBoundary $ myPoly'


myPoly' :: SimplePolygon (Point 2 R)
myPoly' = fromJust . fromPoints $
  read @[Point 2 R] "[Point2 4207.72 2000.33,Point2 4207.67 2000.23,Point2 4207.48 1999.96,Point2 4207.43 1999.86,Point2 4207.39 1999.58,Point2 4207.45 1999.34,Point2 4207.53 1999.12,Point2 4207.59 1998.87,Point2 4207.63 1998.47,Point2 4207.58 1997.89,Point2 4207.48 1997.52,Point2 4207.26 1997.13,Point2 4207.16 1996.77,Point2 4207.02 1996.44,Point2 4206.95 1996.05,Point2 4206.86 1995.84,Point2 4206.79 1995.75,Point2 4206.76 1995.63,Point2 4206.72 1995.2,Point2 4206.74 1994.78,Point2 4206.73 1994.3,Point2 4207.5 1991.99,Point2 4207.59 1991.78,Point2 4207.73 1991.56,Point2 4209.06 1989.82,Point2 4209.27 1989.68,Point2 4209.52 1989.62,Point2 4209.93 1989.64,Point2 4210.14 1989.61,Point2 4211.35 1989.1,Point2 4211.57 1988.87,Point2 4212.6 1987.49,Point2 4212.61 1987.48,Point2 4212.8 1987.69,Point2 4212.8 1987.91,Point2 4212.67 1989.29,Point2 4212.55 1990.17,Point2 4212.32 1990.84,Point2 4212.3 1991.04,Point2 4212.51 1991.16,Point2 4214.5 1991.95,Point2 4214.95 1992.09,Point2 4216.27 1992.32,Point2 4217.24 1992.39,Point2 4219.58 1993.53,Point2 4219.78 1993.71,Point2 4220.01 1993.69,Point2 4221.09 1993.39,Point2 4221.51 1993.18,Point2 4221.71 1992.97,Point2 4222.05 1991.87,Point2 4222.02 1991.21,Point2 4222.07 1991,Point2 4222.28 1990.93,Point2 4222.51 1991.03,Point2 4224.12 1991.73,Point2 4224.86 1992.09,Point2 4225.33 1992.36,Point2 4227.12 1993.51,Point2 4233.86 1998.69,Point2 4234.41 1999.25,Point2 4234.48 1999.34,Point2 4234.47 1999.48,Point2 4234.4 2000.02,Point2 4236.24 2003.5,Point2 4236.37 2003.68,Point2 4237.01 2004.41,Point2 4237.63 2005.03,Point2 4239.47 2005.53,Point2 4239.59 2005.56,Point2 4239.73 2005.55,Point2 4239.99 2005.47,Point2 4240.09 2005.48,Point2 4240.2 2005.52,Point2 4240.63 2006,Point2 4240.7 2006.09,Point2 4240.85 2006.56,Point2 4240.85 2006.83,Point2 4240.82 2006.95,Point2 4240.89 2008.06,Point2 4241.16 2009.17,Point2 4241.43 2010.14,Point2 4241.64 2010.85,Point2 4242.11 2011.92,Point2 4242.76 2013.41,Point2 4242.81 2013.51,Point2 4242.87 2013.6,Point2 4243.03 2013.75,Point2 4243.21 2013.87,Point2 4243.29 2013.94,Point2 4243.35 2014.04,Point2 4243.43 2014.26,Point2 4243.66 2015.1,Point2 4243.72 2015.34,Point2 4243.77 2015.6,Point2 4243.8 2016.04,Point2 4243.85 2017.13,Point2 4243.54 2018.48,Point2 4243.5 2018.58,Point2 4243.43 2018.63,Point2 4242.74 2018.44,Point2 4237.4 2016.55,Point2 4234.92 2015.67,Point2 4231.49 2014.49,Point2 4231.21 2014.41,Point2 4231.14 2014.42,Point2 4231.07 2014.54,Point2 4230.17 2016.44,Point2 4230.16 2016.66,Point2 4230.21 2016.89,Point2 4230.55 2018.02,Point2 4230.97 2018.44,Point2 4231.18 2018.67,Point2 4231.31 2018.89,Point2 4231.36 2019.12,Point2 4231.37 2019.55,Point2 4231.39 2022.6,Point2 4222.8 2022.6,Point2 4222.19 2022.59,Point2 4221.99 2022.58,Point2 4221.62 2021.34,Point2 4221.26 2021.08,Point2 4220.92 2020.9,Point2 4219.77 2021.47,Point2 4219.41 2021.82,Point2 4219.21 2022.2,Point2 4218.59 2022.58,Point2 4218.3 2022.59,Point2 4217.81 2022.6,Point2 4217.14 2023.42,Point2 4216.98 2023.56,Point2 4216.82 2023.69,Point2 4216.61 2023.79,Point2 4216.33 2023.83,Point2 4215.08 2023.82,Point2 4214.86 2023.73,Point2 4213.34 2022.99,Point2 4212.79 2022.64,Point2 4212.71 2022.57,Point2 4212.64 2022.48,Point2 4212.5 2022.16,Point2 4212.39 2021.5,Point2 4213.13 2019.29,Point2 4213.17 2019.19,Point2 4213.3 2019.02,Point2 4213.45 2018.86,Point2 4213.64 2018.6,Point2 4213.68 2018.49,Point2 4213.83 2017.81,Point2 4213.84 2017.67,Point2 4214.01 2013.26,Point2 4213.97 2012.6,Point2 4213.94 2012.49,Point2 4213.28 2011.01,Point2 4212.88 2011.38,Point2 4212.94 2011.91,Point2 4213.01 2012.83,Point2 4213 2013.24,Point2 4212.95 2013.48,Point2 4212.9 2013.59,Point2 4212.35 2014.72,Point2 4212.1 2015.21,Point2 4211.9 2015.32,Point2 4211.48 2015.36,Point2 4211.35 2015.33,Point2 4207.06 2014.21,Point2 4206.97 2014.15,Point2 4206.89 2014.08,Point2 4206.73 2013.31,Point2 4206.73 2013.15,Point2 4206.69 2012.2,Point2 4206.7 2012.06,Point2 4206.86 2011.92,Point2 4206.98 2011.89,Point2 4207.09 2011.9,Point2 4207.2 2011.94,Point2 4207.38 2012.06,Point2 4207.49 2012.1,Point2 4207.63 2012.12,Point2 4207.76 2012.1,Point2 4207.87 2012.05,Point2 4207.95 2011.99,Point2 4208.9 2011.11,Point2 4209.08 2010.83,Point2 4209.26 2010.4,Point2 4209.76 2009.25,Point2 4210.18 2008.31,Point2 4210.93 2006.96,Point2 4210.98 2006.87,Point2 4211.06 2006.79,Point2 4212.17 2006.96,Point2 4213.08 2006.44,Point2 4215.07 2005.21,Point2 4216.83 2004.5,Point2 4217.19 2004.4,Point2 4217.46 2004.36,Point2 4218.39 2004.29,Point2 4219.01 2004.41,Point2 4220.41 2004.44,Point2 4217.49 2003.25,Point2 4216.74 2002.95,Point2 4216.33 2002.87,Point2 4215.82 2003.13,Point2 4215.47 2003.24,Point2 4212.47 2003.94,Point2 4210.82 2003.98,Point2 4210.32 2004.04,Point2 4208.43 2004.85,Point2 4208.23 2007.62,Point2 4207.85 2008.3,Point2 4207.67 2008.39,Point2 4206.84 2007.54,Point2 4206.78 2007.46,Point2 4206.68 2007.25,Point2 4206.66 2006.64,Point2 4207.06 2004.23,Point2 4207.47 2002.09,Point2 4207.53 2001.84,Point2 4207.58 2001.6,Point2 4207.78 2000.74,Point2 4207.77 2000.59,Point2 4207.72 2000.33]"



  -- read "SimplePolygon [Point2 2996.84 650.6,Point2 2996.71 650.69,Point2 2996.3 650.94,Point2 2995.06 651.63,Point2 2991.84 652.53,Point2 2991.3 652.59,Point2 2990.06 652.67,Point2 2985.5 653.09,Point2 2984.43 653.31,Point2 2983.78 653.59,Point2 2982.88 654.08,Point2 2979.43 653.59,Point2 2977.97 653.64,Point2 2977.74 653.69,Point2 2977.61 653.77,Point2 2977.51 653.87,Point2 2977.65 654.42,Point2 2978.55 655.83,Point2 2978.66 655.93,Point2 2979.34 656.03,Point2 2979.57 656.06,Point2 2983.78 656.29,Point2 2983.98 656.23,Point2 2984.82 655.87,Point2 2985.03 655.81,Point2 2985.27 655.76,Point2 2987.46 655.35,Point2 2988.01 655.29,Point2 2988.3 655.28,Point2 2988.59 655.45,Point2 2988.76 655.51,Point2 2989.01 655.53,Point2 2989.32 655.51,Point2 2990.63 655.27,Point2 2990.84 655.22,Point2 2991.41 654.61,Point2 2991.54 654.39,Point2 2994.6 653.65,Point2 2995.12 653.5,Point2 2996.47 652.37,Point2 2996.53 652.26,Point2 2997.02 650.42]"



  -- read "SimplePolygon [Point2 669.413 292.72,Point2 671.295 292.83,Point2 673.202 292.86,Point2 675.121 292.82,Point2 675.769 292.77,Point2 676.429 292.7,Point2 677.09 292.59,Point2 678.244 292.33,Point2 679.089 292.05,Point2 682.884 290.37,Point2 687.742 288.11,Point2 688.087 287.82,Point2 688.13 287.68,Point2 688.106 287.55,Point2 688.019 287.4,Point2 687.544 287.25,Point2 686.927 287.16,Point2 685.729 287.03,Point2 684.125 286.97,Point2 682.829 286.95,Point2 681.527 286.96,Point2 679.57 286.99,Point2 677.59 287.11,Point2 663.89 287.98,Point2 662.501 288.13,Point2 651.985 290.45,Point2 651.51 290.61,Point2 651.294 290.74,Point2 651.14 290.88,Point2 651.176 291.04,Point2 651.275 291.18,Point2 651.51 291.33,Point2 651.8 291.46,Point2 652.417 291.61,Point2 653.096 291.72,Point2 653.596 291.78,Point2 655.552 291.9]"


  -- -- "SimplePolygon [Point2 1930.07 388.08,Point2 1929.98 387.91,Point2 1929.77 387.62,Point2 1929.58 387.51,Point2 1928.57 386.97,Point2 1927.95 386.76,Point2 1927.24 386.57,Point2 1926.75 386.47,Point2 1925.73 386.33,Point2 1925.06 386.28,Point2 1923.93 386.3,Point2 1923.53 386.35,Point2 1923.28 386.51,Point2 1923.3 386.67,Point2 1923.6 387.88,Point2 1923.89 387.99,Point2 1925.99 388.28,Point2 1926.58 388.33,Point2 1928.52 388.46,Point2 1929.04 388.43,Point2 1929.53 388.37]"
