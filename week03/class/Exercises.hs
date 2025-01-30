module Exercises where

import Test.HUnit

data Weather
  = Sunny
  | Cloudy
  | Windy
  | Rainy
  | Snowy
  deriving (Show, Eq)

data WeatherRequest
  = ByLocation String
  | ByCoordinate Int Int
  deriving (Show, Eq)

data WeatherForecast
  = Forecast Weather
  | Unknown
  deriving (Show, Eq)

{- Exercise:
   Write a function called prediction that
   * for coordinates, predicts snowy weather if
      the x-coordinate is between 13 and 16 inclusive and the y-coordinate is between
      2 and 8 inclusive.
   * for locations, predicts snowy weather if the location is Philadelphia
   * otherwise, is Unknown.
   For example,
    prediction (ByLocation "Philadelphia") should be Forecast Snowy
    prediction (ByCoordinate 15 50) should be Unknown -}

prediction = undefined

{- At this point, run testPrediction (defined at the bottom of the file).
Start a REPL with `stack ghci` and run `runTestTT testPrediction`.
Note: the ` are to distinguish commands, not part of the command. -}

----- dividing line -----

data Coord = Coord Int Int
  deriving (Show, Eq)

c1 :: Coord
c1 = Coord 15 5

c2 :: Coord
c2 = Coord 0 90

{- Exercise:
   Write a function called addCoord that (point-wise) adds two coordinates,
   e.g. addCoord c1 c2 should be Coord 15 95. -}

addCoord = undefined

{- Exercise:
   Rewrite WeatherRequest and prediction to use Coord
   instead of Int Int.

   The logic should otherwise be the same, e.g.
   prediction (ByCoordinate c1) should be Forecast Snowy and
   prediction (ByCoordinate c2) should be Unknown.
   (You should also switch which portion of the test cases at the bottom
    of the file are commented out.)-}

----- dividing line -----

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show, Eq)

tree :: Tree
tree = Node Leaf 1 (Node Leaf 2 Leaf)

{- Exercise:
   Write a function called size that takes a tree and returns how many leaves it has.
   example: size tree should be 3. -}

size = undefined

{- Exercise:
   Write a function called add3Tree that takes a tree and adds 3 to every node value.
   example: add3Tree tree should be Node Leaf 4 (Node Leaf 5 Leaf). -}

add3Tree = undefined

{- Finally, test your functions! Again, start a REPL with `stack ghci`
(or run `:r` to reload if you did not quit your previous REPL) and run `main`. -}

testPrediction :: Test
testPrediction =
  "prediction"
    ~: [ "philly" ~: prediction (ByLocation "Philadelphia") ~?= Forecast Snowy,
         "tallinn" ~: prediction (ByLocation "Tallinn") ~?= Unknown,
         {- Comment these out -}
         "xUB" ~: prediction (ByCoordinate 16 6) ~?= Forecast Snowy,
         "yLB" ~: prediction (ByCoordinate 15 2) ~?= Forecast Snowy,
         "mix1" ~: prediction (ByCoordinate 14 0) ~?= Unknown,
         "mix2" ~: prediction (ByCoordinate 100 7) ~?= Unknown,
         "ex" ~: prediction (ByCoordinate 15 50) ~?= Unknown
         {- Uncomment these -}
         --  "xUB" ~: prediction (ByCoordinate (Coord 16 6)) ~?= Forecast Snowy,
         --  "yLB" ~: prediction (ByCoordinate (Coord 15 2)) ~?= Forecast Snowy,
         --  "mix1" ~: prediction (ByCoordinate (Coord 14 0)) ~?= Unknown,
         --  "mix2" ~: prediction (ByCoordinate (Coord 100 7)) ~?= Unknown,
         --  "ex" ~: prediction (ByCoordinate (Coord 15 50)) ~?= Unknown
       ]

testAddCoord :: Test
testAddCoord =
  "addCoordinates"
    ~: [ "ex" ~: addCoord c1 c2 ~?= Coord 15 95,
         "negative" ~: addCoord (Coord (-5) 0) (Coord 100 (-7)) ~?= Coord 95 (-7)
       ]

testSize :: Test
testSize =
  "size"
    ~: [ "leaf" ~: size Leaf ~?= 1,
         "node" ~: size (Node Leaf 6 Leaf) ~?= 2,
         "ex" ~: size tree ~?= 3
       ]

testAdd3Tree :: Test
testAdd3Tree =
  "add3Tree"
    ~: [ "leaf" ~: add3Tree Leaf ~?= Leaf,
         "ex" ~: add3Tree tree ~?= Node Leaf 4 (Node Leaf 5 Leaf)
       ]

main :: IO Counts
main = runTestTT (TestList [testPrediction, testAddCoord, testSize, testAdd3Tree])