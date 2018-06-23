import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe
import Data.Char
import Data.Either
import Data.Function
import Data.List


data Measure = Land Int | Sea
  deriving (Show, Eq)

type Measures = [Measure]

testMeasures1 :: Measures
testMeasures1 = []

testMeasures2 :: Measures
testMeasures2 =
  [ Land 300, Land 200, Land 400, Sea, Sea, Land 300, Land 200, Land 150, Sea
  , Land 40, Land 80, Land 500, Land 650, Land 890, Land 300, Sea, Sea, Sea
  , Sea, Land 40 ]

testMeasures3 :: Measures
testMeasures3 =
  [ Sea, Sea, Sea, Land 50, Land 100, Land 10, Sea, Land 30, Land 80, Land 350
  , Land 700, Land 980, Land 600, Land 200, Land 40, Sea, Sea, Sea, Sea ]

isLand :: Measure -> Bool
isLand Sea = False
isLand (Land _) = True

test_isLand = 
    [ isLand Sea       == False
    , isLand (Land 40) == True
    , isLand (Land 30) == True
    ]

hasSea :: Measures -> Bool
hasSea ms = any (Sea==) ms

test_hasSea =
    [ hasSea testMeasures1 == False
    , hasSea testMeasures2 == True
    , hasSea testMeasures3 == True
    ]

splitSegments :: Measures -> [Measures]
splitSegments ms = groupBy (\x y -> isLand x == isLand y) ms 
-- = -- plitSegments = groupBy ((==) `on` isLand)
-- splitSegments ms = groupBy (\x y -> (x == Sea) == (y == Sea)) ms
-- splitSegments ms = groupBy (\x y -> (isLand x && isLand y) || (x==Sea && y==Sea)) ms

test_splitSegments =
    [ splitSegments testMeasures1 == ([] :: [Measures])
    , splitSegments testMeasures2 == [ [Land 300,Land 200,Land 400]
                                     , [Sea,Sea]
                                     , [Land 300,Land 200,Land 150]
                                     , [Sea]
                                     , [Land 40,Land 80,Land 500,Land 650,Land 890,Land 300]
                                     , [Sea,Sea,Sea,Sea]
                                     , [Land 40]
                                     ]
    , splitSegments testMeasures3 == [ [Sea,Sea,Sea]
                                     , [Land 50,Land 100,Land 10]
                                     , [Sea]
                                     , [Land 30,Land 80,Land 350,Land 700,Land 980,Land 600,Land 200,Land 40]
                                     , [Sea,Sea,Sea,Sea]
                                     ]
    ]

islandCount :: Measures -> Int
islandCount ms = length [c | c <- splitSegments ms, any (\x -> isLand x) c]
-- islandCount ms = sum [1 | x <- splitSegments ms, isLand (head x)]
test_islandCount =
    [ islandCount testMeasures1 == 0
    , islandCount testMeasures2 == 4
    , islandCount testMeasures3 == 2
    ]

lengthOfIslands :: Measures -> [Int]
lengthOfIslands ms = [ length x| x <- splitSegments ms, isLand (head x)]
-- lengthOfIslands ms = [length c | c <- splitSegments ms, any (\x -> isLand x) c]

test_lengthOfIslands =
    [ lengthOfIslands testMeasures1 == ([] :: [Int])
    , lengthOfIslands testMeasures2 == [3,3,6,1]
    , lengthOfIslands testMeasures3 == [3,8]
    ]

hasIslandsOfSameLength :: Measures -> Bool
hasIslandsOfSameLength ms = any (\x -> length x > 1) (group lengthms)
    where lengthms = lengthOfIslands ms

-- hasIslandsOfSameLength ms = any (>1) [length c|c <- group (lengthOfIslands ms)]

test_hasIslandsOfSameLength =
    [ hasIslandsOfSameLength testMeasures1 == False
    , hasIslandsOfSameLength testMeasures2 == True
    , hasIslandsOfSameLength testMeasures3 == False
    ]

level :: Measure -> Int
level (Land x) = x
level Sea = 0

highestlevel :: Measures -> Int
highestlevel ms = maximum [level x | x <- ms]

highPeaks :: Measures -> [Int]
highPeaks ms = [ highestlevel x | x <- splitSegments ms, isLand (head x)]
        
highestIsland :: Measures -> Maybe Int
highestIsland ms
    | null index = Nothing
    | otherwise = Just (head index)
    where
        indexlist = zip [0..] (highPeaks ms)
        max = maximum (highPeaks ms)
        index = [x | (x,y) <- indexlist, y == max ]

test_highestIsland = 
    [ highestIsland testMeasures1 == (Nothing :: Maybe Int)
    , highestIsland testMeasures2 == Just 2
    , highestIsland testMeasures3 == Just 1
    ]

hypotenuse :: Int -> Int -> Int
hypotenuse a b = round (sqrt (fromIntegral (a^2 + b^2)))

test_hypotenuse =
  [ hypotenuse 500 0   == 500
  , hypotenuse 500 30  == 501
  , hypotenuse 130 350 == 373
  ]

walkingDistance :: Int -> Measure -> Measure -> Int
walkingDistance a m1 m2 = hypotenuse a (abs (level m1 - level m2))

test_walkingDistance =
  [ walkingDistance 500 Sea Sea               == 500
  , walkingDistance 500 Sea (Land 100)        == 510
  , walkingDistance 500 (Land 100) (Land 660) == 751
  ]

fullWalkingDistance :: Int -> Measures -> Int
fullWalkingDistance a [] = 0
fullWalkingDistance a [m] = 0
fullWalkingDistance a (m1:m2:ms) = walkingDistance a m1 m2 + fullWalkingDistance a (m2:ms)

test_fullWalkingDistance =
  [ fullWalkingDistance 500 testMeasures1 == 0
  , fullWalkingDistance 500 testMeasures2 == 10398
  , fullWalkingDistance 500 testMeasures3 == 9561
  ]

