{-# OPTIONS_GHC -fdefer-type-errors #-}
import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe
import Data.Char
import Data.Either
import Data.Function
import Data.List

raises :: NFData a => a -> String -> Bool
x `raises` s = unsafePerformIO $
  either ((s ~=) . f) (const False) `fmap` (try $ evaluate $ force x)
  where
    f :: SomeException -> String
    f = show

    (~=) = isPrefixOf `on` (map toUpper . unwords . words)

allTests = (and (concatMap snd tests), tests)

tests =
  [ ("test_isLand", test_isLand)
  , ("test_hasSea", test_hasSea)
  , ("test_splitSegments", test_splitSegments)
  , ("test_islandCount", test_islandCount)
{-
  , ("test_lengthOfIslands", test_lengthOfIslands)
  , ("test_hasIslandsOfSameLength", test_hasIslandsOfSameLength)
  , ("test_highestIsland", test_highestIsland)
  , ("test_hypotenuse", test_hypotenuse)
  , ("test_walkingDistance", test_walkingDistance)
-}
  , ("test_fullWalkingDistance", test_fullWalkingDistance)
  ]



data Measure
  = Land Int
  | Sea
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
isLand Land{} = True

-- Test cases:

test_isLand = 
  [ isLand Sea       == False
  , isLand (Land 40) == True
  , isLand (Land 30) == True
  ]

hasSea :: Measures -> Bool
--hasSea [] = False
--hasSea (x: xs) = isSea x || hasSea xs
---hasSea xs = or [isSea x | x <- xs]
hasSea = any (not . isLand)

-- any :: (a -> Bool) -> ([a] -> Bool)
-- any :: (a -> Bool) -> [a] -> Bool

---------------     f x = g x    ---->    f = g      -- eta-reduction

-- Test cases:

test_hasSea =
  [ hasSea testMeasures1 == False
  , hasSea testMeasures2 == True
  , hasSea testMeasures3 == True
  , hasSea [Land 11] == False
  ]

splitSegments :: Measures -> [Measures]
splitSegments = groupBy ((==) `on` isLand)

--Test cases:

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
--islandCount ms = sum [1 | (m:_) <- splitSegments ms, isLand m ]
islandCount = length . filter (isLand . head) . splitSegments

--Test cases:

test_islandCount =
  [ islandCount testMeasures1 == 0
  , islandCount testMeasures2 == 4
  , islandCount testMeasures3 == 2
  ]


hypotenuse :: Int -> Int -> Int
hypotenuse a b = round (sqrt (fromIntegral (a^2 + b^2)))


level :: Measure -> Int
level Sea = 0
level (Land m) = m

walkingDistance :: Int -> Measure -> Measure -> Int
walkingDistance a m1 m2 = hypotenuse a (abs (level m1 - level m2))

fullWalkingDistance :: Int -> Measures -> Int
fullWalkingDistance a ms = sum (zipWith (walkingDistance a) ms (drop 1 ms))
{-
fullWalkingDistance a [] = 0
fullWalkingDistance a [m] = 0
fullWalkingDistance a (m1:m2:ms)
    = walkingDistance a m1 m2 + fullWalkingDistance a (m2:ms)
-}
test_fullWalkingDistance =
  [ fullWalkingDistance 500 testMeasures1 == 0
  , fullWalkingDistance 500 testMeasures2 == 10398
  , fullWalkingDistance 500 testMeasures3 == 9561
  ]





