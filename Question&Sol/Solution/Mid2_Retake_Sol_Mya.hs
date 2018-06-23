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

type Intersection = Int
type Street       = (Intersection, Intersection)
type Map          = [Street]

map1 :: Map
map1 = [(1,2),(2,1),(1,3),(3,4),(3,5),(4,6),(6,4),(5,6),(2,3),(3,2),(2,4),(4,2)]

map2 :: Map
map2 = [(2,3),(3,4),(5,1),(1,6),(8,7),(7,8),(9,8),(8,9),(5,2),(2,5),(7,5),(5,7),(4,6),(6,9)]


intersections :: Map -> [Intersection]
-- intersections m = sort (nub ( concat [ x : y : []| (x,y) <- m]) )
intersections m = sort (nub (map fst m ++ map snd m))

test_intersections = [
    intersections [] == []
    , intersections [(2,3), (3,2)] == [2,3]
    , intersections [(2,3), (1,2), (3,1)] == [1,2,3]
    , intersections map1 == [1,2,3,4,5,6]
    , intersections map2 == [1,2,3,4,5,6,7,8,9]
    ]

outNeighbors :: Map -> Intersection -> [Intersection]
outNeighbors m i = [ snd x | x <- m, fst x == i]

test_outNeighbors = [
    sort (outNeighbors map1 1) == [2,3]
    , sort (outNeighbors map1 3) == [2,4,5]
    , sort (outNeighbors map1 5) == [6]
    , sort [outNeighbors map1 i | i <- intersections map1] == [[1,3,4],[2,3],[4],[4,5,2],[6],[6,2]]
    , sort [outNeighbors map2 i | i <- intersections map2] == [[1,2,7],[3,5],[4],[6],[6],[7,9],[8],[8,5],[9]]
    ]

type Route = [Intersection]

step :: Map -> Route -> [Route]
step m r = [ n : r| n <- outNeighbors m (head r), notElem n r ]
        
test_step = [
    sort (step [(1,2)] [2]) == []
    , sort (step map1 [2]) == [[1,2],[3,2],[4,2]]
    , sort (step map1 [3]) == [[2,3],[4,3],[5,3]]
    , sort (step map1 [1, 2]) == [[3,1,2]]
    , sort (step map1 [3, 2]) == [[4,3,2],[5,3,2]]
    , sort (step map1 [4, 2]) == [[6,4,2]]
    ]

steps :: Map -> [Route] -> [Route]
steps m rs = concat [ step m r | r <- rs]

test_steps = [
    sort (steps map1 [[1]]) == [[2,1],[3,1]]
    , sort (steps map1 [[2, 1], [3, 1]]) == [[2,3,1],[3,2,1],[4,2,1],[4,3,1],[5,3,1]]
    , sort (steps map1 [[3, 2, 1], [4, 2, 1], [4, 3, 1], [5, 3, 1], [2, 3, 1]]) == [[2,4,3,1],[4,2,3,1],[4,3,2,1],[5,3,2,1],[6,4,2,1],[6,4,3,1],[6,5,3,1]]
    , sort (steps map1 [[4, 3, 2, 1], [5, 3, 2, 1], [6, 4, 2, 1], [6, 4, 3, 1],  [2, 4, 3, 1], [6, 5, 3, 1], [4, 2, 3, 1]]) == [[4,6,5,3,1],[6,4,2,3,1],[6,4,3,2,1],[6,5,3,2,1]]
    , sort (steps map2 (steps map2 (step map2 [4]))) == [[8,9,6,4]]
    , sort (steps map2 (steps map2 (step map2 [9]))) == [[5,7,8,9]]
    , sort (steps map2 (steps map2 (step map2 [5]))) == [[4,3,2,5],[9,6,1,5],[9,8,7,5]]
    ]

getRoutesWithEnding :: Intersection -> [Route] -> [Route]
getRoutesWithEnding i rs = [ r | r <- rs, head r == i]

test_getRoutesWithEnding = [
    sort (getRoutesWithEnding 6 []) == []
    , sort (getRoutesWithEnding 1 [[1]]) == [[1]]
    , sort (getRoutesWithEnding 1 [[2,1]]) == []
    , sort (getRoutesWithEnding 2 [[3,2,1]]) == []
    , sort (getRoutesWithEnding 6 [[4,3,2,1],[5,3,2,1],[6,4,2,1],[6,4,3,1],[2,4,3,1],[6,5,3,1],[4,2,3,1]]) == [[6,4,2,1],[6,4,3,1],[6,5,3,1]]
    , sort (getRoutesWithEnding 6 [[4,3,2,1],[3,2,1],[4,2,1],[4,3,1],[4,3,1],[5,3,1],[4,2,3,1]]) == []
    ]

hasRouteWithEnding :: Intersection -> [Route] -> Bool
hasRouteWithEnding i rs
    | null (getRoutesWithEnding i rs) = False
    | otherwise = True

test_hasRouteWithEnding = [
    hasRouteWithEnding 3 [] == False
    , hasRouteWithEnding 3 [[1, 2], [3, 2], [4, 2]] == True
    , hasRouteWithEnding 3 [[2, 1], [3, 1]] == True
    , hasRouteWithEnding 2 [[2, 1], [3, 1]] == True
    , hasRouteWithEnding 5 [[2, 1], [3, 1]] == False 
    , hasRouteWithEnding 1 [[2, 1], [3, 1]] == False
    ]

initRoutes :: Map -> Intersection -> [Route]
initRoutes m i
    | elem i [fst x | x <- m] = [[i]]
    | otherwise = error "initRoutes: invalid intersection node"

test_initRoutes = [
    initRoutes map1 1 == [[1]]
    , initRoutes map1 4 == [[4]]
    , initRoutes map2 7 == [[7]]
    , initRoutes map1 7 `raises` "initRoutes: invalid intersection node"
    ]

routes2 :: Map -> [Route] -> Intersection -> [Route]
routes2 m rs i
    | hasRouteWithEnding i rs = getRoutesWithEnding i rs
    | otherwise = routes2 m (steps m rs) i

routes :: Map -> Intersection -> Intersection -> [Route]
routes m i1 i2
    | hasRouteWithEnding i2 initial = initial
    | otherwise = routes2 m initial i2
        where
            initial = initRoutes m i1

test_routes = [
    routes map1 2 2 == [[2]]
    , routes map1 1 5 == [[5,3,1]]
    , routes map1 5 3 == [[3,2,4,6,5]]
    ]

type Pizzeria = Intersection

routesFromPizzerias :: Map -> [Pizzeria] -> Intersection -> [Route]
routesFromPizzerias m ps i = concat [routes m i1 i | i1 <- ps ]

test_routesFromPizzerias = [
    sort (routesFromPizzerias map1 [1] 4) == [[4,2,1],[4,3,1]]
    , sort (routesFromPizzerias map1 [4,5] 6) == [[6,4],[6,5]]
    , sort (routesFromPizzerias map1 [2,5] 4) == [[4,2],[4,6,5]]
    , sort (routesFromPizzerias map1 [2,1,5] 4) == [[4,2],[4,2,1],[4,3,1],[4,6,5]]
    , sort (routesFromPizzerias map1 [1] 6) == [[6,4,2,1],[6,4,3,1],[6,5,3,1]]
    ]

optimalRoute :: Map -> [Pizzeria] -> Intersection -> Route
optimalRoute m ps i = minimumBy (\x y -> compare (length x) (length y)) rs
    where rs = routesFromPizzerias m ps i

test_optimalRoute = [
    optimalRoute map1 [1,6] 3 == [3,1]
    , optimalRoute map1 [1,6] 4 == [4,6]
    , [optimalRoute map1 [1,6] j | j <- intersections map1] == [[1],[2,1],[3,1],[4,6],[5,3,1],[6]]
    , [optimalRoute map1 [1,3] j | j <- intersections map1] == [[1],[2,1],[3],[4,3],[5,3],[6,4,3]]
    , [optimalRoute map2 [1,3] j | j <- intersections map2] == [[1],[2,5,7,8,9,6,1],[3],[4,3],[5,7,8,9,6,1],[6,1],[7,8,9,6,1],[8,9,6,1],[9,6,1]]
    ]