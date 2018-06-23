import Control.DeepSeq
import Control.Exception
import Data.Char
import Data.Either
import Data.Function
import System.IO.Unsafe
import Data.List

	
type Sign = Int	

raises :: NFData a => a -> String -> Bool
x `raises` s = unsafePerformIO $
  either ((s ~=) . f) (const False) `fmap` (try $ evaluate $ force x)
  where
    f :: SomeException -> String
    f = show
	
	
	

(~=) = isPrefixOf `on` (map toUpper . unwords . words)


validSigns :: (Sign -> Sign -> a) -> Sign -> Sign -> a

validSigns f x y | x > 2 || x <0 = error "sign not correct" 
                 | y > 2 || y <0 = error "sign not correct" 
                 | otherwise = f x y
				 

beats :: Sign -> Sign -> Bool				 

beats x y =   validSigns (\x y -> if (x == 0 && y == 2) || (x == 1 && y == 0) || (x == 2 && y == 1) then True else False ) x y 	


isDraw :: Sign -> Sign -> Bool
isDraw x y = validSigns	( \x y -> if x == y then True else False) x y



result :: Sign -> Sign -> Int
result x y | beats x y = 1 
           | beats y x = -1
		   | otherwise = 0 
		   
type Play = [Sign]
type Rounds = (Play, Play)


tournament :: Rounds -> Int
tournament ( [] , [] ) = 0
tournament ( x:xs , y:ys ) | length (x:xs) /= length (y:ys) = error "not equal signs"
                           | otherwise = decideRes ( x:xs , y:ys ) (0,0)

decideRes ( [] , [] ) ( f , z )	| f > z =  1
                                | f < z	= 2
                                | otherwise = 0								
decideRes ( x:xs , y:ys ) ( f , z ) | (result x y) == 1 =  decideRes ( xs , ys ) ( f + 1 , z ) 	   
		                            | (result x y) == -1 =  decideRes ( xs , ys ) ( f  , z + 1 )
									| (result x y) == 0 =  decideRes ( xs , ys ) ( f  , z )
									
									
									
partitionRounds :: Rounds -> ([Sign], [Sign], [Sign])									

partitionRounds ([], []) = ([],[],[])
partitionRounds ( x:xs , y:ys ) | result x y == 1 = ( x : xss , yss , zss)
                                | result x y == -1 = (  xss ,yss ,  x :zss)
								| result x y == 0 = (  xss ,x :yss ,  zss)
                     where ( xss , yss , zss) = partitionRounds( xs , ys )

					 
next :: Sign -> Sign
next x | x == 1 = 2
  	   | x == 2	= 0		 
       | x == 0 = 1 


frequency :: Eq a => a -> [a] -> Int
frequency a [] = 0
frequency a (x:xs) | x == a = 1 + frequency a (xs)
                   | otherwise =  frequency a (xs)
				   
				   
mostFrequent :: Ord a => [a] {- non-empty -} -> a

mostFrequent x = head ( maximumBy (compare `on` length) ( (group . sort)  x ))


type Strategy = Rounds -> Sign -- (Play, Play) -> Sign

strategy1 :: Strategy

strategy1 ([],[])    = 0
strategy1 ( (x:xs) , (y:ys) )  
                | result ( head (x:xs) ) ( head (y:ys) ) == 1 = next (next a)	
                | otherwise = next (head (y:ys))				
            where a = head (x:xs)

			
strategy2 :: Strategy			
strategy2 ([],[])    = 2
strategy2 ( x , y )  =  next(mostFrequent y) 

strategy3 :: Strategy
strategy3 ([], [])          = 1
strategy3 ( x, y)          | length w == length l = next(mostFrequent d)
                           | length w >= length l = mostFrequent w
						   | otherwise = next(mostFrequent l)
            where (w , d , l) =  partitionRounds (x ,y)
			
			
applyStrategies :: Strategy -> Strategy -> Rounds -> Rounds
	
applyStrategies s1 s2 (x,y) =  	 ( (s1 (x,y)): x,	(s2 (y,x)):y )	


play :: Int -> Strategy -> Strategy -> Rounds

play n s1 s2  | n <= 0 =  ([],[])
                | otherwise =  (a,b)
		where (a,b)	= applyStrategies s1 s2 ( play (n-1) s1 s2 )  

