import Control.DeepSeq
import Control.Exception
import System.IO.Unsafe
import Data.Char
import Data.Either
import Data.Function
import Data.List hiding (insert)

data Iterator = It [Char] [Char]
  deriving (Eq, Show)

  
fromList :: [Char] -> Iterator
fromList ls = It [] ls
test_fromList = [
   fromList ['a', 'b', 'c', 'd']                == It [] ['a', 'b', 'c', 'd']
 , fromList ['h', 'e', 'l', 'l', 'o']           == It [] ['h', 'e', 'l', 'l', 'o']
 , fromList ['o', 'n', 'e', ' ', 't', 'w', 'o'] == It [] ['o', 'n', 'e', ' ', 't', 'w', 'o']
 ]
 
right :: Iterator -> Iterator
right (It ls []) = It ls []
right (It ls (x:xs)) = It (x:ls) xs
test_right = [
   right (It [] ['a', 'b', 'c', 'd'])                                 == (It ['a'] ['b', 'c', 'd'])
 , right (right (It [] ['a', 'b', 'c', 'd']))                         == (It ['b', 'a'] ['c', 'd'])
 , right (right (right (It [] ['a', 'b', 'c', 'd'])))                 == (It ['c', 'b', 'a'] ['d'])
 , right (right (right (right (It [] ['a', 'b', 'c', 'd']))))         == (It ['d', 'c', 'b', 'a'] [])  -- reached the end of the list
 , right (right (right (right (right (It [] ['a', 'b', 'c', 'd']))))) == (It ['d', 'c', 'b', 'a'] [])
 , right (It [] [])    == It [] []
 , right (It ['a'] []) == It ['a'] []
 ]
 
 
 
left :: Iterator -> Iterator
left (It [] ls) = It [] ls
left (It (x:xs) ls) = It xs (x:ls)

test_left = [
   left (It ['d', 'c', 'b', 'a'] [])                             == It ['c', 'b', 'a'] ['d']
 , left (left (It ['d', 'c', 'b', 'a'] []))                      == It ['b', 'a'] ['c', 'd']
 , left (left (left (It ['d', 'c', 'b', 'a'] [])))               == It ['a'] ['b', 'c', 'd']
 , left (left (left (left (It ['d', 'c', 'b', 'a'] []))))        == It [] ['a', 'b', 'c', 'd'] -- reached the front of the list
 , left (left (left (left (left (It ['d', 'c', 'b', 'a'] []))))) == It [] ['a', 'b', 'c', 'd']
 , left (It [] [])    == It [] []
 , left (It ['a'] []) == It [] ['a']
 ]
 
 
 
toList :: Iterator -> [Char]
toList (It [] [])=[]
toList (It ls [])=reverse ls
toList (It [] ls)= ls
toList (It lls rls)= reverse lls ++ rls

test_toList = [
   toList (It [] ['h', 'e', 'l', 'l', 'o']) == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['h'] ['e', 'l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['e', 'h'] ['l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['o', 'l', 'l', 'e', 'h'] []) == ['h', 'e', 'l', 'l', 'o']
 , toList (It [] []) == []
 ]
 
 
get :: Iterator -> Maybe Char
get (It _ [] )= Nothing
get (It _ (x:xs))=Just x

test_get = [
   get (It [] ['h', 'e', 'l', 'l', 'o']) == Just 'h'
 , get (It ['h'] ['e', 'l', 'l', 'o'])   == Just 'e'
 , get (It ['e', 'h'] ['l', 'l', 'o'])   == Just 'l'
 , get (It ['o', 'l', 'l', 'e', 'h'] []) == Nothing
 , get (It [] [])                        == Nothing
 ]
 
 
insert :: Char -> Iterator -> Iterator
insert c (It xs ls)= It xs (c:ls)

test_insert = [
   insert 'c' (It [] ['a', 'r']) == It [] ['c', 'a', 'r']
 , insert 'e' (It ['n', 'o'] []) == It ['n', 'o'] ['e']
 , toList (insert 'e' (right (right (fromList ['o', 'n']))))          == ['o', 'n', 'e']
 , insert 'e' (It ['c', 'x', 'e'] ['l', 'l', 'e', 'n', 't'])          == (It ['c','x','e'] ['e','l', 'l', 'e', 'n', 't'])
 , toList (insert 'e' (It ['c', 'x', 'e'] ['l', 'l', 'e', 'n', 't'])) == ['e','x','c','e','l', 'l', 'e', 'n', 't']
 ]
 
 
 
skip :: Int -> Iterator -> Iterator
skip 0 (It xs ls)=It xs ls
skip i (It [] [])=It [] []
skip i (It [] ls)
    |i>0 = It (reverse (take i ls)) (drop i ls)
	|i<0 = It [] ls
skip i (It xs [])
    |i<0 =It (drop (i*(-1)) xs) (take (i*(-1)) xs)
	|i>0 = It xs []
skip i (It xss@(x:xs) lss@(l:ls))
    |i>0 = It (reverse (take i lss)++xss) (drop i lss) 
    |i<0 = It (drop (i*(-1)) xss) (reverse(take (i*(-1)) xss)++lss)

test_skip = [
   skip 0 (It [] ['h', 'e', 'l', 'l', 'o'])     == It [] ['h', 'e', 'l', 'l', 'o']
 , skip 1 (It [] ['h', 'e', 'l', 'l', 'o'])     == It ['h'] ['e', 'l', 'l', 'o']
 , skip 2 (It [] ['h', 'e', 'l', 'l', 'o'])     == It ['e', 'h'] ['l', 'l', 'o']
 , skip 3 (It [] ['h', 'e', 'l', 'l', 'o'])     == It ['l', 'e', 'h'] ['l', 'o']
 , skip 20 (It [] ['h', 'e', 'l', 'l', 'o'])    == It ['o', 'l', 'l', 'e', 'h'] []
 , skip (- 1) (It [] ['h', 'e', 'l', 'l', 'o']) == It [] ['h', 'e', 'l', 'l', 'o']
 , skip (- 2) (It [] ['h', 'e', 'l', 'l', 'o']) == It [] ['h', 'e', 'l', 'l', 'o']
 , skip (- 1) (It ['l', 'e', 'h'] ['l', 'o'])   == It ['e', 'h'] ['l', 'l', 'o']
 , skip (- 2) (It ['l', 'e', 'h'] ['l', 'o'])   == It ['h'] ['e', 'l', 'l', 'o']
 , skip (- 3) (It ['l', 'e', 'h'] ['l', 'o'])   == It [] ['h', 'e', 'l', 'l', 'o']
 , skip (- 4) (It ['l', 'e', 'h'] ['l', 'o'])   == It [] ['h', 'e', 'l', 'l', 'o']
 , skip (- 20) (It ['l', 'e', 'h'] ['l', 'o'])  == It [] ['h', 'e', 'l', 'l', 'o']
 , skip 0 (It [] [])                            == It [] []
 , skip 2 (It [] [])                            == It [] []
 ]
 
isPrefix str (It _ ls)= str == take (length str) ls

test_isPrefix = [
   isPrefix "he"  (It [] ['h', 'e', 'l', 'l', 'o'])   == True
 , isPrefix "hel" (It [] ['h', 'e', 'l', 'l', 'o'])   == True
 , isPrefix "el"  (It [] ['h', 'e', 'l', 'l', 'o'])   == False
 , isPrefix "ehe" (It [] ['h', 'e', 'l', 'l', 'o'])   == False
 , isPrefix "lo"  (It ['l', 'e', 'h'] ['l', 'o'])     == True
 , isPrefix "he"  (It ['l', 'e', 'h'] ['l', 'o'])     == False
 , isPrefix "o"  (It ['o', 'l', 'l', 'e', 'h'] [])    == False
 , isPrefix "hello" (It [] [])                        == False
 , isPrefix ""      (It [] [])                        == True
 ]
 
match :: String -> [Char] -> Bool
match str [] = False
match str lss@(l:ls) 
   | isPrefix str (It [] lss) = True
   | otherwise = match str ls
test_match = [
   match "h" ['h', 'e', 'l', 'l', 'o']     == True
 , match "he" ['h', 'e', 'l', 'l', 'o']    == True
 , match "hello" ['h', 'e', 'l', 'l', 'o'] == True
 , match "" ['h', 'e', 'l', 'l', 'o']      == True
 , match "lo" ['h', 'e', 'l', 'l', 'o']    == True
 , match "ell" ['h', 'e', 'l', 'l', 'o']   == True
 , match "ela" ['h', 'e', 'l', 'l', 'o']   == False
 , match "hal" ['h', 'e', 'l', 'l', 'o']   == False
 , match "loop" ['h', 'e', 'l', 'l', 'o']  == False
 , match "h" []                            == False
 , match "h" ['p']                         == False
 ]
 
 
elemDistance :: Char -> Iterator -> Maybe Int
elemDistance c (It ls xs) 
       |c `elem` xs = Just (elemIndex c xs)
	   |c `elem` ls = Just ( (if (elemIndex c ls)==0 then (1) else (elemIndex c ls)+1) * (-1))
	   |otherwise = Nothing
	      where 
		  elemIndex c xs = (length(takeWhile (/=c) xs))

test_elemDistance = [
   elemDistance 'e' (It ['h'] ['e', 'l', 'l', 'o']) == Just 0
 , elemDistance 'l' (It ['h'] ['e', 'l', 'l', 'o']) == Just 1
 , elemDistance 'o' (It ['h'] ['e', 'l', 'l', 'o']) == Just 3
 , elemDistance 'h' (It ['h'] ['e', 'l', 'l', 'o']) == Just (- 1)
 , elemDistance 'X' (It ['h'] ['e', 'l', 'l', 'o']) == Nothing
 , elemDistance 'a' (It ['a','h'] ['h', 'a'])       == Just 1
 , elemDistance 'a' (It [] [])                      == Nothing
 , elemDistance 'o' (It ['e', 'n', 'o'] [])         == Just (- 3)
 ]

abs1 x
  |x>0=x
  |x<0=x*(-1)
  |x==0 = 0

rm :: Maybe Int -> Int
rm (Just x)=x


closer :: Char -> Char -> Iterator -> Char
closer c1 c2 (It xs ls)
    |abs1(rm(elemDistance c1 (It xs ls))) < abs1(rm(elemDistance c2 (It xs ls)))=c1
	|abs1(rm(elemDistance c2 (It xs ls))) < abs1(rm(elemDistance c1 (It xs ls)))=c2
	|abs1(rm(elemDistance c2 (It xs ls))) == abs1(rm(elemDistance c1 (It xs ls)))=c2
test_closer = [
   closer 'e' 'h' (It ['h'] ['e', 'l', 'l', 'o'])   == 'e'
 , closer 'h' 'o' (It ['h'] ['e', 'l', 'l', 'o'])   == 'h'
 , closer 'h' 'l' (It ['h'] ['e', 'l', 'l', 'o'])   == 'l'
 , closer 'a' 'a' (It ['a','h'] ['h', 'a'])         == 'a'
 ]
 
rm' :: Maybe a -> [a]
rm' (Just a) = [a]
rm' (Nothing) = []

modify :: (Char -> Maybe Char) -> Iterator -> Iterator
modify f (It [] []) = It [] []
modify f (It xs []) = It xs []
modify f (It [] (l:ls)) = It [] (rm'(f l)++ls)
modify f (It xs (l:ls)) = It xs (rm'(f l)++ls)

test_modify = [
   modify (\c -> Just (toLower c)) (It [] ['A'])                            == (It [] ['a'])
 , modify (\c -> Just (toUpper c)) (It ['e','h'] ['l','l','o'])             == It ['e','h'] ['L','l','o']
 , modify (\c -> Just (toUpper c)) (It ['l','e', 'k', 's', 'a', 'h'] ['l']) == It ['l','e', 'k', 's', 'a', 'h'] ['L']
 , modify (\c -> Nothing) (It ['e','h'] ['l','l','o'])                      == It ['e','h'] ['l','o']
 , modify (\c -> Nothing) (It ['l','e', 'k', 's', 'a', 'h'] ['l'])          == It ['l','e', 'k', 's', 'a', 'h'] []
 , toList (modify (\c -> Nothing) (It ['e','h'] ['l','l','o']))             == ['h','e','l','o']
 , toList (modify (\c -> Nothing) (It ['l','e', 'k', 's', 'a', 'h'] ['l'])) == ['h', 'a', 's', 'k', 'e', 'l']
 , modify (\c -> Just (toUpper c)) (It ['o', 'n', 'e'] [])                  == It ['o', 'n', 'e'] []
 , modify (\c -> Nothing) (It ['o', 'n', 'e'] [])                           == It ['o', 'n', 'e'] []
 , modify (\c -> Just (toUpper c)) (It [] [])                               == It [] []
 ]

replace :: Char -> Iterator -> Iterator
replace _ (It x [] ) = It x []
replace x (It x1 x2) = modify (\c -> Just x) (It x1 x2)

test_replace = [
   replace 'Z' (It [] ['a']) == It [] ['Z']
 , replace 'Z' (It ['a'] []) == It ['a'] []
 , replace 'd' (It [] ['m', 'a', 'd'])                     == It [] ['d','a', 'd']
 , toList (replace 't' (right (fromList ['a', 'p', 'e']))) == ['a', 't', 'e']
 ]