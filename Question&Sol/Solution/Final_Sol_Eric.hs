import Data.List hiding (insert)
import Data.Char
import Data.Maybe

allTests = (and (concatMap snd tests), tests)

tests =
  [ ("test_fromList", test_fromList)
 , ("test_right", test_right)
 , ("test_left", test_left)
   , ("test_toList", test_toList)
  , ("test_get", test_get)
  , ("test_insert", test_insert)
  , ("test_skip", test_skip)
  , ("test_isPrefix", test_isPrefix)
  , ("test_match", test_match)
  , ("test_elemDistance", test_elemDistance)
  , ("test_closer", test_closer)
  , ("test_modify", test_modify)
  , ("test_replace", test_replace)
  ]

data Iterator = It [Char] [Char]
  deriving (Eq, Show)

fromList :: [Char] -> Iterator
fromList l = It [] l

test_fromList = [
   fromList ['a', 'b', 'c', 'd']                == It [] ['a', 'b', 'c', 'd']
 , fromList ['h', 'e', 'l', 'l', 'o']           == It [] ['h', 'e', 'l', 'l', 'o']
 , fromList ['o', 'n', 'e', ' ', 't', 'w', 'o'] == It [] ['o', 'n', 'e', ' ', 't', 'w', 'o']
 ]

right :: Iterator -> Iterator
right (It l []) = It l []
right (It l r) = It ((head r):l) (tail r)

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
left (It [] []) = It [] []
left (It [l] []) = It [] [l]
left (It [] r) = It [] r
left (It l r)= It (tail l) ((head l):r)

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
toList (It l r) = (reverse l) ++ r

test_toList = [
   toList (It [] ['h', 'e', 'l', 'l', 'o']) == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['h'] ['e', 'l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['e', 'h'] ['l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['o', 'l', 'l', 'e', 'h'] []) == ['h', 'e', 'l', 'l', 'o']
 , toList (It [] []) == []
 ]

get :: Iterator -> Maybe Char
get (It l []) = Nothing
get (It l r) = Just (head r)

test_get = [
   get (It [] ['h', 'e', 'l', 'l', 'o']) == Just 'h'
 , get (It ['h'] ['e', 'l', 'l', 'o'])   == Just 'e'
 , get (It ['e', 'h'] ['l', 'l', 'o'])   == Just 'l'
 , get (It ['o', 'l', 'l', 'e', 'h'] []) == Nothing
 , get (It [] [])                        == Nothing
 ]

insert :: Char -> Iterator -> Iterator
insert c (It l r) = It l (c:r)

test_insert = [
   insert 'c' (It [] ['a', 'r']) == It [] ['c', 'a', 'r']
 , insert 'e' (It ['n', 'o'] []) == It ['n', 'o'] ['e']
 , toList (insert 'e' (right (right (fromList ['o', 'n']))))          == ['o', 'n', 'e']
 , insert 'e' (It ['c', 'x', 'e'] ['l', 'l', 'e', 'n', 't'])          == (It ['c','x','e'] ['e','l', 'l', 'e', 'n', 't'])
 , toList (insert 'e' (It ['c', 'x', 'e'] ['l', 'l', 'e', 'n', 't'])) == ['e','x','c','e','l', 'l', 'e', 'n', 't']
 ]

skip :: Int -> Iterator -> Iterator
skip n (It l r) | n == 0 = (It l r)
                | n > 0  = skip (n - 1) (right (It l r))
                | n < 0  = skip (n + 1) (left (It l r))

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

isPrefix :: String -> Iterator -> Bool
isPrefix [] (It [] []) = True
isPrefix (x:xs) (It [] []) = False
isPrefix [] (It l r) = True
isPrefix (x:xs) (It l r) | null r = False
                         | x == (head r)    = isPrefix xs (It l (tail r))
                         | otherwise = False
 
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
match [] _ = True
match s [] = False
match s (c:cs) | (isPrefix s (fromList (c:cs))) = True
               | otherwise                      = match s (toList (right (fromList cs)))

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

numberCharactersIterator (It l r) = (zip [0,1..] r) ++ (zip [-1,-2..] l)

findIndicesChar c l = [fst x | x <- l , ((snd x) == c)]

elemDistance :: Char -> Iterator -> Maybe Int
elemDistance c (It [] []) = Nothing
elemDistance c (It l r) | null searchList = Nothing
                        | otherwise       = (Just (head searchList)) where searchList = findIndicesChar c (numberCharactersIterator (It l r))

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

closer :: Char -> Char -> Iterator -> Char
closer c d it | (abs (distance c)) >  (abs (distance d)) = d
              | (abs (distance c)) < (abs (distance d))  = c
              | otherwise                                = if (distance c) >= 0 then c else d
    where distance x = fromJust (elemDistance x it)

test_closer = [
   closer 'e' 'h' (It ['h'] ['e', 'l', 'l', 'o'])   == 'e'
 , closer 'h' 'o' (It ['h'] ['e', 'l', 'l', 'o'])   == 'h'
 , closer 'h' 'l' (It ['h'] ['e', 'l', 'l', 'o'])   == 'l'
 , closer 'a' 'a' (It ['a','h'] ['h', 'a'])         == 'a'
 ]

modify :: (Char -> Maybe Char) -> Iterator -> Iterator
modify f (It [] []) = It [] []
modify f (It l [])  = It l []
modify f (It l (r:rs)) | isJust (f r)      = It l ((fromJust (f r)):rs)
                       | isNothing (f r)   = It l rs

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
replace c (It l r) = modify (\x -> Just c) (It l r)

test_replace = [
   replace 'Z' (It [] ['a']) == It [] ['Z']
 , replace 'Z' (It ['a'] []) == It ['a'] []
 , replace 'd' (It [] ['m', 'a', 'd'])                     == It [] ['d','a', 'd']
 , toList (replace 't' (right (fromList ['a', 'p', 'e']))) == ['a', 't', 'e']
 ]