import Data.List hiding (insert)
import Data.Char
allTests = (and (concatMap snd tests), tests)

tests =
  [ ("test_fromList", test_fromList)
  , ("test_right", test_right)
  , ("test_left", test_left)
  , ("test_toList", test_toList)
  , ("test_get", test_get)
  , ("test_insert", test_insert)
  , ("test_skip", test_skip)
  , ("test_suchThat", test_suchThat)
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
fromList = It []
test_fromList = [
   fromList ['a', 'b', 'c', 'd']                == It [] ['a', 'b', 'c', 'd']
 , fromList ['h', 'e', 'l', 'l', 'o']           == It [] ['h', 'e', 'l', 'l', 'o']
 , fromList ['o', 'n', 'e', ' ', 't', 'w', 'o'] == It [] ['o', 'n', 'e', ' ', 't', 'w', 'o']
 ]
right :: Iterator -> Iterator
right (It cs (c:cs')) = It (c:cs) cs'
right it              = it
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
left (It (c:cs) cs')  = It cs (c:cs')
left it               = it
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
toList (It l r) = reverse l ++ r
test_toList = [
   toList (It [] ['h', 'e', 'l', 'l', 'o']) == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['h'] ['e', 'l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['e', 'h'] ['l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
 , toList (It ['o', 'l', 'l', 'e', 'h'] []) == ['h', 'e', 'l', 'l', 'o']
 , toList (It [] []) == []
 ]
get :: Iterator -> Maybe Char
get (It _ (c:cs)) = Just c
get _             = Nothing
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
skip 0 it = it
skip n it
  | n > 0     = skip (n - 1) (right it)
  | otherwise = skip (n + 1) (left it)
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
suchThat :: (Char -> Bool) -> Iterator -> Maybe Int
suchThat p (It _ r)
  | null indices = Nothing
  | otherwise    = Just (head indices)

  where
    indices :: [Int]
    indices = [i | (c, i) <- zip r [0..], p c]
test_suchThat = [
   suchThat (== 'o') (It ['l', 'e', 'h'] ['l', 'o'])          == Just 1
 , suchThat isUpper (It ['l', 'e', 'h'] ['l', 'o', 'X'])      == Just 2
 , suchThat isUpper (It ['l', 'e', 'h'] ['l', 'o', 'X', 'D']) == Just 2
 , suchThat isUpper (It ['l', 'e', 'h'] ['l', 'o'])           == Nothing
 ]
isPrefix :: String -> Iterator -> Bool
isPrefix s (It _ r) = all (\(c, c') -> c == c') (zip s r)
-- isPrefix s (It _ r) = s `isPrefixOf` r
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
match s cs = loop (fromList cs)
--match s cs = any (isPrefix s) (takeWhile (\it -> not (isEmpty it)) (iterate right (fromList cs)))
  where
    loop :: Iterator -> Bool
    loop it
      | isEmpty it = False
      | otherwise  = isPrefix s it || loop (right it)

    isEmpty :: Iterator -> Bool
    isEmpty (It _ r) = null r
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
elemDistance c (It l r)
  | null indices = Nothing
  | otherwise    = Just (head indices)
  where
    indices :: [Int]
    indices = [i | (i, c') <- numbered, c == c']
  
    numbered :: [(Int, Char)]
    numbered = zip [0..] r ++ zip [-1,-2..] l
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
closer c c' it = minIndex (elemDistance c it) (elemDistance c' it)
  where
    minIndex :: Maybe Int -> Maybe Int -> Char
    minIndex (Just x) (Just y)
      | abs x < abs y  = c
      | abs x > abs y  = c'
      | x > 0          = c
      | otherwise      = c'
    
test_closer = [
   closer 'e' 'h' (It ['h'] ['e', 'l', 'l', 'o'])   == 'e'
 , closer 'h' 'o' (It ['h'] ['e', 'l', 'l', 'o'])   == 'h'
 , closer 'h' 'l' (It ['h'] ['e', 'l', 'l', 'o'])   == 'l'
 , closer 'a' 'a' (It ['a','h'] ['h', 'a'])         == 'a'
 ]
modify :: (Char -> Maybe Char) -> Iterator -> Iterator
modify f (It l (c:r)) = update (f c)
  where
    update :: Maybe Char -> Iterator
    update (Just c') = It l (c':r)
    update Nothing   = It l r
modify _ it = it
{-
modify f (It l (c:r)) =
  case f c of
    Just c' -> It l (c':r)
    Nothing -> It l r
modify _ it = it
-}
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
replace c = modify (\_ -> Just c)
test_replace = [
   replace 'Z' (It [] ['a']) == It [] ['Z']
 , replace 'Z' (It ['a'] []) == It ['a'] []
 , replace 'd' (It [] ['m', 'a', 'd'])                     == It [] ['d','a', 'd']
 , toList (replace 't' (right (fromList ['a', 'p', 'e']))) == ['a', 't', 'e']
 ]