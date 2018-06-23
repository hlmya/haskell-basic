import Data.List hiding (insert)
import Data.Char

data Iterator = It [Char] [Char]
  deriving (Eq, Show)

fromList :: [Char] -> Iterator
fromList xs = It [] xs

test_fromList = [
        fromList ['a', 'b', 'c', 'd']                == It [] ['a', 'b', 'c', 'd']
    , fromList ['h', 'e', 'l', 'l', 'o']           == It [] ['h', 'e', 'l', 'l', 'o']
    , fromList ['o', 'n', 'e', ' ', 't', 'w', 'o'] == It [] ['o', 'n', 'e', ' ', 't', 'w', 'o']
    ]

right :: Iterator -> Iterator
-- right (It [] []) = It [] []
-- right (It xs []) = It xs []
-- right (It xs (y:ys)) = It (y:xs) ys
right (It xs (y:ys)) = It (y:xs) ys
right it             = it

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
left (It (x:xs) ys) = It xs (x:ys)
left it             = it

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
-- toList (It [] r) = r
-- toList i = toList (left i)

toList (It l r) = reverse l ++ r

test_toList = [
    toList (It [] ['h', 'e', 'l', 'l', 'o']) == ['h', 'e', 'l', 'l', 'o']
    , toList (It ['h'] ['e', 'l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
    , toList (It ['e', 'h'] ['l', 'l', 'o'])   == ['h', 'e', 'l', 'l', 'o']
    , toList (It ['o', 'l', 'l', 'e', 'h'] []) == ['h', 'e', 'l', 'l', 'o']
    , toList (It [] []) == []
    ]

get :: Iterator -> Maybe Char
get (It _ (y:ys)) = Just y
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
skip n it
    | n == 0 = it
    | n > 0 = skip (n-1) (right it)
    | otherwise = skip (n+1) (left it)


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
suchThat f (It _ ys)
    | any f ys = Just index
    | otherwise = Nothing
    where
        newlist = zip ys [0..]
        index = head [i | (x, i) <- newlist, f x]

test_suchThat = [
    suchThat (== 'o') (It ['l', 'e', 'h'] ['l', 'o']) == Just 1
    , suchThat isUpper (It ['l', 'e', 'h'] ['l', 'o', 'X', 'D']) == Just 2
    , suchThat isUpper (It ['l', 'e', 'h'] ['l', 'o']) == Nothing
    ]

isPrefix :: String -> Iterator -> Bool
isPrefix "" (It _ []) = True
isPrefix (c:s) (It _ []) = False
-- isPrefix s (It _ r) = all (\(a,b) -> a == b) (zip s r)
isPrefix s (It _ r)
    | length s <= length r = all (\(a,b) -> a == b) (zip s r)
    | otherwise = False
-- any (== False) [a == b|(a,b) <- zip s ys] = False

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
-- match s [] = False
-- match s l 
--     | isPrefix s it = True
--     | otherwise = match s (tail l)
--     where 
--         it = fromList l
match [] _ = True
match s [] = False
match s (l:ls) 
    | isPrefix s it = True
    | otherwise = match s (toList (right (fromList ls)))
    where 
        it = fromList (l:ls)

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

rightDistance :: Char -> Iterator -> Maybe Int
rightDistance c (It _ r)
    | elem c r = Just index
    | otherwise = Nothing
    where
        newlist = zip [0..] r
        index = head [ i |(i,x) <- newlist, c == x]

leftDistance :: Char -> Iterator -> Maybe Int
leftDistance c (It l _ )
    | elem c l = Just index
    | otherwise = Nothing
    where
        newlist = zip [(-1),(-2)..] l
        index = head [ i |(i,x) <- newlist, c == x]

-- elemDistance :: Char -> Iterator -> Maybe Int
-- elemDistance c (It [] []) = Nothing
-- elemDistance c (It xs ys)
--     | elem c ys = rightDistance c (It xs ys)
--     | (elem c xs) || null ys = leftDistance c (It xs ys)
--     | otherwise = Nothing

elemDistance :: Char -> Iterator -> Maybe Int
elemDistance c (It l r)
    | null indexlist = Nothing
    | otherwise = Just (head indexlist)
    where
        newlist = zip [0..] r ++ zip [-1,-2..] l
        indexlist = [i|(i,x) <- newlist, x == c]

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

level :: Maybe a -> a
level (Just x) = x

closer :: Char -> Char -> Iterator -> Char
closer c1 c2 it 
    | abs (level1) > abs (level2) = c2
    | abs (level1) < abs (level2) = c1
    | otherwise = if level1 > 0 then c1 else c2
    where
        level1 = level (elemDistance c1 it)
        level2 = level (elemDistance c2 it)
    
test_closer = [
    closer 'e' 'h' (It ['h'] ['e', 'l', 'l', 'o'])   == 'e'
    , closer 'h' 'o' (It ['h'] ['e', 'l', 'l', 'o'])   == 'h'
    , closer 'h' 'l' (It ['h'] ['e', 'l', 'l', 'o'])   == 'l'
    , closer 'a' 'a' (It ['a','h'] ['h', 'a'])         == 'a'
    ]

modify :: (Char -> Maybe Char) -> Iterator -> Iterator
modify f (It l (r:rs))
    | f r == Nothing = It l rs
    | otherwise = It l (x:rs)
    where 
        Just x = f r
modify _ it = it

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
replace c it = modify (\x -> Just c) it

test_replace = [
    replace 'Z' (It [] ['a']) == It [] ['Z']
    , replace 'Z' (It ['a'] []) == It ['a'] []
    , replace 'd' (It [] ['m', 'a', 'd'])                     == It [] ['d','a', 'd']
    , toList (replace 't' (right (fromList ['a', 'p', 'e']))) == ['a', 't', 'e']
    ]