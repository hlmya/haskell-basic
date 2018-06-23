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
  [ ("test_buildDictionary", test_buildDictionary)
  , ("tests_isValidCharacter", tests_isValidCharacter)
  , ("tests_isValidCharSequence", tests_isValidCharSequence)
  , ("test_findCode", test_findCode)
  , ("test_findChar", test_findChar)
  , ("test_translate", test_translate)
  , ("test_doubleEverySnd", test_doubleEverySnd)
  , ("test_convertToBase", test_convertToBase)
  , ("test_summarize", test_summarize)
  , ("test_getCheckCode", test_getCheckCode)
  , ("test_getCheckChar", test_getCheckChar)
  , ("test_generate", test_generate)
  , ("test_validate", test_validate)
  ]

type Entry      = (Char, Int)
type Dictionary = [Entry]

buildDictionary :: [Char] -> Dictionary
buildDictionary cs = zip cs [0..]

test_buildDictionary = [
   buildDictionary "abcdef" == [('a', 0), ('b', 1), ('c', 2), ('d', 3), ('e', 4), ('f', 5)]
 , buildDictionary (['0'..'9'] ++ ['a'..'z']) ==
    [('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5)
    ,('6', 6), ('7', 7), ('8', 8), ('9', 9), ('a', 10), ('b', 11)
    ,('c', 12), ('d', 13), ('e', 14), ('f', 15), ('g', 16), ('h', 17)
    ,('i', 18), ('j', 19), ('k', 20), ('l', 21), ('m', 22), ('n', 23)
    ,('o', 24), ('p', 25), ('q', 26), ('r', 27), ('s', 28), ('t', 29)
    ,('u', 30), ('v', 31), ('w', 32), ('x', 33), ('y', 34), ('z', 35)
    ]
 , buildDictionary ['a'..'z'] ==
    [('a', 0), ('b', 1), ('c', 2), ('d', 3), ('e', 4), ('f', 5)
    ,('g', 6), ('h', 7), ('i', 8), ('j', 9), ('k', 10), ('l', 11)
    ,('m', 12), ('n', 13), ('o', 14), ('p', 15), ('q', 16), ('r', 17)
    ,('s', 18), ('t', 19), ('u', 20), ('v', 21), ('w', 22), ('x', 23)
    ,('y', 24), ('z', 25)
    ]
 ]

dictionary1 = buildDictionary "abcdef"
dictionary2 = buildDictionary ['a'..'z']
dictionary3 = buildDictionary (['0'..'9'] ++ ['a'..'z'])

isValidCharacter :: Dictionary -> Char -> Bool
isValidCharacter d c = any (\(c', _) -> c' == c) d

tests_isValidCharacter = [
   isValidCharacter dictionary1 'a'
 , not (isValidCharacter dictionary1 't')
 , not (isValidCharacter dictionary1 '3')
 , isValidCharacter dictionary2 't'
 , isValidCharacter dictionary3 '4'
 ]

isValidCharSequence :: Dictionary -> String -> Bool
isValidCharSequence d s = all (isValidCharacter d) s

tests_isValidCharSequence = [
   isValidCharSequence dictionary1 "abc"
 , not (isValidCharSequence dictionary1 "foobar")
 , isValidCharSequence dictionary2 "foobar"
 , isValidCharSequence dictionary3 "zulu110"
 ]

findCode :: Char -> Dictionary -> Int
findCode c ((x,y):xs)
 | c == x    = y
 | otherwise = findCode c xs
findCode c _ = error ("findCode: Illegal character " ++ show c)

test_findCode = [
   findCode 'a' dictionary1 == 0
 , findCode 'f' dictionary1 == 5
 , findCode 'f' dictionary2 == 5
 , findCode 'f' dictionary3 == 15
 , findCode '4' dictionary1 `raises` "findCode: Illegal character '4'"
 , findCode ' ' dictionary1 `raises` "findCode: Illegal character ' '"
 ]

findChar :: Int -> Dictionary -> Char
findChar i ((x,y):xs)
  | i == y    = x
  | otherwise = findChar i xs
findChar i _ = error $ "findChar: Illegal code " ++ show i

test_findChar = [
   findChar 0  dictionary1 == 'a'
 , findChar 8  dictionary2 == 'i'
 , findChar 0  dictionary3 == '0'
 , findChar 30 dictionary1 `raises` "findChar: Illegal code 30"
 , findChar 30 dictionary2 `raises` "findChar: Illegal code 30"
 ]

translate :: Dictionary -> String -> [Int]
translate d s = map (\c -> findCode c d) s

test_translate = [
   translate dictionary1 "abcdef" == [0, 1, 2, 3, 4, 5]
 , translate dictionary3 "abcdef" == [10, 11, 12, 13, 14, 15]
 , translate dictionary3 "ball007" == [11,10,21,21,0,0,7]
 ]

doubleEverySnd :: [Int] -> [Int]
doubleEverySnd ns = reverse (double (reverse ns))
  where
    double []       = []
    double [x]      = [2 * x]
    double (x:y:xs) = (2 * x) : y : double xs

test_doubleEverySnd = [
   doubleEverySnd [] == []
 , doubleEverySnd [1] == [2]
 , doubleEverySnd [1..10] == [1, 4, 3, 8, 5, 12, 7, 16, 9, 20]
 , doubleEverySnd [1..11] == [2, 2, 6, 4, 10, 6, 14, 8, 18, 10, 22]
 , doubleEverySnd [1..42] ==
     [ 1, 4, 3, 8, 5, 12, 7, 16, 9, 20, 11, 24, 13, 28, 15, 32, 17, 36
     , 19, 40, 21, 44, 23, 48, 25, 52, 27, 56, 29, 60, 31, 64, 33, 68, 35
     , 72, 37, 76, 39, 80, 41, 84
     ]
 ]

type Base = Int

convertToBase :: Int -> Base -> [Int]
convertToBase _ b | b <= 1 = error ("convertToBase: Invalid base " ++ show b)
convertToBase 0 _ = [0]
convertToBase n b | n > 0 = convert n b
  where
     convert 0 _ = []
     convert n b = (n `mod` b) : convert (n `div` b) b
convertToBase n _ = error ("convertToBase: Invalid number " ++ show n)

test_convertToBase = [
   convertToBase 0 6 == [0]
 , convertToBase 5 6 == [5]
 , convertToBase 10 6 == [4, 1]
 , convertToBase 20 5 == [0, 4]
 , convertToBase 10 (-10) `raises` "convertToBase: Invalid base -10"
 , convertToBase (-10) 3  `raises` "convertToBase: Invalid number -10"
 ]

summarize ::  Base -> [Int] -> Int
summarize b ns = sum (concat (map (\n -> convertToBase n b) (doubleEverySnd ns)))

test_summarize = [
   summarize 6 [1..10] == 35
 , summarize 5 [1..10] == 33
 , summarize 6 (translate dictionary3 "foobar") == 28
 ]

getCheckCode :: Base -> [Int] -> Int
getCheckCode b l
  | m == 0    = 0
  | otherwise = (d + 1) * b - s
  where
    s      = summarize b l
    (d, m) = (s `div` b, s `mod` b)

test_getCheckCode = [
   getCheckCode 2 [0,4,5,2,1,7] == 0
 , getCheckCode 6 [0,4,5,2,1,7] == 1
 , getCheckCode 10 [0,4,5,2,1,7] == 7
 , getCheckCode 15 [0,4,5,2,1,7,7,9,8] == 10
 ]

getCheckChar :: Dictionary -> [Int] -> Char
getCheckChar d l = findChar (getCheckCode (length d) l) d

test_getCheckChar = [
   getCheckChar dictionary1 [1,2,3,4,5,0,1,2] == 'd'
 , getCheckChar dictionary3 [1,2,3,4,5,0,1,2] == 'a'
 ]

generate :: Dictionary -> String -> String
generate d str
  | isValidCharSequence d str = str ++ [getCheckChar d (translate d str)]
  | otherwise = error ("generate: Illegal sequence " ++ show str)

test_generate = [
   generate dictionary3 "foobar" == "foobar5"
 , generate dictionary3 "20141205" == "20141205a"
 , generate dictionary2 "foobar" == "foobart"
 , generate dictionary1 "foobar" `raises` "generate: Illegal sequence \"foobar\""
 , generate dictionary2 "20141205" `raises` "generate: Illegal sequence \"20141205\""
 ]

validate :: Dictionary -> String -> Bool
validate d s
  | isValidCharSequence d s = s == generate d (init s)
  | otherwise               = error ("validate: Illegal elements in sequence " ++ show s)

test_validate = [
   validate dictionary2 (generate dictionary2 "foobar")
 , validate dictionary3 (generate dictionary3 "20141205")
 , validate dictionary1 "foobar" `raises` "validate: Illegal elements in sequence \"foobar\""
 ]