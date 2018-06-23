import Data.Maybe
-- Functional languages practice 12
-- Type class instances

-- Define Time and UKTime types. Then
--   1) make them instances of the Show type class
--   2) make them instances of the Eq type class
--   3) make them instances of the Ord type class

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

data Time = Time Int Int

data UKTime = AM Int Int
            | PM Int Int

{-
Type class 'Show' is declared like this:

class Show a where
  show :: a -> String

This means that there exist several definitions of 'show', one for
each specific value of type variable 'a'. For instance, one for
integers, one for booleans:

show 0      This 'show' has type Int -> String
show True   This 'show' has type Bool -> String

Behind the scenes, GHCi will call different 'show' definitions.
Without type classes, we would be required to use showInt and
showBool, because we could not define two functions with the same
name.

Type classes allow what is called *overloading* in programming
languages: several function definitions have the same name.
-}

-- 'show' on the left hand side of '=' is a different 'show' than on
-- the right hand side.
-- On the left hand side it is a function of Time -> String.
-- On the right hand side it is a function of Int -> String.
instance Show Time where
  show (Time h m) = show h ++ ":" ++ show m

{-
-- When you use 'deriving Show', GHCi will generate the following for you:
instance Show Time where
  show (Time h m) = "Time " ++ show h ++ " " ++ show m
-}

instance Show UKTime where
  show (AM h m) = show h ++ ":" ++ show m ++ " AM"
  show (PM h m) = show h ++ ":" ++ show m ++ " PM"

instance Eq Time where
(Time h m) == (Time h' m') = h == h' && m == m'

(Time h m) /= (Time h' m') = h /= h' || m /= m'

-- Nullable type class

-- Define a type for files. A file is represented by its contents.

-- Define a type class Nullable. Instances of this class
-- may not store any data.
-- It should have two methods: isNull and isNotNull

-- Make file an instance of the Nullable type class.

-- Make list of 'a's an instance of the Nullable type class.

-- Make Maybe an instance of the Nullable type class.

class Nullable a where
  isNull    :: a -> Bool

  isNotNull :: a -> Bool

data File = File String

instance Nullable File where
  isNull (File contents)    = contents == []

  isNotNull (File contents) = contents /= []

instance Nullable [a] where
  isNull []     = True
  isNull (x:xs) = False

  isNotNull []     = False
  isNotNull (x:xs) = True

-- Measurable type class

-- Define a type class Measurable. Instances of this class
-- have some notion of size.

-- Make file an instance of the Measureable type class. The size
-- should return the number of characters in the file.

-- Make list of 'a's an instance of the Measureable type class.

class Measurable a where
  size :: a -> Int

instance Measurable File where
  size (File contents) = length contents

instance Measurable [a] where
  size xs = length xs

-- Make file an instance of the Show type class.

instance Show File where
  show (File contents) = "File [" ++ contents ++ "]"

-- Make file an instance of the Eq type class.

instance Eq File where
  (File contents) == (File contents') = contents == contents'

  (File contents) /= (File contents') = contents /= contents'

-- Parametric types

-- Define 'update' that updates ith element of a list.
-- The result of update n f xs depends on f.
-- If f returns Just x, then the nth element is replaced by x.
-- If f returns Nothing, then the nth element is removed from the
-- list.
-- You can use function 'maybe' from Data.Maybe.

update :: Int -> (a -> Maybe a) -> [a] -> [a]
update 0 f (x:xs) = maybe xs (\x' -> x':xs) (f x)
update n f (x:xs)
  | n > 0 = x : update (n - 1) f xs
update n f l
  | n < 0 = error ("update: index is negative: " ++ show n)
update n f []     = error "update: index is too large"

test_update = [
  update 1 (\n -> Just (n + 1)) [2,3,4] == [2,4,4]
, update 1 (\_ -> Nothing)      [2,3,4] == [2,4]
, update 3 (\_ -> Nothing)      "HELP"  == "HEL"
, update 2 (\_ -> Just 'P')     "MAT"   == "MAP"
]

-- Advanced exercises

-- Define a parametric type for password-protected contents.
-- The type parameter is the type that holds the contents.

type Password = String

data Protected a = P Password a

-- Define a function that password-protects a file.
-- Then make it more general, so that it works for anything, not
-- just for files.

protectFile :: File -> Password -> Protected File
protectFile f pw = P pw f

protect :: a -> Password -> Protected a
protect x pw = P pw x

-- Define a function that removes password-protection on a file if
-- the correct password is given.
-- The result should be a Maybe.
-- Then make it more general, so that it works for anything, not
-- just for files.

unprotectFile :: Protected File -> Password -> Maybe File
unprotectFile (P pw file) pw'
  | pw == pw' = Just file
  | otherwise = Nothing

unprotect :: Protected a -> Password -> Maybe a
unprotect (P pw x) pw'
  | pw == pw' = Just x
  | otherwise = Nothing

-- Define a parametric type for zip files. A zip file is just a list
-- of path and contents pairs.

data Zip a = ZipFile [(String, a)]

-- Create sample zip files of password-unprotected and
-- password-protected files.

unprotectedZip :: Zip File
unprotectedZip = ZipFile [ ("/config.txt", File "time zone = UTC")
                        , ("/startpage.txt",  File "https://google.com")
                        ]

protectedZip :: Zip (Protected File)
protectedZip = ZipFile [ ("/config.txt", protect (File "time zone = UTC") "secret")
                      , ("startpage.txt", protect (File "https://google.com") "secret2")
                      ]

-- Define a function that adds a file to a zip file.

add :: File -> String -> Zip File -> Zip File
add file path (ZipFile entries) = ZipFile ((path, file) : entries)

-- Define a function that unzips a file from a zip file using
-- path.
-- Use lookup above.
-- This function simply retrieves the file from the list of
-- contents of the zip file.
getFromZip :: String -> Zip File -> Maybe File
getFromZip p (ZipFile entries) = lookup p entries