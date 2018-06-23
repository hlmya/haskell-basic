type Priority = Int

data Entry a = Entry Priority a
  deriving (Eq)
  
data PQueue a = PQueue [Entry a]
  deriving (Eq)

{-

allTests = (and (concatMap snd tests), tests)

tests =
  [ ("test_mkEntry", test_mkEntry)
  , ("test_value", test_value)
  , ("test_priority", test_priority)
  , ("test_push", test_push)
  , ("test_fromList", test_fromList)
  , ("test_top", test_top)
  , ("test_remove", test_remove)
  , ("test_pop", test_pop)
  , ("test_show", test_show)
  ]

-}

(@@) :: a -> Priority -> Entry a
(@@) a b = Entry b a

test_mkEntry = [
   "verd" @@ 2 == Entry 2 "verd"
 , "rouge" @@ 8 == Entry 8 "rouge"
 , "blanc" @@ 3 == Entry 3 "blanc"
 ]
 
value :: Entry a -> a
value (Entry b a) = a
test_value = [
   value ("ten" @@ 10) == "ten"
 , value ('a' @@ 1)    == 'a'
 ]
 
priority :: Entry a -> Priority
priority (Entry b a) = b

test_priority = [
   priority ("ten" @@ 10) == 10
 , priority ('a' @@ 1)    == 1
 ]
 
empty :: PQueue a
empty = PQueue []

--span :: (a -> Bool) -> [a] -> ([a], [a])


push :: Entry a -> PQueue a -> PQueue a
push x ( PQueue [] )= PQueue [x]
push x (PQueue ls) = PQueue ( gm ++ [x] ++ sm )
		where 
		gm = [a | a <- ls , (priority a) >= (priority x) ]
		sm = [ b | b <- ls, (priority b) < (priority x) ] 
     

test_push = [
   push ("ten" @@ 10) empty == PQueue [Entry 10 "ten"]
 , push ("one" @@ 1) (push ("ten" @@ 10) empty) == PQueue [Entry 10 "ten", Entry 1 "one"]
 , push ("yet another two" @@ 2) (push ("two" @@ 2) (push ("one" @@ 1) empty)) == PQueue [Entry 2 "two", Entry 2 "yet another two", Entry 1 "one"]
 ]

flat [] = []
flat (x:xs)= fst x : flat xs
 
fromList :: [(Priority, a)] -> PQueue a
fromList [] = PQueue []
fromList ls = push( (snd $ last ls) @@ (fst $ last ls) ) (fromList (init ls)) 

test_fromList = [
   fromList ([] :: [(Priority, String)]) == PQueue []
 , fromList [(1,"one"), (5, "five"), (4, "four")] == PQueue [Entry 5 "five", Entry 4 "four", Entry 1 "one"]
 , fromList [(5, "first five"), (1,"one"),  (5, "second five"), (4, "four"), (5, "third five")] == PQueue [Entry 5 "first five", Entry 5 "second five", Entry 5 "third five", Entry 4 "four", Entry 1 "one"]
 ]
 
priq :: PQueue String
priq = fromList [(10, "ten"), (9, "nine"), (8, "eight"), (2, "two")]

top :: PQueue a -> Maybe (Entry a)
top (PQueue []) = Nothing
top (PQueue ls) = Just (head ls)

test_top = [
   top priq == Just (Entry 10 "ten")
 , top (empty :: PQueue Int) == Nothing
 ]
 
remove :: PQueue a -> PQueue a
remove (PQueue [])= empty
remove (PQueue ls) = PQueue (tail ls)

test_remove = [
   remove priq == PQueue [Entry 9 "nine", Entry 8 "eight", Entry 2 "two"]
 , remove (empty :: PQueue Int) == empty
 ]
 
pop :: PQueue a -> (Maybe (Entry a), PQueue a)
pop (PQueue []) = (Nothing,empty)
pop (PQueue ls) = (Just (head ls) , PQueue (tail ls))

test_pop = [
   pop priq == (Just (Entry 10 "ten"), PQueue [Entry 9 "nine", Entry 8 "eight", Entry 2 "two"])
 , pop (empty :: PQueue Int) == (Nothing, empty)
 ]
islast :: Eq a => a -> [a] -> Bool
islast x ls = x == last ls


--lists [x] = True
--lists [x] = [True]
--lists (x:xs) = islast xs : lists xs
  --x == last ls (Entry p v)
--( islast (Entry p v) ls?","
instance Show a => Show (Entry a) where
    show (Entry p v) = show p ++ " -> " ++ show v
instance Show a => Show (PQueue a) where 
 show (PQueue (x:xs)) = "PQueue [" ++ show x ++ "," ++ tail (show xs)
test_show = [
    show ('t' @@ 10) == "10 -> 't'"
  , show ("a" @@ 5)  == "5 -> \"a\""
  , show (True @@ 1) == "1 -> True"
  , show (fromList [(1, True), (2, False),(3,False),(5,True)]) == "PQueue [5 -> True,3 -> False,2 -> False,1 -> True]"
  ]