type Priority = Int

data Entry a = Entry Priority a
  deriving (Eq, Show)

data PQueue a = PQueue [Entry a]
  deriving (Eq, Show)

(@@) :: a -> Priority -> Entry a
(@@) s p = Entry p s

test_mkEntry = [
    "verd" @@ 2 == Entry 2 "verd"
    , "rouge" @@ 8 == Entry 8 "rouge"
    , "blanc" @@ 3 == Entry 3 "blanc"
    ]

value :: Entry a -> a
value (Entry p a) = a

test_value = [
    value ("ten" @@ 10) == "ten"
    , value ('a' @@ 1)    == 'a'
    ]

priority :: Entry a -> Priority
priority (Entry p a) = p

test_priority = [
    priority ("ten" @@ 10) == 10
    , priority ('a' @@ 1)    == 1
    ]

empty :: PQueue a
empty = PQueue []

separate :: Int -> [Entry a] -> ([Entry a],[Entry a])
separate n ((Entry p s):xs) = span (\(Entry p s) -> n <= p) ((Entry p s):xs)

push :: Entry a -> PQueue a -> PQueue a
push e (PQueue []) = PQueue [e]
push (Entry p s) (PQueue (x:xs)) = PQueue insertQ
    where
        splitQ = separate p (x:xs)
        insertQ = fst splitQ ++ [Entry p s] ++ snd splitQ

-- push x ( PQueue [] )= PQueue [x]
-- push x (PQueue l) = PQueue (gm ++ [x] ++ sm)
--     where
--         gm = [a | a <- l, priority a >= priority x]
--         sm = [b | b <- l, priority b < priority x]
     

test_push = [
    push ("ten" @@ 10) empty == PQueue [Entry 10 "ten"]
    , push ("one" @@ 1) (push ("ten" @@ 10) empty) == PQueue [Entry 10 "ten", Entry 1 "one"]
    , push ("yet another two" @@ 2) (push ("two" @@ 2) (push ("one" @@ 1) empty)) == PQueue [Entry 2 "two", Entry 2 "yet another two", Entry 1 "one"]
    ]

fromList :: [(Priority, a)] -> PQueue a
fromList l = foldl (\acc (p,s) -> push (Entry p s) acc ) empty l
-- fromList l = foldr (\(p,s) acc -> push (Entry p s) acc) empty (reverse l)

-- fromList [] = PQueue []
-- fromList ls = push( (snd $ last ls) @@ (fst $ last ls) ) (fromList (init ls))

test_fromList = [
    fromList ([] :: [(Priority, String)]) == PQueue []
    , fromList [(1,"one"), (5, "five"), (4, "four")] == PQueue [Entry 5 "five", Entry 4 "four", Entry 1 "one"]
    , fromList [(5, "first five"), (1,"one"),  (5, "second five"), (4, "four"), (5, "third five")] == PQueue [Entry 5 "first five", Entry 5 "second five", Entry 5 "third five", Entry 4 "four", Entry 1 "one"]
    ]

priq :: PQueue String
priq = fromList [(10, "ten"), (9, "nine"), (8, "eight"), (2, "two")]

top :: PQueue a -> Maybe (Entry a)
top (PQueue []) = Nothing
top (PQueue l) = Just (head l)

test_top = [
    top priq == Just (Entry 10 "ten")
    , top (empty :: PQueue Int) == Nothing
    ]

remove :: PQueue a -> PQueue a
remove (PQueue []) = empty
remove (PQueue l ) = PQueue (tail l)

test_remove = [
    remove priq == PQueue [Entry 9 "nine", Entry 8 "eight", Entry 2 "two"]
    , remove (empty :: PQueue Int) == empty
    ]

pop :: PQueue a -> (Maybe (Entry a), PQueue a)
pop (PQueue []) = (Nothing, empty)
pop (PQueue l) = (nextElem, newQ)
    where 
        nextElem = top (PQueue l) 
        newQ = remove (PQueue l )

test_pop = [
    pop priq == (Just (Entry 10 "ten"), PQueue [Entry 9 "nine", Entry 8 "eight", Entry 2 "two"])
    , pop (empty :: PQueue Int) == (Nothing, empty)
    ]

-- instance Show a => Show (Entry a) where
--     show (Entry p v) = show p ++ " -> " ++ show v
-- instance Show a => Show (PQueue a) where 
--     show (PQueue (x:xs)) = "PQueue [" ++ show x ++ "," ++ tail (show xs)

-- test_show = [
--     show ('t' @@ 10) == "10 -> 't'"
--     , show ("a" @@ 5)  == "5 -> \"a\""
--     , show (True @@ 1) == "1 -> True"
--     , show (fromList [(1, True), (2, False)]) == "PQueue [2 -> False,1 -> True]"
--     ]



 
    