import Data.Function
{-
Time

a) Create a Haskell type Time that represents a time of day with 1-second resolution,
such as 5:15:02 or 13:12:45. Create a function time :: Int -> Int -> Int -> Time that
constructs a Time from values h, m, and s, where 0 <= h < 24 and 0 <= m, s < 60.

b) Declare that Time is an instance of the type classes Eq, Ord, Enum, Bounded, and Show.

c) Write a function add :: Time -> Int -> Time that adds a (positive or negative) number
of seconds to a Time, wrapping past midnight if necessary.
-}

newtype Time = Time { getTime :: Int }

time :: Int -> Int -> Int -> Time
time h m s = Time $ (3600 * h + 60 * m + s) `mod` (24 * 3600)

add :: Time -> Int -> Time
add (Time s1) s2 = time 0 0 (s1 + s2)

instance Eq Time where
    (==) :: Time -> Time -> Bool
    (==) = (==) `on` getTime

instance Ord Time where
    compare :: Time -> Time -> Ordering
    compare = compare `on` getTime

instance Enum Time where
    toEnum :: Int -> Time
    toEnum = time 0 0

    fromEnum :: Time -> Int
    fromEnum = getTime

instance Bounded Time where
    minBound :: Time
    minBound = time 0 0 0

    maxBound :: Time
    maxBound = time 23 59 59

instance Show Time where
    show :: Time -> String
    show (Time t) = concat [pad h, ":", pad m, ":", pad s]
      where
        (t', s) = t  `divMod` 60
        (h,  m) = t' `divMod` 60

        pad = reverse . take 2 . (++ repeat '0') . reverse . show

{-
Fractions

Consider the GFrac datatype that we saw in the lecture, representing a fraction:
-}
data GFrac t = GFrac t t deriving (Show)

instance (Eq t, Num t) => Eq (GFrac t) where
    (==) :: (Eq t, Num t) => GFrac t -> GFrac t -> Bool
    (==) (GFrac a b) (GFrac c d) = a * d == b * c

{-
Declare that this datatype is an instance of the Ord and Num type classes.
-}

instance (Ord t, Num t) => Ord (GFrac t) where
    compare :: (Ord t, Num t) => GFrac t -> GFrac t -> Ordering
    compare f1 f2 = case (norm f1, norm f2) of
        (GFrac a b, GFrac c d) -> compare (a * d) (b * c)
      where
        norm (GFrac x y) = GFrac (signum y * x) (abs y)

instance Num t => Num (GFrac t) where
    (+) :: Num t => GFrac t -> GFrac t -> GFrac t
    GFrac a b + GFrac c d = GFrac (a * d + b * c) (b * d)

    (*) :: Num t => GFrac t -> GFrac t -> GFrac t
    GFrac a b * GFrac c d = GFrac (a * c) (b * d)

    abs :: Num t => GFrac t -> GFrac t
    abs (GFrac a b) = GFrac (abs a) (abs b)

    signum :: Num t => GFrac t -> GFrac t
    signum (GFrac a b) = GFrac (signum a * signum b) 1

    fromInteger :: Num t => Integer -> GFrac t
    fromInteger i = GFrac (fromInteger i) 1

    negate :: Num t => GFrac t -> GFrac t
    negate (GFrac a b) = GFrac (negate a) b

{-
Dictionary

a) Declare a polymorphic type class Dictionary that represents a mapping from element of
    type 'k' to type 'v'. It should have an empty dictionary value, plus operations to get
    or set the value for a given key. Retrieving the value of an absent key should return
    Nothing. Assume that keys are orderable.

b) Declare a datatype Assoc k v representing an association list, i.e. a list of key-value pairs.
   Declare that Assoc is an instance of Dictionary. Also declare that it is an instance of Functor.
   fmap should affect the values (not the keys) stored in the association list.

c) Declare a binary tree type that can map keys to values. Declare that it is an instance of
   Dictionary. Also declare that it is an instance of Functor. fmap should affect the values
   (not the keys) stored in the tree.
-}

class Dictionary d where
    empty :: d k v
    get :: Ord k => k -> d k v -> Maybe v
    set :: Ord k => k -> v -> d k v -> d k v

newtype Assoc k v = Assoc { getAssoc :: [(k, v)] } deriving (Show)

instance Dictionary Assoc where
    empty :: Assoc k v
    empty = Assoc []

    get :: Ord k => k -> Assoc k v -> Maybe v
    get k = lookup k . getAssoc

    set :: Ord k => k -> v -> Assoc k v -> Assoc k v
    set k v = Assoc . go . getAssoc
      where
        go [] = [(k, v)]
        go ((k', v'):kvs)
            | k == k'   = (k, v):kvs
            | otherwise = (k', v'):go kvs

instance Functor (Assoc k) where
    fmap :: (a -> b) -> Assoc k a -> Assoc k b
    fmap f = Assoc . map (fmap f) . getAssoc

data TreeMap k v = Nil | Node (TreeMap k v) k v (TreeMap k v) deriving (Show)

instance Dictionary TreeMap where
    empty :: TreeMap k v
    empty = Nil

    get :: Ord k => k -> TreeMap k v -> Maybe v
    get k Nil = Nothing
    get k (Node l k' v r) = case k `compare` k' of
        LT -> get k l
        EQ -> Just v
        GT -> get k r

    set :: Ord k => k -> v -> TreeMap k v -> TreeMap k v
    set k v Nil = Node Nil k v Nil
    set k v (Node l k' v' r) = case k `compare` k' of
        LT -> Node (set k v l) k' v' r
        EQ -> Node l k v r
        GT -> Node l k' v' (set k v r)

instance Functor (TreeMap k) where
    fmap :: (a -> b) -> TreeMap k a -> TreeMap k b
    fmap _ Nil = Nil
    fmap f (Node l k v r) = Node (fmap f l) k (f v) (fmap f r)

{-
Finite State Machine

As you may have learned in an automata class, a finite state machine consists of

    a finite set of states

    a finite set of input symbols

    a transition function that maps a state and an input symbol to a new state

    a start state

    a set of final (or accepting) states

In this exercise we will consider finite state machines whose states are integers and
whose input symbols are characters.

a) Design finite state machines that accept the following sets of characters, represented
   by regular expressions:

    ab(..)*

    (.*)aaba(.*)

b) Design a representation for finite state machines in Haskell.

c) Write a function accept that takes a finite state machine and a string, and returns true
   if the the machine accepts the string. Use your function to test the finite state machines
   that you defined in part (a).
-}

data FSM = FSM
    { start :: Integer
    , final :: [Integer]
    , trans :: Integer -> Char -> Integer
    }

accept :: FSM -> String -> Bool
accept (FSM start final trans) = (`elem` final) . foldl trans start
-- Try using it with scanl!

exampleA :: FSM
exampleA = FSM 0 [2] trans
  where
    -- 0 is the initial state
    -- 1 is the state after consuming a
    -- 2 is the state after consuming ab + even number of characters
    -- 3 is the same as 2, but for odd number
    -- 4 is the fail state (leads to itself, not an accepting state)
    trans 0 'a' = 1
    trans 0 _   = 4
    trans 1 'b' = 2
    trans 1 _   = 4
    trans 2 _   = 3
    trans 3 _   = 2
    trans 4 _   = 4
    trans _ _   = error "bad state"
