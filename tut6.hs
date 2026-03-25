{-
Quick recap of GHCi operations:

:cd        - change working directory
:i <name>  - information about <name>
:l <file>  - load <file> into the interactive environment
:main      - run the main "function" (will be discussed later)
:m <mod>   - load/unload a library module
:t <expr>  - compute the type of <expr>
:r         - reload currently loaded files
-}

-- Like in Prolog, a function definition consists of one or more clauses
-- that can handle separate cases. Unlike in Prolog, Haskell finds the first
-- clause that matches the arguments; it doesn't do any sort of backtracking!

{-
Second Last

Write a function secondLast that returns the second-to-last element of a list.

> secondLast [2, 4, 6, 8]
6
-}

secondLast :: [a] -> a  -- Takes a list of a's and returns an a
secondLast [x, _] = x  -- Matches a list with exactly two elements
secondLast (x:xs) = secondLast xs  -- Matches any nonempty list
secondLast _      = error "secondLast: list too short"  -- Matches anything

{-
k-th element

Write a function kth k l that returns the kth element of a list l,
where elements are numbered starting from 0. (Do not use the built-in operator !!, which does the same thing.)
-}

kth :: Int -> [a] -> a
kth _ []     = error "kth: list too short"
kth n (x:xs) = if n <= 0
    then x
    else kth (n - 1) xs

{-
Bit Count

Write a function bitCount that takes a non-negative Integer and
returns the number of ones in its binary representation.
-}

bitCount :: Integer -> Integer
bitCount 0 = 0
bitCount n = (n `mod` 2) + bitCount (n `div` 2)
-- Does not work for negative numbers!
-- In particular, mod and div behave like the Python % and /. If you
-- want the C-style ones (that round towards 0 rather than -inf),
-- use rem and quot instead.



{-
Reverse

Write a function rev :: [a] -> [a] that reverses a list of any type.
What is your function's running time as a function of N, the length of the input list?
-}

revHelper :: [a] -> [a] -> [a]
revHelper []     acc = acc
revHelper (x:xs) acc = revHelper xs (x:acc)  -- We can use the same accumulator trick from Prolog!

rev :: [a] -> [a]
rev xs = revHelper xs []

{-
Deduplicate

Write a function dedup that eliminates consecutive duplicate elements in a list of integers:

> dedup [2, 4, 4, 4, 6, 6, 8, 4]
[2, 4, 6, 8, 4]
-}

-- > dropSame 4 [4,4,4,5,4]
-- [5,4]
dropSame :: Integer -> [Integer] -> [Integer]
dropSame _ [] = []
dropSame x (y:ys) = if x == y
    then dropSame x ys
    else y:ys

dedup :: [Integer] -> [Integer]
dedup []     = []
dedup (x:xs) = x:dedup (dropSame x xs)  -- Remove all elements that are equal to x from the start
                                        -- and recursively handle the rest

{-
Or

Which of these functions is equivalent to the built-in operator || ?
-}
my_or1, my_or2, my_or3, my_or4 :: Bool -> Bool -> Bool

my_or1 a b = a || b

my_or2 a b = b || a

my_or3 _ True = True  -- Only the second operand needs to be evaluated here
my_or3 True _ = True
my_or3 _ _ = False

my_or4 True False = True  -- Both operands need to be evaluated here, no short-circuit
my_or4 True True = True
my_or4 False True = True
my_or4 False False = False

-- Built-in || short-circuits the evaluation if the first operand is True.
-- We can check what happens with my_or True undefined. undefined is a special
-- value that will raise an exception if someone tries to evaluate it!

-- > my_or1 True undefined
-- True

-- > my_or2 True undefined
-- *** Exception: Prelude.undefined

-- > my_or3 True undefined
-- *** Exception: Prelude.undefined

-- > my_or4 True undefined
-- *** Exception: Prelude.undefined

{-
Fibonacci Numbers

Construct an infinite list containing all Fibonacci numbers: 1, 1, 2, 3, 5, 8, 13, ...
-}

generate :: Integer -> Integer -> [Integer]
generate a b = a:generate b (a + b)

fibs :: [Integer]
fibs = generate 0 1
-- > take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]

{-
Even Fibonacci Numbers

Solve Project Euler's problem 2:

By considering the terms in the Fibonacci sequence whose values do
not exceed four million, find the sum of the even-valued terms.
-}

-- We could definitely solve this with explicit recursion, but I want
-- to give a sneak peak at what we'll be able to do with Haskell:
answer :: Integer
answer = sum $ takeWhile (< 4000000) $ filter even fibs
