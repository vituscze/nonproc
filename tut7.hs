{-
Function Types

What are the types of these functions?

a) triple x = 3 * x
b) even x = (x `mod` 2 == 0)
c) palindrome xs = (reverse xs == xs)
d) f x = f x

triple is a function, so it must have a type a -> b for some
a and b. (*) has the type Num a => a -> a -> a and 3 has the
type Num a => a, which forces x to have the type Num a => a.

This narrows down the function to Num a => a -> b. The result
is 3 * x, which has the type Num a => a, giving us the whole
function type as Num a => a -> a

As above, even has to have type a -> b. x is used with mod, which
has the type Integral a => a -> a -> a. The type of 2 is Num a => a,
giving us (Integral a, Num a) => a as the type of (x `mod` 2), but since
Integral is a subclass of Num, we can simplify to just Integral a => a.
(==) has the type Eq a => a -> a -> Bool, which forces a to also be in
Eq (but that one is also implied by Integral). The return value is thus
Bool (= b) and the type of the function Integral a => a -> Bool

reverse has the type [a] -> [a], (==) the type Eq b => b -> b -> Bool.
xs and reverse xs thus has the type [a], which can match (==) if we
set b = Eq [a] => [a], which simplifies to Eq a => [a]. Combining it all
together, we get Eq a => [a] -> Bool

For the final function, we assume f :: a -> b as before. Therefore x :: a.
What's the type of f x? Well, x :: a matches f :: a -> b and doesn't restrict
any other type. a -> b is thus the most general type for f.
-}

{-
List Functions

Write these functions (which are all built into the standard library):

a) isPrefixOf :: Eq a => [a] → [a] → Bool

Take two lists and return true if the first list is a prefix of the second.

b) isInfixOf :: Eq a => [a] → [a] → Bool

Take two lists and return true if the first list is contained anywhere within the second.

c) group :: Eq a => [a] → [[a]]

Group adjacent identical elements into sublists. For example,

group "Mississippi" == ["M","i","ss","i","ss","i","pp","i"]
-}

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf []     _      = True
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
isPrefixOf _      _      = False

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs []     = null xs
isInfixOf xs (y:ys) = isPrefixOf xs (y:ys) || isInfixOf xs ys

group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:y:ys)
    | x == y    = add x rest
    | otherwise = [x]:rest
  where
    add x []       = [[x]]
    add x (xs:xss) = (x:xs):xss

    rest = group (y:ys)


{-
Cyclic List

Implelement the built-in function cycle that takes a list L and returns an infinite list consisting of L repeated over and over:

> take 10 (cycle "abc")
"abcabcabca"
-}

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

{-
Greatest Common Divisor

Write a function that computes the greatest common divisor of two numbers. It should work with any integral type, e.g. either Int or Integer.
-}

gcd' :: Integral a => a -> a -> a
gcd' x 0 = x
gcd' x y = gcd' y (x `mod` y)

{-
Vector Sum

Write a function add_vec that adds two vectors represented as lists. Give your function the most general possible type.
-}

addVec :: Num a => [a] -> [a] -> [a]
addVec []     _      = []
addVec _      []     = []
addVec (x:xs) (y:ys) = (x + y):addVec xs ys

{-
Dot Product

Write a function dot that computes the dot product of two vectors represented as lists.
-}

dot :: Num a => [a] -> [a] -> a
dot []     _      = 0
dot _      []     = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

{-
Matrix Addition

Write a function add that adds two matrices represented as lists of lists.
-}

add :: Num a => [[a]] -> [[a]] -> [[a]]
add []     _      = []
add _      []     = []
add (x:xs) (y:ys) = addVec x y:add xs ys

{-
Transpose

Write a function that transposes a matrix represented as a list of lists.
-}

splitCol :: [[a]] -> ([a], [[a]])
splitCol ((x:xs):xss) = let (col, rest) = splitCol xss in (x:col, xs:rest)
splitCol _            = ([], [])

transpose :: [[a]] -> [[a]]
transpose xs = case splitCol xs of
    (_, []) -> []
    (c, cs) -> c:transpose cs

{-
All Pairs

Construct an infinite list allPairs that contains all pairs of positive integers. Every pair must appear exactly once in the list.
-}

diagonal :: Integer -> [(Integer, Integer)]
diagonal n = [(x, n - x) | x <- [0..n]]

allPairs :: [(Integer, Integer)]
allPairs = [p | n <- [0..], p <- diagonal n]

{-
Mergesort

Write a function that sorts a list of values using a mergesort. Give your function an appropriate type using a type class constraing.
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

split :: [a] -> ([a], [a])
split []     = ([], [])
split (x:xs) = (x:r, l)  -- Flip the pair so that x alternates between left and right lists
  where
    (l, r) = split xs

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort l) (mergeSort r)
  where
    (l, r) = split xs

{-
Collatz Sequence

Solve Project Euler's problem 14.

Which starting number, under one million, produces the longest chain?
-}

collatz :: Integer -> Integer
collatz n = go 1 n
  where
    go acc 1 = acc
    go acc n = go (acc + 1) (if even n then n `div` 2 else 3 * n + 1)

euler14 :: Integer
euler14 = snd (maximum [(collatz n, n) | n <- [1 .. 999999]])
