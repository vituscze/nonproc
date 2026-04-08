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

{-
All Primes

Construct an infinite list containing all prime numbers using the Sieve of Eratosthenes.
-}

-- Difference of two sorted lists (for simplicity assuming the lists don't have duplicates)
diff :: Ord a => [a] -> [a] -> [a]
diff [] _  = []
diff xs [] = xs
diff (x:xs) (y:ys)
    | x < y     = x:diff xs (y:ys)
    | x == y    = diff xs ys
    | otherwise = y:diff (x:xs) ys

primes :: [Integer]
primes = 2:sieve [3,5..]
  where
    sieve []     = []
    sieve (x:xs) = x:sieve (xs `diff` [x * x, x * x + 2 * x ..])

{-
Quicksort

Write a function that sorts a list of values using quicksort. Give your function
an appropriate type using a type class constraint. When partitioning a list, use
the first element as the pivot (which will be very inefficient if the list is
already sorted, but that is OK for this exercise).
-}

-- Note that this isn't exactly quicksort since one of the key features
-- of that algorithm is the in-place partition, which we obviously cannot
-- do on a linked list.
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

{-
Amicable Numbers

Solve Project Euler's problem 21.
-}

properDivisors :: Integral a => a -> [a]
properDivisors n = [d | d <- [1 .. n `div` 2], n `mod` d == 0]

divisorSum :: Integral a => a -> a
divisorSum n = sum (properDivisors n)

isAmicable :: Integral a => a -> Bool
isAmicable a = a /= b && divisorSum b == a
  where
    b = divisorSum a

problem21 :: Int
problem21 = sum [n | n <- [1 .. 10000], isAmicable n]

-- Somewhat faster
problem21' :: Int
problem21' = sum [n | n <- [1 .. 10000], isAmicable' n]
  where
    sums = [divisorSum n | n <- [0 ..]]

    isAmicable' a = a /= b && sums !! b == a
      where
        b = sums !! a
