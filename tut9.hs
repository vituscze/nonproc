iterate' :: (a -> a) -> a -> [a]
iterate' f x = x:iterate' f (f x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x:takeWhile' p xs
    | otherwise = []

find' :: (a -> Bool) -> [a] -> Maybe a
find' p xs = case filter p xs of
    []  -> Nothing
    x:_ -> Just x

altMap, altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x:altMap g f xs

-- Without explicit recursion, just with library functions
altMap' f g = zipWith id (cycle [f, g])

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _   []     = error "maximumBy': empty list"
maximumBy' _   [x]    = x
maximumBy' cmp (x:xs) = case cmp x maxRest of
    LT -> maxRest
    _  -> x
  where
    maxRest = maximumBy' cmp xs

maximumOn' :: Ord b => (a -> b) -> [a] -> a
maximumOn' f = maximumBy' (\x y -> compare (f x) (f y))

{-
Subsets

Write a function subsets :: [a] -> [[a]] that returns a list of all subsets of a set represented as a list.
-}
subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = [set | rest <- subsets xs, set <- [rest, x:rest]]
--               subsets xs ++ map (x:) (subsets xs)

{-
Combinations

Write a function combinations :: Int -> [a] -> [[a]] that takes an integer k and a list,
and returns a list of all combinations of k elements of the elements in the list. The elements
in each combination should appear in the same order as in the original list:

> combinations 2 "zebra"
["ze","zb","zr","za","eb","er","ea","br","ba","ra"]
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]  -- Exactly one trivial combination
combinations _ []     = []    -- Not enough elements for a combination
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++ combinations k xs
-- TODO: This definition is somewhat wasteful because
-- combinations k xs where k > length xs will process a lot of lists
-- even though there are no combinations of that size.

{-
Permutations

Write a function perms :: [a] -> [[a]] that returns a list of all permutations of a given list.
Do not assume that the type a implements Eq.
-}
perms, perms' :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [p | rest <- perms xs, p <- insertAll rest]
            -- concatMap insertAll (perms xs)
  where
    insertAll []     = [[x]]
    insertAll (y:ys) = (x:y:ys):map (y:) (insertAll ys)

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- select xs]
             -- (x, xs):map (second (x:)) (select xs)
             -- second comes from Control.Arrow

perms' [] = [[]]
perms' xs = [y:p | (y, ys) <- select xs, p <- perms' ys]
         -- concatMap (\(y, ys) -> map (y:) $ perms' ys) (select xs)

{-
Partitions

A partition of a positive integer n is a set of positive integers that add up to n. For example, the number 4 has 5 partitions:

1 + 1 + 1 + 1
2 + 1 + 1
2 + 2
3 + 1
4

Write a function partitions :: Int -> [[Int]] that produces a list of all partitions of a positive integer.
-}
partitions :: Int -> [[Int]]
partitions n = go n n
  where
    go 0 _   = [[]]
    go n lim = [x:rest | x <- [1 .. min n lim], rest <- go (n - x) x]
            -- concatMap (\x -> map (x:) $ go (n - x) x) [1 .. min n lim]

{-
N Queens

Is it possible to place N queens on an N x N chessboard such that no two queens attack each other?

a) Write a function that can produce a list of all possible solutions for a given N, where each solution
is a list of positions of queens on the board. Use an exhaustive search. For example:

> queens 4
[[(2,1),(4,2),(1,3),(3,4)],[(3,1),(1,2),(4,3),(2,4)]]

b) Modify your function to return a list of strings, where each string depicts a possible solution:

> queens 4
[". Q . . \n. . . Q \nQ . . . \n. . Q . \n",". . Q . \nQ . . . \n. . . Q \n. Q . . \n"]
-}

type Pos = (Int, Int)

threaten :: Pos -> Pos -> Bool
threaten (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x1 - x2) == abs (y1 - y2)

queens :: Int -> [[Pos]]
queens n = go 1 []
  where
    go x acc
        | x > n     = [acc]
        | otherwise = [sol | y <- [1 .. n]
                           , not $ any (threaten (x, y)) acc
                           , sol <- go (x + 1) ((x, y):acc)]


toLine :: Int -> Int -> String
toLine n k = unwords [replicate (k - 1) '.', "Q", replicate (n - k) '.', "\n"]

toBox :: Int -> [Pos] -> String
toBox n sol = concat [toLine n y | (_, y) <- sol]
-- toBox n = concatMap (toLine n . snd)

queens' :: Int -> [String]
queens' n = map (toBox n) (queens n)
-- queens' = map <$> toBox <*> queens
-- Although we probably won't have time to cover <$> and <*>
