import Control.Monad
import Data.Array
import Data.List
import Data.Maybe

{-
Counting Sort

Write a function csort :: (Int, Int) -> [Int] -> [Int] that implements a counting sort. (csort (lo, hi) xs)
should sort a list xs of values, all of which are in the range lo ≤ x ≤ hi. It should run in O(N), where N = length xs.
-}

csort :: (Int, Int) -> [Int] -> [Int]
csort bounds vals = concatMap (uncurry (flip replicate)) (assocs arr)
  where
    arr = accumArray (+) 0 bounds $ zip vals [1, 1..]

{-
Applicative Functor

Rewrite the expression [(x, y) | x <- [1..3], y <- [1..4]] using no variables, by using the applicative functor operator <*>.
-}

expr :: [(Int, Int)]
expr = (,) <$> [1..3] <*> [1..4]

{-
Sequence

The function sequence has type

sequence :: Monad m => [m a] → m [a]

It combines a list of monadic values into a single monadic value. For example:

> sequence [Just 4, Just 3, Just 2]
Just [4,3,2]
> sequence [Just 4, Nothing, Just 2]
Nothing

sequence [a, b, c] is the same as

do
  x <- a
  y <- b
  z <- c
  return [x, y, z]

Implement sequence.
-}

-- Available as just liftM2 in the standard library
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f ma mb = do
    a <- ma
    b <- mb
    pure $ f a b

sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr (liftM2 (:)) (pure [])

{-
In Haskell, we may represent a graph in adjacency-list representation using an association list that maps each vertex to a list of its neighbors:

type Graph a = [(a, [a])]

For example:

graph = [ ('a', "ce"), ('b', "de"), ('c', "adf"), ('d', "bcef"),
          ('e', "bdf"), ('f', "cdeg"), ('g', "f") ]

a) Write a function

adjacent :: Eq a => Graph a -> a -> [a]

that returns a list of a vertex's neighbors in a graph.

b) Write a function

paths :: Eq a => Graph a -> a -> a -> [[a]]

that takes a graph and the ids of start and end vertices v and w, and returns a list of all possible paths from v to w,
where a path is represented as a list of vertices. A path may not contain the same vertex twice.
-}

type Graph a = [(a, [a])]

example :: Graph Char
example = [ ('a', "ce"), ('b', "de"), ('c', "adf"), ('d', "bcef"),
            ('e', "bdf"), ('f', "cdeg"), ('g', "f") ]

adjacent :: Eq a => Graph a -> a -> [a]
adjacent g v = fromMaybe [] $ lookup v g

paths :: Eq a => Graph a -> a -> a -> [[a]]
paths g start end = go [start]
  where
    go [] = error "paths: internal error"
    go vs@(v:_)
        | v == end  = [reverse vs]
        | otherwise = concatMap (\n -> go (n:vs)) (adjacent g v \\ vs)

{-
Depth-First Search

Write a function

dfs :: Eq a => Graph a -> a -> [a]

that takes a graph and the id of a start vertex v, and returns a list of all vertices that are reachable from v.
Use a depth-first search, and return the list of vertices in the order in which they were discovered.
-}

dfs :: (Show a, Eq a) => Graph a -> a -> [a]
dfs g = reverse . go []
  where
    go visited v
        | v `elem` visited = visited
        | otherwise        = foldl go (v:visited) (adjacent g v)

{-
Breadth-First Search

Write a function that takes a graph and the ids of start and end vertices v and w, and returns a list of vertices on the shortest path from v to w,
or Nothing if there is no such path. Use a breadth-first search.
-}

path :: Eq a => Graph a -> a -> a -> Maybe [a]
path g start end = go [start] [[start]]
  where
    go visited (path@(v:_):rest)
        | v == end  = Just . reverse $ path
        | otherwise = go (next ++ visited) (rest ++ map (:path) next)
      where
        next = adjacent g v \\ visited
    go _ _ = Nothing


{-
State Space Search

Consider this type defining a state space:

type StateSpace s a = (s -> [a], s -> a -> s)

In the type declaration above, s is a state type and a is an action type. The first function returns a list of possible actions in any state.
The second function takes a state S and an action A, and returns a state that results from performing the action A in S.

Write a function

solve :: Eq s => StateSpace s a -> s -> (s -> Bool) -> Maybe [a]

that takes a state space, a start state and a function that determines whether a given state is a goal. The function should find the shortest path
from the start state to any goal state, and should return a list of actions along that path. If no goal state can be reached, return Nothing.
-}

type StateSpace s a = (s -> [a], s -> a -> s)

solve :: Eq s => StateSpace s a -> s -> (s -> Bool) -> Maybe [a]
solve (actions, perform) start isEnd = go [start] [(start, [])]
  where
    go visited ((state, path):rest)
        | isEnd state = Just . reverse $ path
        | otherwise   = go (map fst next ++ visited) (rest ++ next)
      where
        next = [(s, a:path) | a <- actions state, let s = perform state a, s `notElem` visited]
    go _ _ = Nothing

{-
Missionaries and Cannibals

Three missionaries and three cannibals wish to cross a river using a boat that can carry only two people.
At no time may the cannibals outnumber the missionaries on either river bank, since then they would eat the
missionaries. How can they cross? Write a Haskell program that can find the shortest solution.
-}

-- misL, canL, boatL, misR, canR
type RiverState = ((Int, Int), Bool, (Int, Int))
-- mis, can
type RiverAction = (Int, Int)

riverStateSpace :: StateSpace RiverState RiverAction
riverStateSpace = (actions, perform)
  where
    safe (m, c) = m >= c || m == 0

    move (m1, c1) (m2, c2) = do
        m <- [0 .. m1]
        c <- [0 .. c1]
        guard $ m + c `elem` [1, 2] && safe (m1 - m, c1 - c) && safe (m2 + m, c2 + c)
        pure (m, c)

    actions (l, boat, r) = if boat then move l r else move r l

    perform (l, boat, r) a = (lOp l a, not boat, rOp r a)
      where
        [lOp, rOp] = if boat then [sub, add] else [add, sub]

        add (a, b) (c, d) = (a + c, b + d)
        sub (a, b) (c, d) = (a - c, b - d)

riverSolution :: Maybe [RiverAction]
riverSolution = solve riverStateSpace ((3, 3), True, (0, 0)) isEnd
  where
    isEnd (_, _, (3, 3)) = True
    isEnd _              = False

{-
Bridge and Torch

Four people wish to cross a river using a narrow bridge, which can hold only two people at a time.
They have one torch and, because it's night, the bridge crossers must take the torch.

Person A can cross the bridge in 1 minute, B in 2 minutes, C in 5 minutes, and D in 8 minutes. When two people cross the bridge
together, they must move at the slower person's pace. Can they all get across the bridge if the torch lasts only 15 minutes?
Write a Haskell program that can determine the answer.
-}

-- Position of each person, position of the torch, time
type BridgeState = ([Bool], Bool, Int)

-- Which one or two people are moving
type BridgeAction = [Bool]

bridgeStateSpace :: StateSpace BridgeState BridgeAction
bridgeStateSpace = (actions, perform)
  where
    actions (people, torch, _) =
        filter (\m -> sum (map fromEnum m) `elem` [1, 2]) $ mapM (\p -> [False .. p == torch]) people

    perform (people, torch, time) move =
        (zipWith (/=) people move, not torch, time + maximum (zipWith (*) [1, 2, 5, 8] (map fromEnum move)))

bridgeSolution :: Maybe [BridgeAction]
bridgeSolution = solve bridgeStateSpace (replicate 4 False, False, 0) isEnd
  where
    isEnd (people, _, t) = and people && t <= 15
