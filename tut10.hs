import Data.Char

{-
Valid Search Tree

Consider this definition of a binary tree:

data Tree a = Nil | Node (Tree a) a (Tree a)

Write a function isSearchTree that determines whether a binary tree is a valid binary search tree:

isSearchTree :: Ord a => Tree a -> Bool

Assume that duplicate values are not allowed in the tree.
-}

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

-- We could use Nothing to represent +/- infinity but this is
-- much more convenient.
--
-- For many type definitions, Haskell can automatically derive
-- the correct implementation of (==) and compare, so we do not
-- need to write those manually.
--
-- The behavior of (==) is obvious. For compare, it uses the
-- order in which the constructors are written: leftmost being
-- the smallest.
data Extended a = NegInf | Fin a | PosInf deriving (Eq, Ord)

isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = go NegInf PosInf
  where
    -- We keep track of the interval of allowed values, (lo .. hi)
    go _  _  Nil          = True
    go lo hi (Node l x r) = lo < x' && x' < hi && go lo x' l && go x' hi r
      where
        x' = Fin x

-- Same idea as before, but using functions to represent the interval bounds.
-- The first function checks for the lower bound, the second for the upper bound.
--
-- For example, the interval (a .. b) would be represented as a pair of functions
-- ((> a), (< b)). A value is within the interval if both functions evaluate return
-- True.
isSearchTree' :: Ord a => Tree a -> Bool
isSearchTree' = go (const True) (const True)
  where
    go _  _  Nil          = True
    go lo hi (Node l x r) = lo x && hi x && go lo (< x) l && go (> x) hi r

{-
Tree Fold

Write a function treeFoldr that performs a right fold of a function over the values in a binary tree:

treeFoldr :: (b -> a -> a) -> a -> Tree b -> a

Use treeFoldr to write functions that

    add all the values in a binary tree of numbers;

    generate a list containing all the values in a binary tree

-}

treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr f = go
  where
    go v Nil          = v
    -- The initial value 'v' needs to end up on the right;
    -- process it with the right subtree. Then apply 'f' with
    -- the middle element and finally go to the left.
    go v (Node l x r) = go (f x (go v r)) l

sumTree :: Num a => Tree a -> a
sumTree = treeFoldr (+) 0

treeToList :: Tree a -> [a]
treeToList = treeFoldr (:) []

-- Yet another version of isSearchTree. The pair contains the
-- right-most processed element (empty at the start) and info
-- whether the tree is a BST.
isSearchTree'' :: Ord a => Tree a -> Bool
isSearchTree'' = snd . treeFoldr handle (Nothing, True)
  where
    handle v (Nothing,   ok) = (Just v, ok)  -- Nothing to do
    handle v (Just next, ok) = (Just v, v < next && ok)  -- Make sure the next element is strictly greater

{-
Tree Insertion

Write a function that can insert a value into a binary search tree. Choose an appropriate type for your function.

Using the function you wrote in part (a), write a function that inserts all values in a given list into a binary search tree.
-}

insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = Node Nil x Nil
insert x (Node l y r) = case x `compare` y of
    LT -> Node (insert x l) y r
    EQ -> Node l x r
    GT -> Node l y (insert x r)

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr insert Nil

{-
Balanced Tree

We will say that a binary tree is balanced if for every node in the tree, the left and right
subtrees differ in height by at most 1. Write a function that takes a binary tree and returns
True iff it is balanced.
-}

isBalanced :: Tree a -> Bool
isBalanced = snd . go
  where
    -- go returns a pair containing the height of the tree and
    -- whether it's balanced.
    go Nil          = (-1 :: Int, True)  -- Silence a warning
    go (Node l _ r) = (1 + max lh rh, abs (lh - rh) <= 1 && lb && rb)
      where
        (lh, lb) = go l
        (rh, rb) = go r

{-
Infinite Tree

Construct an infinite binary tree of type Tree Integer that looks like this:

       1
     /   \
    2     3
   / \   / \
  4   5 6   7
  .   . .   .
  .   . .   .
-}

infiniteTree :: Tree Integer
infiniteTree = go 1
  where
    go n = Node (go (n * 2)) n (go (n * 2 + 1))

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _            = Nil
takeTree _ Nil          = Nil
takeTree n (Node l x r) = Node (takeTree (n - 1) l) x (takeTree (n - 1) r)

-- >>> takeTree 3 infiniteTree
-- Node (Node (Node Nil 4 Nil) 2 (Node Nil 5 Nil)) 1 (Node (Node Nil 6 Nil) 3 (Node Nil 7 Nil))

{-
Prefix Expressions

We can use this an algebraic data type to represent an arithmetic expression:

data Expr = Const Int | Var String |
            Add Expr Expr | Sub Expr Expr | Mul Expr Expr

a) Write a function parse :: String -> Expr that can parse an expression written in prefix notation, such as

* + x y - x y
* z - z 2
14
+ 0 + 1 + 2 3

You may assume that (as in the examples above) all input tokens are separated by spaces.

b) Write a function eval :: [(String, Int)] -> Expr -> Int that evaluates an expression. The first argument
to the function is an environment, i.e. an association list that maps variables to their values.
-}

data Expr
    = Const Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    deriving (Show)

parse :: String -> Expr
parse = fst . go . words
    -- Strictly speaking, we should check that all tokens
    -- were used.
  where
    ops = [("+", Add), ("-", Sub), ("*", Mul)]

    -- go takes all the remaining tokens and returns
    -- an expression and the unused tokens.
    go []     = error "parse: invalid expression"
    go (w:ws) = case lookup w ops of
        Just op -> binary op ws
        Nothing
            | all isDigit w -> (Const (read w), ws)  -- TODO: Handle negative numbers
            | otherwise     -> (Var w, ws)

    binary op ws = (op l r, ws'')
      where
        (l, ws' ) = go ws
        (r, ws'') = go ws'

eval :: [(String, Int)] -> Expr -> Int
eval _   (Const c) = c
eval env (Var v)   = case lookup v env of
    Just val -> val
    Nothing  -> error "eval: unbound variable"
eval env (Add l r) = eval env l + eval env r
eval env (Sub l r) = eval env l - eval env r
eval env (Mul l r) = eval env l * eval env r
