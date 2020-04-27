module Exercises where

------------------------------
-- exercises from Chapter 8 --
------------------------------
-- exercises 1, 2, 3 only
------------------------------

-- exercise 1
-- in a similar manner to the function 'add', define a recursive multiplication function
-- mult :: Nat -> Nat -> Nat for the recursive type of natural numbers.
-- hint: make use of add in your definition
data Nat = Zero | Succ Nat
            deriving Show

-- add by using recursion and our data definition Succ
add :: Nat -> Nat -> Nat
add Zero n      = n
add (Succ m) n  = Succ (add m n)

-- multiply recursively, using add
-- omg! it worked!! ;-) :-o :-)
mult :: Nat -> Nat -> Nat
mult _ Zero             = Zero
mult Zero _             = Zero
mult (Succ m) n         = add (mult m n) n
-- doing it this way matches the order in the 'add' function (add Zero n = n)
-- and catches the 'one case' for multiplying more efficiently

-- originally, after the mult _ Zero | Zero _ cases, I had >>
-- mult (Succ (Zero)) n    = add Zero n
-- mult n (Succ (Zero))    = add Zero n
-- mult (Succ m) n         = add (mult m n) n


nat0 = Zero
nat1 = Succ (nat0)
nat2 = Succ (nat1)
nat3 = Succ (nat2)
nat4 = Succ (nat3)
nat5 = Succ (nat4)

nat_to_int :: Nat -> Int
nat_to_int Zero        = 0
nat_to_int (Succ n)    = 1 + nat_to_int n

int_to_nat :: Int -> Nat
int_to_nat 0       = Zero
int_to_nat n       = Succ (int_to_nat (n-1))


-- exercise 2
-- the standard Prelude defines ::
    -- data Ordering = LT | EQ | GT
    -- compare :: Ord a => a -> a -> Ordering
    -- compare decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value
-- use compare to redefine occurs :: Ord a => a -> Tree a -> Bool for SEARCH TREES
    -- a SEARCH TREE has the property where ::
    --  if you're searching for a value, you can compare it to the value of the current node
    --      if it's less than the current node, then the value, if it exists in the tree, will be in the left branch
    --      if it's greater than the current node, then the value, if it exists in the tree, will be in the right branch

-- data Tree a = Leaf a | Node (Tree a) a (Tree a)
--                 deriving Show
--
-- -- -- the original 'occurs' for a search tree
-- -- occurs_in_search_tree :: Ord a => a -> Tree a -> Bool
-- -- occurs_in_search_tree x (Leaf y)        = x == y
-- -- occurs_in_search_tree x (Node l y r)    | x == y    = True
-- --                                         | x < y     = occurs_in_search_tree x l
-- --                                         | otherwise = occurs_in_search_tree x r
--
-- -- the new 'occurs', which uses Ord's compare method
-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y)       = x == y
-- occurs x (Node l y r)   = case (compare x y) of
--                             LT -> occurs x l
--                             EQ -> True
--                             GT -> occurs x r
-- -- Why is this new definition more efficient than the original?
--     -- I think this is more efficient because you stay within the base type 'Ord'
--     -- and you don't go making any calls to the '==' or etc methods defined on x's type
--     -- you literally stay in the Ord type the whole time. you don't use any other methods.
--     -- seems weird, maybe, but that's all I've got!...
--
-- -- here, we define a simple tree. hard-coded.
-- t :: Tree Int
-- t = Node    (Node   (Leaf 1)
--                     3
--                     (Leaf 4))
--             5
--             (Node   (Leaf 6)
--                     7
--                     (Leaf 9))


-- exercise 3
-- given a binary tree type as defined below...
data Tree a = Leaf a | Node (Tree a) (Tree a)
                        deriving Show
-- let's say that a BinaryTree is 'balanced' if the number of leaves in the L and R subtrees of each node differ by at most one.
-- leaves themselves are 'trivially balanced'
-- define a function with the signature given below which determines if a BinaryTree is balanced or not.

b1 = Node   (Node   (Node   (Leaf 0)
                            (Leaf 1))
                    (Node   (Leaf 2)
                            (Leaf 3)))
            (Node   (Node   (Leaf 4)
                            (Leaf 5))
                    (Node   (Leaf 6)
                            (Leaf 7)))

b2 = Node   (Node   (Leaf 'a')
                    (Leaf 'b'))
            (Node   (Node   (Leaf 'c')
                            (Leaf 'd'))
                    (Leaf 'e'))

b3 = Node   (Node   (Node   (Leaf "Ksenia")
                            (Leaf "Rose"))
                    (Leaf "Brian"))
            (Node   (Node   (Node   (Leaf "Amy")
                                    (Leaf "Bobby"))
                            (Node   (Leaf "Carly")
                                    (Leaf "Dave")))
                    (Leaf "Egg"))

b4 = Node   (Node   (Node   (Leaf 'a')
                            (Leaf 'b'))
                    (Node   (Node   (Leaf 'c')
                                    (Leaf 'd'))
                            (Leaf 'e')))
            (Node   (Node   (Node   (Leaf 'a')
                                    (Leaf 'b'))
                            (Node   (Leaf 'c')
                                    (Leaf 'd')))
                    (Leaf 'e'))

num_leaves :: Tree a -> Int
num_leaves (Leaf x)     = 1
num_leaves (Node l r)   = num_leaves l + num_leaves r

balanced :: Tree a -> Bool
balanced (Leaf x)   = True
balanced (Node l r) = abs (num_leaves l - num_leaves r) <= 1 && balanced l && balanced r

-- this initial implementation isn't complete, it only checks the first two levels against each other,
-- but doesn't ensure that each sublevel is also balanced!
-- the above implementation uses recursion to ensure that each sublevel is balanced as well
-- balanced :: Tree a -> Bool
-- balanced (Node l r)   = case (compare (num_leaves l) (num_leaves r)) of
--                         LT -> num_leaves r - num_leaves l == 1 || num_leaves r - num_leaves l == 0
--                         EQ -> True
--                         GT -> num_leaves l - num_leaves r == 1 || num_leaves l - num_leaves r == 0
