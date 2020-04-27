module Examples where 
-----------------------------
-- examples from Chapter 8 --
-----------------------------

-----------------------------
-- Making your own TYPES ::
-- these use existing types and simply shortcut/rename them for convenience and efficiency of use
-----------------------------

-- -- this could be a positional type
-- -- commented out here so it can be actually declared next to Move, which uses it :)
-- type Pos = (Int, Int)
-- and this could be a function that transforms a position
type Trans = Pos -> Pos
-- here we can define our own 'association' type between 'key' and 'value' pairs, a type of lookup tables that associate keys of one type to values of another type.
type Assoc k v = [(k, v)]
-- now we can use our Assoc type to make a function that returns the first value for a given key in a table
-- k must support Equality
-- function find takes in k and an Assoc (our k v association table), and returns v
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']


-----------------------------
-- Making your own completely new types :: DATA ::
-- data Move and type Pos
-----------------------------

-- define our own type called Move
data Move = North | South | East | West
    deriving Show

-- our shortcut name for a position
type Pos = (Int, Int)

-- move in Move direction
-- usage move [North|South|East|West] Pos
-- using our 'shortcut' type named Pos for position
move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)

-- apply a list of Moves to a Pos
moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

-- reverse a move
rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East


-----------------------------
-- Making your own completely new types :: DATA ::
-- data Shape
-----------------------------
-- because Circle and Rect use arguments, they are actually CONSTRUCTOR FUNCTIONS
    -- which produce results of type Shape from arguments of type Float.
-- the difference between CONSTRUCTOR functions and NORMAL functions, is that
    -- CONSTRUCTOR functions have no defining equations, they exist purely to build data
    -- NORMAL functions evaluate to something :: negate 1.0 becomes -1.0
    -- CONSTRUCTOR functions don't evaluate :: Circle 1.0 is already fully evaluated!
        -- the expression Circle 1.0 is just a piece of data, like 1.0 itself is data
        -- cannot be simplified because there are no defining equations for Circle
data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


-----------------------------
-- Using Maybe
--  data type defined in the standard prelude as
--      data Maybe a = Nothing | Just a
-----------------------------

-- create a safe division method
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

-- create a safe head method
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- using Maybe is better than using 'error', because 'error' can't be caught, your program just 'panics' and 'shuts down'
-- whereas if you use Maybe then you can actually work with 'catching' the proper cases in your code and moving on appropriately


-----------------------------
-- newtype
-- if you have need of a type and it can have ONLY ONE constructor with ONE argument, it can be declared using newtype
-- Using newtype helps improve type safety without affecting performance
--      newtype rather than type means that
--          Haskell recognizes it as a separate type, not just an alias (as with 'type')
--          this means we can type check against it
--          it makes for more type-safe code
--      newtype rather than data gives an efficiency benefit
--          newtype constructors do not incur any cost when program is running
--          they are automatically removed by the compiler once type checking is complete
-----------------------------
-- newtype Nat = N Int


-----------------------------
-- RECURSIVE TYPES
--      the Natural number type from previous example can be defined recursively
--      data and newtype can be done recursively, but not type
-----------------------------

--------------------------------------
-- Nat :: our own Natural Number type
--------------------------------------

data Nat = Zero | Succ Nat

-- add by using recursion and our data definition Succ
add_recursion :: Nat -> Nat -> Nat
add_recursion Zero n      = n
add_recursion (Succ m) n  = Succ (add_recursion m n)

-- add by using translation functions between integers and our natural numbers
nat2int :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0       = Zero
int2nat n       = Succ (int2nat (n-1))

add_conversion :: Nat -> Nat -> Nat
add_conversion m n = int2nat (nat2int m + nat2int n)

--------------------------------------
-- List :: our own List type
--------------------------------------
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil             = 0
len (Cons _ xs)     = 1 + len xs


-------------------------------------------------
-- Tree :: our own Node-and-Leaf BINARY Tree type
-- meaning that at each branching we can have two branches only
-------------------------------------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a)
                deriving Show

-- here, we define a simple tree. hard-coded.
t :: Tree Int
t = Node    (Node   (Leaf 1)
                    3
                    (Leaf 4))
            5
            (Node   (Leaf 6)
                    7
                    (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)        = [x]
flatten (Node l x r)    = flatten l ++ [x] ++ flatten r
-- if applying this function to a tree gives a sorted list, then the tree is called a 'search tree'
-- applying this function to our previous tree above ('t') does give a sorted list.

-- a search tree has the property where ::
--  if you're searching for a value, you can compare it to the value of the current node
--      if it's less than the current node, then the value, if it exists in the tree, will be in the left branch
--      if it's greater than the current node, then the value, if it exists in the tree, will be in the right branch
occurs_in_search_tree :: Ord a => a -> Tree a -> Bool
occurs_in_search_tree x (Leaf y)        = x == y
occurs_in_search_tree x (Node l y r)    | x == y    = True
                                        | x < y     = occurs_in_search_tree x l
                                        | otherwise = occurs_in_search_tree x r
-- this is a more efficient definition because it will not search the WHOLE tree for a value.
