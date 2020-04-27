module Tautology where

-- this implements a Tautology Checker as explained in Graham Hutton's text,
-- Chapter 8, Section 6
-- so cool!

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            deriving Show

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool


--------------------
-- a function which looks up and returns the FIRST VALUE for a GIVEN KEY
-- within an association (called t for association 'table')
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

--------------------
-- a function which evaluates a proposition to a Boolean value
-- effectively, this function GIVES MEANING to our 'data Prop' defined above.
-- without it, 'data Prop' is just a bunch of Bools and Chars
    -- note that in Prop we defined Const as a Bool [the base case, system-defined type]
    -- so when you hit the word Const, you return the Boolean value of that Const.
    -- note that in Prop we also defined Var as a Char [another base case, system-defined type]
eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
-- if you hit a Const, just RETURN its BOOLEAN value
eval s (Var x)      = find x s
-- if you come down to a Var, look up its boolean value and RETURNs the BOOLEAN
eval s (Not p)      = not (eval s p)
-- Not means not of the evaluated expression (booleans)
eval s (And p q)    = eval s p && eval s q
-- And means && of the evaluated sub-expressions (booleans)
eval s (Imply p q)  = eval s p <= eval s q
-- Imply is literally the <= logic, applied to evaluated sub-expressions (booleans)

--------------------
-- a function which returns a list of all the variables in a proposition
vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var x)        = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q

--------------------
-- a function which generates all combinations of Int number of boolean values
-- basically, it is a binary counting function
bools :: Int -> [[Bool]]
bools 0     = [[]]
bools n     = map (False:) bss ++ map (True:) bss
                where bss = bools (n-1)
-- note: the reason it uses 'map' is because 'bss' is a list of lists (hence the double s)
-- and to get that False: in front of each element (which is a list), we use 'map',
-- which takes that 'function' (False:) or (True:) and yes it's a function because of currying
-- and then applies it (maps it) into / onto each element of bss.
-- EACH ELEMENT of bss is a LIST. and this works, because (x:) pre-pends x onto a LIST.

--------------------
-- a function which removes duplicates in a list
-- filter is defined as
-- filter :: (a -> Bool) -> [a] -> [a]
-- aka filter (predicate) list RETURNS filtered_list
rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : filter (/= x) (rmdups xs)

--------------------
-- a function which GENERATES all possible substitutions for a given proposition
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)
-- it *maps* the zipping function onto each element of bools (length vs)!...
-- so that you get the full set of bools and then you zip the vars onto EACH ELEMENT
-- each element has length equal to the number of variables
-- however, there are many such elements.
-- wow.

--------------------
-- a function which EVALUATES all possible substitutions for a given proposition
-- 'and' checks if ALL OF THESE evaluate to TRUE, e.g. if it is a Tautology
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


------------
-- examples
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
