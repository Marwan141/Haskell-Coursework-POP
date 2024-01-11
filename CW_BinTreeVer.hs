{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module Coursework where

import Data.List
import qualified Data.Set as HS (empty, fromList, toList, null, singleton, insert, union, intersection, difference, member, size, map, foldr, delete, fromList, toList, Set )
import Test.QuickCheck

{-
  Your task is to design a datatype that represents the mathematical concept of
  a (finite) set of elements (of the same type). We have provided you with an
  interface (do not change this!) but you will need to design the datatype and
  also support the required functions over sets. Any functions you write should
  maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a
  list. Alternatively, one could use an algebraic data type, wrap a binary
  search tree, or even use a self-balancing binary search tree. Extra marks will
  be awarded for efficient implementations (a self-balancing tree will be more
  efficient than a linked list for example).

  You are **NOT** allowed to import anything from the standard library or other
  libraries. Your edit of this file should be completely self-contained.

  **DO NOT** change the type signatures of the functions below: if you do, we
  will not be able to test them and you will get 0% for that part. While sets
  are unordered collections, we have included the Ord constraint on some
  signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Everything must be in
  this file.

  See the note **ON MARKING** at the end of the file.
-}

{-
   PART 1.
   You need to define a Set datatype.
-}

-- you **MUST** change this to your own data type. The declaration of Set a =
-- Int is just to allow you to load the file into ghci without an error, it
-- cannot be used to represent a set.
data Set a = Empty | Node a (Set a) (Set a) deriving (Show, Ord)

height :: Set a -> Int
height Empty = -1
height (Node _ l r) = 1 + max(height l) (height r)

balanceValue :: Set a -> Int
balanceValue Empty = 0
balanceValue (Node _ l r) = (height l - height r)
 
-- balance the tree (this made me die inside)
balance :: Set a -> Set a
balance Empty = Empty
balance (Node x l r)
  | balanceValue (Node x l r) > 1 && balanceValue l >= 0 = rotateRight (Node x l r)
  | balanceValue (Node x l r) < -1 && balanceValue r <= 0 = rotateLeft (Node x l r)
  | balanceValue (Node x l r) > 1 && balanceValue l < 0 = rotateRight (Node x (rotateLeft l) r)
  | balanceValue (Node x l r) < -1 && balanceValue r > 0 = rotateLeft (Node x l (rotateRight r))
  | otherwise = Node x l r


rotateLeft :: Set a -> Set a
rotateLeft (Node x l (Node y m r)) = Node y (Node x l m) r
rotateLeft tree = tree

rotateRight :: Set a -> Set a
rotateRight (Node x (Node y l m) r) = Node y l (Node x m r)
rotateRight tree = tree




{-
   PART 2.
   If you do nothing else, you must get the toList, fromList and equality working. If they
   do not work properly, it is impossible to test your other functions, and you
   will fail the coursework!
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList Empty = []
toList (Node x left right) = quickSort (toList left ++ [x] ++ toList right)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(filter(<x) xs) ++ [x] ++ quickSort(filter(>=x) xs)

nub :: Ord a => [a] -> [a] -- if u want to test this use Coursework.nub
nub [] = []
nub (x:xs) = x : Coursework.nub (filter (/= x) xs)

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList x =
  balance (Node (xs !! mid) 
               (fromList (take mid xs)) 
               (fromList (drop (mid + 1) xs)))
               where
                 mid = length xs `div` 2
                 xs = quickSort (Coursework.nub x)

         

-- Make sure you satisfy this property. If it fails, then all of the functions
-- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1) == (toList s2)

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- the empty set
empty :: Set a
empty = Empty

-- is it the empty set?
null :: Set a -> Bool -- Write Coursework.null for use
null s = cardinality s == 0

-- build a one element Set
singleton :: a -> Set a -- Write Coursework.singleton for use
singleton x = Node x Empty Empty

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: Ord a => a -> Set a -> Set a -- Write Coursework.insert for use
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = balance(Node y (Coursework.insert x left) right)
  | x > y = balance(Node y left (Coursework.insert x right))
  | otherwise = Node y left right -- no change

auxToList :: Set a -> [a] -- (No Ord (: SHOULD ALWAYS BE WSORTED!! )
auxToList Empty = []
auxToList (Node x left right) = auxToList left ++ [x] ++ auxToList right

auxFromList :: [a] -> Set a -- List should already be sorted
auxFromList [] = Empty
auxFromList xs = let mid = length xs `div` 2
              in (Node (xs !! mid) 
               (auxFromList (take mid xs)) 
               (auxFromList (drop (mid + 1) xs)))  
         

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a -- Write Coursework.union for use
union Empty t2 = t2
union t1 Empty = t1
union (Node x l1 r1) t2 =
  let (lt2, gt2) = split x t2
  in join x (Coursework.union l1 lt2) (Coursework.union r1 gt2)

split :: Ord a => a -> Set a -> (Set a, Set a)
split x Empty = (Empty, Empty)
split x (Node y l r)
  | x < y     = let (lt, gt) = split x l in (lt, join y gt r) -- split left side of the tree
  | x > y     = let (lt, gt) = split x r in (join y l lt, gt) -- split right side of the tree
  | otherwise = (l, r)  

join :: Ord a => a -> Set a -> Set a -> Set a
join x l r = balance (Node x l r)

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a 
intersection Empty t2 = Empty
intersection t1 Empty = Empty
intersection (Node x l1 r1) t2 =
  let (lt2, gt2) = split x t2
  in if member x t2 then join x (intersection l1 lt2) (intersection r1 gt2) else Empty


-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference Empty t2 = Empty
difference t1 Empty = t1
difference (Node x l1 r1) t2 =
  let (lt2, gt2) = split x t2
  in if member x t2 then difference (difference l1 lt2) (difference r1 gt2) else join x (difference l1 lt2) (difference r1 gt2)

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Node y left right)
  | x < y = member x left
  | x > y = member x right
  | otherwise = True

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality Empty = 0
cardinality (Node _ l r) = 1 + cardinality l + cardinality r

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ Empty = Empty
setmap f (Node x left right) = Node (f x) (setmap f left) (setmap f right)

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> Set a -> b -> b -- what even is foldr for a set
setfoldr f s acc = foldr f acc (auxToList s)

-- remove an element *x* from the set-- return the set unaltered if *x* is not present
removeSet :: (Eq a) => a -> Set a -> Set a
removeSet x s = balance (auxFromList [d | d <- auxToList s, d /= x])

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet s = auxFromList(map auxFromList (subsequences (auxToList s))) 

-- I tried my best pls have mercy

{-
   ON MARKING:

   Be careful! This coursework will be marked using QuickCheck, against
   Haskell's own Data.Set implementation. This testing will be conducted
   automatically via a marking script that tests for equivalence between your
   output and Data.Set's output. There is no room for discussion, a failing test
   means that your function does not work properly: you do not know better than
   QuickCheck and Data.Set! Even one failing test means 0 marks for that
   function. Changing the interface by renaming functions, deleting functions,
   or changing the type of a function will cause the script to fail to load in
   the test harness. This requires manual adjustment by a TA: each manual
   adjustment will lose 10% from your score. If you do not want to/cannot
   implement a function, leave it as it is in the file (with undefined).

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough
   for a passing mark of 40%, as long as both toList and fromList satisfy the
   toFromListProp function.

   The maximum mark for those who use Haskell lists to represent a Set is 70%.
   To achieve a higher grade than is, one must write a more efficient
   implementation. 100% is reserved for those brave few who write their own
   self-balancing binary tree.
-}
