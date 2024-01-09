{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module Coursework where

import Data.List
import qualified Data.Set as HS (fromList, toList, union, intersection, difference, empty, singleton, member, null)
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

tree1 :: Set Int
tree1 = fromList [1..1000]

tree2 :: Set Int
tree2 = fromList [999..5000]

height :: Set a -> Int
height Empty = -1
height (Node _ l r) = 1 + max(height l) (height r)

balanceValue :: Set a -> Int
balanceValue Empty = 0
balanceValue (Node _ l r) = (height l - height r)
 
-- balance the tree
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

numOfElements :: Set a -> Int
numOfElements Empty = 0
numOfElements (Node _ l r) = 1 + numOfElements l + numOfElements r

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
toList (Node x left right) = toList left ++ [x] ++ toList right

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(filter(<x) xs) ++ [x] ++ quickSort(filter(>=x) xs)

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList = foldl (flip Coursework.insert) Empty

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
null :: Set a -> Bool
null s = cardinality s == 0

-- build a one element Set
singleton :: a -> Set a
singleton x = Node x Empty Empty

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = balance(Node y (Coursework.insert x left) right)
  | x > y = balance(Node y left (Coursework.insert x right))
  | otherwise = Node y left right -- no change

auxToList :: Set a -> [a] -- (No Ord (: )
auxToList Empty = []
auxToList (Node x left right) = auxToList left ++ [x] ++ auxToList right

auxFromList :: [a] -> Set a -- List should already be sorted
auxFromList [] = Empty
auxFromList (x:xs) = Node x (auxFromList (take (length xs `div` 2) xs)) (auxFromList (drop (length xs `div` 2) xs)) -- take the middle element as the root and then recursively build the tree

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union Empty t2 = t2
union t1 Empty = t1
union (Node x l1 r1) t2 =
  let (lt2, gt2) = split x t2
  in join x (Coursework.union l1 lt2) (Coursework.union r1 gt2)

split :: Ord a => a -> Set a -> (Set a, Set a)
split x Empty = (Empty, Empty)
split x (Node y l r)
  | x < y     = let (lt, gt) = split x l in (lt, join y gt r)
  | x > y     = let (lt, gt) = split x r in (join y l lt, gt)
  | otherwise = (l, r)  -- x == y, so don't include y in either lt or gt

join :: Ord a => a -> Set a -> Set a -> Set a
join x l r = balance (Node x l r)

unionProp :: IO ()
unionProp =
  quickCheck ((\xs ys -> (toList . Coursework.union (fromList xs) . fromList $ ys) == quickSort (nub (xs ++ ys))) :: [Int] -> [Int] -> Bool)
-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = auxFromList([x | x <- toList s1, x `elem` toList s2])

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = auxFromList([x | x <- toList s1, x `notElem` toList s2])

differenceProp :: IO ()
differenceProp =
  quickCheck
    ((\xs -> HS.toList (HS.difference (HS.fromList xs) (HS.fromList xs)) == []) :: [Int] -> Bool)

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Node y left right)
  | x < y = member x left
  | x > y = member x right
  | otherwise = True

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality = numOfElements

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f s = fromList(map f (auxToList s))

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f s acc = foldr f acc (auxToList s)

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Eq a) => a -> Set a -> Set a
removeSet x s = balance (auxFromList [d | d <- auxToList s, d /= x])

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet s = auxFromList(map auxFromList (subsequences (auxToList s)))

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
