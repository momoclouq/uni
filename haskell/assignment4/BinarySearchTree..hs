-- Exercise 1 - week 4 


module BinarySearchTree
where
import BinaryTree  -- hiding (member)
import QuickTest
-- 1.
--data Tree elem = Empty | Node (Tree elem) elem (Tree elem) deriving (Show)

-- 2.
t0 :: Tree Char
t0 = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty)) 

t1 :: Tree Int
t1 = Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))

t2 :: Tree String
t2 = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

t3 :: Tree Char
t3 = Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

-- 3.
size :: Tree elem -> Int
size Empty = 0
size (Node l o r) = size l + 1 + size r

-- 4.
minHeight :: Tree elem -> Int
minHeight Empty = 0
minHeight (Node l o r) | minHeight l <= minHeight r = 1 + minHeight l
                       | otherwise = 1 + minHeight r

maxHeight :: Tree elem -> Int 
maxHeight Empty = 0
maxHeight (Node l o r) | maxHeight l <= maxHeight r = 1 + maxHeight r
                       | otherwise = 1 + maxHeight l


-- 5. relation: ????
 
-- 6. 
member :: (Ord elem) => elem -> Tree elem -> Bool
member _ Empty = False
member x (Node l o r) = x == o || member x l  || member x r

----------------
-- Exercise 2

-- 1.
preorder :: Tree elem -> [elem]
preorder Empty = []
preorder (Node l o r) = o : preorder l ++ preorder r

postorder :: Tree elem -> [elem]
postorder Empty = []
postorder (Node l o r) = postorder l ++ postorder r ++ [o]

inorder :: Tree elem -> [elem]
inorder Empty = []
inorder (Node l o r) = inorder l ++ [o] ++ inorder r

-- 2. 
-- layout :: (Show elem) => Tree elem -> String

----------------
-- Exercise 3
-- 1.
split :: [elem] -> ([elem],[elem])
split [] = ([],[])
split a = (take (length a `div` 2) a, drop (length a `div` 2) a)

-- wrong
build :: [elem] -> Tree elem
build [a] = Node Empty a Empty
build a = Node (build (fst (split a))) (head (reverse (fst (split a)))) (build (snd (split a)))
 

-- 2.
--wrong
balanced :: [elem] -> Tree elem
balanced [] = Empty
balanced [a] = Node Empty a Empty
balanced a = Node (balanced (fst  (split (reverse(tail (reverse a)))))) (head (reverse (fst (split a)))) (balanced( snd (split (reverse(tail (reverse a))))))


---------------
-- Exercise 4
registry :: Tree String
registry = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
-- 1.
member1 :: (Ord elem) => elem -> Tree elem -> Bool
member1 a Empty = False
member1 a (Node l o r) | a == o = True
                      | a > o = member1 a r
                      | otherwise = member1 a l

-- 2.
insert :: (Ord elem) => elem -> Tree elem -> Tree elem
insert a Empty = Node Empty a Empty
insert a (Node l o r) | a >= o = Node l o (insert a r)
                      | otherwise = Node (insert a l) o r

-- 3.
getvalues :: Tree elem -> [elem]
getvalues Empty = []
getvalues (Node l o r) = [o] ++ getvalues r ++ getvalues l

inserts :: (Ord elem) => [elem] -> Tree elem -> Tree elem
inserts [] a = a
inserts (x:xs) a = inserts xs (insert x a)

delete :: (Ord elem) => elem -> Tree elem -> Tree elem
delete _ Empty = Empty
delete a (Node l o r) | a > o = Node l o (delete a r)
                      | a < o = Node (delete a l) o r
                      | otherwise = inserts k l   
                            where k = getvalues r

-- 4. drop


-- isSearchTree :: (Ord elem) => Tree elem -> Bool
-- trees :: [elem] -> Probes (Tree elem)  -- should be defined in BinaryTree
