{-# LANGUAGE BangPatterns #-}
--Exercise 11.1:

foldBool :: a -> a -> Bool -> a
foldBool true false True = true
foldBool true false False = false

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

foldDay :: a -> a -> a -> a -> a -> a -> a -> Day -> a
foldDay sunday monday tuesday wednesday thursday friday saturday Sunday = sunday  
foldDay sunday monday tuesday wednesday thursday friday saturday Monday = monday
foldDay sunday monday tuesday wednesday thursday friday saturday Tuesday = tuesday
foldDay sunday monday tuesday wednesday thursday friday saturday Wednesday = wednesday
foldDay sunday monday tuesday wednesday thursday friday saturday Thursday = thursday
foldDay sunday monday tuesday wednesday thursday friday saturday Friday = friday
foldDay sunday monday tuesday wednesday thursday friday saturday Saturday = saturday

--Exercise 11.2: <= corresponds to logical implication (A -> B)

--Exercise 11.3:

data Set a = Empty | Singleton a | Union (Set a) (Set a)

foldSet :: a -> (b -> a) -> (a -> a -> a) -> Set b -> a
foldSet empty singleton union = f
  where f (Empty)       = empty
        f (Singleton x) = singleton x
        f (Union l r)   = union (f l) (f r)

isIn :: Eq a => a -> Set a -> Bool
isIn x = foldSet False (==x) (||)

subset :: Eq a => Set a -> Set a -> Bool
subset s1 = foldSet False (flip(isIn) s1) (&&)

instance Eq a => Eq (Set a) where
  xs == ys = (xs `subset` ys) && (ys `subset` xs)

--Exercise 11.4:

data BTree a = Leaf a | Fork (BTree a) (BTree a) 
data Direction = L | R deriving Show
type Path = [ Direction ]

foldBTree :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldBTree leaf fork = f
  where f (Leaf x) = leaf x
        f (Fork l r) = fork (f l) (f r)

--Not sure how to use fmap within the find function
instance Functor BTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

find :: Eq a => a -> BTree a -> Maybe Path
find x = foldBTree l f 
  where l y = if y==x then (Just []) else Nothing
        f (Just z) _ = Just (L:z)
        f _ (Just z) = Just (R:z)
        f _ _ = Nothing

--Exercise 12.1

data Queue a = Queue [a]

empty = Queue []

-- O(1)
isEmpty :: Eq a => Queue a -> Bool
isEmpty (Queue xs) = (xs == [])

-- O(n^2)
add :: a -> Queue a -> Queue a
add x (Queue xs) = Queue (xs ++ [x])

-- O(1)
get :: Queue a -> (a, Queue a)
get (Queue (x:xs)) = (x, (Queue xs))

--Representing the queue by a list of its elements in reverse order would make add O(1) but make get O(n^2) 

data Queue' a = Queue' [a] [a]

empty' = Queue' [] []

--O(1)
isEmpty' :: Eq a => Queue' a -> Bool
isEmpty' (Queue' xs ys) = (xs == []) && (ys == [])

--O(1)
add' :: a -> Queue' a -> Queue' a
add' x (Queue' xs ys) = Queue' xs (x:ys)

--O(n) if x == [] otherwise O(1)
get' :: Queue' a -> (a, Queue' a)
get' (Queue' (x:xs) ys) = (x, (Queue' xs ys))
get' (Queue' [] ys) = ((last ys), Queue' [] (init ys))

--Exercise 12.2

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--fib 10 = 55, fib 20 = 6765, fib 30 = 832040. Later calls have to calculate more fibonacci numbers before calculating the given one

two 0 = (0,1)
two n = (y, x + y) where (x,y) = two (n-1)

roughly :: Integer -> String
roughly n = x : 'e' : show (length xs) where x:xs = show n

-- *Main> roughly (fst(two 10000))
-- "3e2089"

--The top left entry will be equal to the top right entry of the previous n. The top right entry will be equal to the sum of the top left and top right entries of the previous n.
--The bottom left entry will be equal to the bottom right entry of the previous n and the bottom right entry will be the sum of the bottom left and bottom right entrys of the previous n.
--Since F = [fib 0, fib 1], [fib 1, fib 2]] continuing in the pattern described above gives the desired F^n. Can be proved formally by induction

type Matrix a = [[a]]

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

cols (xs:xss) = foldr g (repeat []) (xs:xss) where g xs xss = zipWith (:) xs xss

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul xss yss = [[dot xs zs | zs <- cols yss] | xs <- xss]

power (mul) y x n --x^n*y
      | n == 0 = y
      | even n = power mul y (x `mul` x) (n `div` 2)
      | odd n = power mul (x `mul` y) x (n-1)

f = [[0, 1],[1, 1]] -- F^1
y = [[1, 0], [0, 1]] --Identity matrix

-- *Main> roughly (((power mul y f 1000000)!!0)!!1)
-- "1e208987"

--Exercise 12.3

test f = f (const error) () ["strict", "lazy"]

loop s n [] = n
loop s n (x:xs) = loop s (s n x) xs

loop' s n [] = []
loop' s (!n) (x:xs) = loop' s (s n x) xs

--Prediction: test loop = Exception: lazy, test loop' = Exception: strict

--test loop returns a lazy exception while test loop' returns a strict exception
--loop is lazy beacuse it builds up the whole expression before evaluation, while loop' is strict because !n means n must be evaluated before it is used.
--test foldl returns lazy 












