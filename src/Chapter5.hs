module Chapter5 where

import Data.Char

{- 5.1 List generators -}
c0 = [x^2 | x <- [1..5]]
-- Translates as
-- generate a list by calculating the power of x x^2 "such that" x "is drawn from"

c1 = [(x,y) | x <- [1..3], y <- [4,5]]      -- executes as 1,1,2,2,3,3 and 4,5,4,5,4,5
c1Rev = [(x,y) | y <- [4,5], x <- [1..3]]   -- executes as 1,2,3,1,2,3 and 4,4,4,5,5,5

-- Later generators can depend on variables from earlier generators.
c2 = [(x,y) | x <- [1..3], y <- [x..3]]

-- Another example of later generators depending on earlier generators is the list concat
c3 :: [[a]] -> [a]
c3 xss = [x | xs <- xss, x <- xs]
-- input: [[1,2,3],[4,5,6]]
-- result: [1,2,3,4,5,6]

-- Wildcard in list generators can be used to ignore/discard elements from a list.
c4 :: [(a,b)] -> [a]
c4 pairs = [x | (x,_) <- pairs]
-- The bool value of tuple is ignored by the _ wildcard.
-- input: [(1,True), (2,False), (3,True), (4,False)]
-- result: [1,2,3,4]

c5 :: Num t => [a] -> t
c5 xs = sum [1 | _ <- xs]

{- 5.2 Gaurds -}
-- Allows a predicate to be executed to determine if the current value should be included
-- with the returned list.
c6 xs = [x | x <- xs, even x]
-- input: [1..10]
-- result: [2,4,6,8,10]

-- Another example
c7 n = [x | x <- [1..n], n `mod` x == 0]
prime n = c7 n == [1,n]

{- 5.3 The zip function -}
c8 :: [a] -> [(a,a)]
c8 xs = zip xs (tail xs)
-- input: [1,2,3,4,5,6]
-- result: [(1,2), (2,3), (3,4), (4,5), (5,6)]
-- Takes the main list and a copy of the main list (ignoring the first element)

c9 :: Ord a => [a] -> Bool
c9 xs = and [x <= y | (x,y) <- c8 xs]

c10 :: Eq a => a -> [a] -> [Int]
c10 x xs = [i | (x', i) <- zip xs [0..n], x == x']
            where n = length xs - 1
-- input: True [True,False,True,False,False,False,True,True]
-- result: [0,2,6,7]
-- Cool function that uses zip that pairs the list element with the its index in the list,
--  it then applies a predicate to identify if the value of x is equal to the value of the elemnt.
--  but returns the index.
--  in C# LINQ this would look like:
--  xs.select((element, index) => new Tuple<T,Int>(element, index))
--    .where(tup => tup.Item1 == x)
--    .select(tup => tup.Item2);

{- 5.4 String comprehensions -}
c11 x str = str !! x



{- Exercises -}
-- Exercise 0 --
-- option A
--e0a :: Num a => [a] -> [a]
--e0a = sum [[x * x] | x <- [1..100]]
-- Output: Does not compile.

-- option B
e0b = sum [x ^ 2 | x <- [1..100]]
-- Output: 338350

-- option C
e0c = sum [const 2 x | x <- [1..100]]

-- Exercise 1 --
-- option A
e1a n a = [True | _ <- [1..n]]
-- Input: e1a 3 False   Output: [True, True, True]

e1b n a = [n | _ <- [1..n]]
-- Input: e1b 3 True    Output: [3,3,3]

e1c n a = [a | _ <- [1..a]]
-- Input: e1c 3 True    Output: runtime error 'No instance for (Num Bool) arising from a use of 'e1c'

e1d n a = [a | _ <- [1..n]]
-- Input: e1d 3 "echo"  Output: ["echo","echo","echo"]

-- Exercise 2 --
-- Option A
e2a n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 + y^2 == z^2]
-- Input: e2a 10    Output: [] i.e. empty list

-- Option B
e2b n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]

-- Option C
e2c n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Option D
e2d n = [(x,y,(x^2 + y^2)) | x <- [1..n], y <- [1..n]]

-- Exercise 3 --
-- Option A
e3a n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (c7 num) == num

-- Option B
e3b n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum(init(c7 num)) == num

-- Exercise 4
-- Option A
e4a = [z | z <- [[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]]

e4b = concat [[[(x,y)] | x <- [1,2,3]] | y <- [4,5,6]]

--e4c = concat [(x,y) | y <- [4,5,6]] | x <- [1,2,3]

e4d = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- Exercise 5
find :: (Eq a) => a -> [(a,b)] -> [b]
find key xs = [v | (k, v) <- xs, key == k]

e5a :: (Eq a) => a -> [a] -> [Int]
e5a x xs = find x (zip xs [0..n])
    where n = length xs - 1
{-
e5b :: (Eq a) => a -> [a] -> [Int]
e5b x xs = find x xs -}
-- Compile error since the 2nd parameter of find expects [(a,b)] as the database but xs is just an [a]
{-
e5c :: (Eq a) => a -> [a] -> [Int]
e5c x xs = find x (zipWith (+) xs [0..n])
    where n = length xs - 1 -}
-- Compile error since zipWith applies the (+) function on each element at it zips.
{-
e5d :: (Eq a) => a -> [a] -> [Int]
e5d x xs = find n (zip xs [0..x])
    where n = length xs - 1 -}
-- Compile error since the [0..x] where x can be anything.

e6a xs ys = sum [x * y | x <- xs, y <- ys]

e6b xs ys = sum [x * y | (x,y) <- xs `zip` ys]

e12a xs ys = concat [[x,y] | x <- xs, y <- ys]
-- input: e12a [1..3] [4..6] --output: [1,4,1,5,1,6,2,4,2,5,2,6,3,4,3,5,3,6]

e12b xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]
-- input: e12b [1..3] [4..6] --output: [1,4,2,5,3,6]
{-
e12c xs ys = [x,y | (x,y) <- xs `zip` ys]
-}

e12d xs ys = [x : [y] | x <- xs, y <- ys]


divides x y = mod x y == 0
e13a x = [d | d <- [1..x], x `divides` d]