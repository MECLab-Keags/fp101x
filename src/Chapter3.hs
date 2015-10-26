module TypesClasses where
{- Chapter 3 -}

{- 3.6 Curried functions -}
-- add function that takes a Tuple of (Int,Int)
add :: (Int, Int) -> Int
add (x, y) = x + y

-- curried version of the add function
add' :: Int -> (Int -> Int)
add' x y = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

{- 3.9 Basic classes -}
-- Eq - equality types
-- :t (==)         -- output: (==) :: Eq a => a -> a -> Bool

-- Ord - ordered types
-- Note: Strings, Lists and Tuples are ordered lexicographically
--  (i.e the same as a dictionary)
-- :t (>=)         -- output: (>=) :: Ord a => a -> a -> Bool
-- :t (min)        -- output: (min) :: Ord a => a -> a -> a

-- Show - showable types
-- Contains types whose values that can be converted into strings
-- :t show         -- output: show :: Show a => a -> String

-- Read - readable types
-- Contains types whose values can be converted FROM string
-- :t read         -- output: read :: Read a => String -> a
readTrue = read "True" :: Bool

{- 3.11 Excercises -}
-- Question 1
listChar = ['a','b','c']     -- answer: ['a','b','c'] :: [Char]
tupChar = ('a','b','c')      -- answer: ('a','b','c') :: (Char, Char, Char)
listTup = [(False, 'o'), (True, '1')]    -- answer: [(False, 'o'), (True, '1')] :: [(Bool, Char)]
tupeList = ([False, True], ['0', '1'])   -- answer: ([False, True], ['0', '1']) :: ([Bool], [Char])
listFunc = [tail, init, reverse]         -- answer: [tail, init, reverse] :: [[a] -> [a]]

-- Question 2
second xs = head(tail xs)   -- answer: second :: [a] -> a
swap (x, y) = (y, x)        -- answer: swap :: (t,t1) -> (t1, t)
pair x y = (x, y)           -- answer: pair :: t -> t1 -> (t, t1)
double x = x * 2            -- answer: double :: Num a => a -> a
palindrome xs = reverse xs == xs        -- answer: palindrome :: Eq a => [a] -> Bool
twice f x = f(f x)          -- answer: twice :: (t -> t) -> t -> t

