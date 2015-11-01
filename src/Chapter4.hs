module Chapter4 where
--import Prelude hiding ((||))
--import Prelude hiding ((&&))


{- 4.1 Function definitions -}
isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

isEven :: Integral n => n -> Bool
isEven n = n `mod` 2 == 0

--splitAt :: Int -> [a] -> ([a], [a])
--splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1/n

{- 4.2 Conditional expressions -}
-- NOTE: if expressions must always have an else
abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
            if n == 0 then 0 else 1

{- 4.3 Guarded exquations -}
absGuarded n | n >= 0 = n
             | otherwise = -n

-- Guarded version of the signum is easier to understand than nested if-else expressions.
signumGuarded n | n < 0 = -1
                | n == 0 = 0
                | otherwise = 1

{- 4.4 Pattern matching -}
-- Instead of:
--(^) :: Bool -> Bool -> Bool
--True (^) True = True
--True (^) False = False
--False (^) True = False
--False (^) False = False

-- Rather:
--True (^) True = True
--_(^)_ = False

-- Or even better:
--True (^) b = b
--False (^)_ = False

-- Pattern match to test if the list starts a character of 'a'
test :: [Char] -> Bool
test ('a':_) = True
test_ = False

{- 4.8 Exercises -}

-- Exercise 0 --
-- option 1:
{-halve xs = (take n xs, drop n xs)
    where n = length xs / 2 -}
-- result: compile error on the use of '/'.

-- Option 2:
--halve xs = splitAt (length xs `div` 2) xs
-- result: halve :: [a] -> ([a],[a])

-- Option 3:
{-halve xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs-}
-- result: CORRECT halve :: [a] -> ([a], [a])

-- Option 4:
{-halve xs = splitAt (length xs `div` 2)-}
-- result: Partial application since the last xs is missing as a parameter to 'splitAt'

-- Option 5:
{-halve xs = (take n xs, drop (n + 1) xs)
    where n = length xs `div` 2-}
-- result: wrong, it drops the 4 i.e. ([1,2,3], [5,6])

-- Option 6:
{-halve xs = splitAt (div (length xs) 2) xs-}
-- result: CORRECT

-- Option 7:
--halve xs = splitAt (length xs / 2) xs
-- result: compile error on the use of '/'.


-- Exercise 1 --
{-
    If the parameter list is empty then an empty list should be returned otherwise the tail of the list should be returned.
    safetail :: [a] -> [a]
-}
-- Option 1:
--safetail xs = if null xs then [] else tail xs
-- result: CORRECT

-- Option 2:
--safetail [] = []
--safetail (_: xs) = xs
-- result: CORRECT

-- Option 3:
{-
safetail (_:xs)
    | null xs = []
    | otherwise = tail xs
-}
-- result: Throws an exception on the null pattern match.

-- Option 4:
{-
safetail xs
    | null xs = []
    | otherwise = tail xs
    -}
-- result: CORRECT

-- Option 5:
{-
safetail xs = tail xs
safetail [] = []
-}
-- result: Throws an exception on the first pattern match expression, when [] is passed as the parameter,
--          since expressions are evaluated top--to-bottom left-to-right.

 -- Option 6:
 {-
 safetail [x] = [x]
 safetail (_:xs) = xs
-}
-- result: Compile error on [x] = [x] since [x] isn't a parameter

-- Option 7:
{-
safetail xs = \ xs ->
    case xs of
        [] -> []
        (_:xs) -> tail xs
-}
-- result: I get a run-time error "No instance for (Show ([t10] -> [t10]))"
--          so need to come back and look at this.

-- Exercise 2 --
-- Option 1:
{-
False || False = False
_ || _ = True
-}
-- result: I think that works.

-- Option 2:
{-
False || b = b
True || _ = True
-}
-- result: I think that works too??

-- Option 3:
{-
b || c
    | b == c = True
    | otherwise = False
-}
-- result: Breaks ||, True || False returns False

-- Option 4:
{-
b || c
    | b == c = b
    | otherwise = True
-}
-- result: CORRECT, first guard if  b == c then b (i.e. b = True, c = True the return b). Otherwise just return True

-- Option 5:
{-
b || False = b
_ || True = True
-}
-- result : CORRECT

-- Option 6:
{-
b || c
    | b == c = c
    | otherwise = True
-}
-- result: CORRECT

-- Option 7:
{-
b || True = b
_ || True = True
-}
-- result: Compile error.

-- Option 8:
{-
False || False = False
False || True = True
True || False = True
True || True = True
-}
-- result: CORRECT

-- Exercise 3 --
-- Option 0:
{-
True && True = True
_ && _ = False
-}
-- result: CORRECT

-- Option 1:
{-
a && b = if a then if b then True else False else False
-}
-- result: CORRECT

-- Option 2:
{-
a && b = if not(a) then not(b) else True
-}
-- result: False && False returns True.

-- Option 3:
{-
a && b = if a then b
-}
-- result: Compile error, missing else clause

-- Option 4:
{-
a && b = if a then if b then False else True else False
-}
-- result: Incorrect

-- Option 5:
{-
a && b = if a then b else False
-}
-- result: CORRECT

-- Option 6:
{-
a && b = if b then a else False
-}
-- result: CORRECT

-- Exercise 7 --
{- Sample: remove 0 [1,2,3,4,5] = [2,3,4,5] -}
remove n xs = take n xs ++ drop (n+1) xs

-- Exercise 8 --
{- Sample: funct 3 [1,2,3,4,5,6,7] -}
funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

-- Lab Exercise 2 --
e2 = [[[1, 2, 3]], [[3, 4, 5]]]

-- Lab Exercise 3 --
e3 x = x * 2

-- Lab Exercise 6 --
e6 x y = x * y

-- Lab Exercise 8 --
e8 x y = (y,x)

-- Lab exercise 9 --
e9 [x,y] = (x, True)

-- Lab exercise 10 --
e10 (x,y) = [x,y]

-- Lab exercise 11 --
e11 :: (Char, Bool)
e11 = ('\a',False)

-- Lab exercise 13 --
e13 :: Int -> Int -> Int
e13 x y = x + y * y

-- Lab exercise 15 --
e15 :: [a] -> [b] -> (a,b)
e15 xs ys = (head xs, head ys)