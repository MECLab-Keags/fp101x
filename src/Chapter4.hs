module Chapter4 where

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


