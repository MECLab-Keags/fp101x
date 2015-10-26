module Chapter4 where

{- 4.1 Function definitions -}
isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

isEven :: Integral n => n -> Bool
isEven n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

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
(^) :: Bool -> Bool -> Bool
True ^ True = True
True ^ False = False
False ^ True = False
False ^ False = False

-- Rather:
True ^ True = True
_^_ = False

-- Or even better:
True ^ b = b
False ^_ = False

-- Pattern match to test if the list starts a character of 'a'
test :: [Char] -> Bool
test ('a':_) = True
test_ = False
