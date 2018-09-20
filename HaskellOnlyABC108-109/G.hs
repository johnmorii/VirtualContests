import Control.Applicative

main = do
    [n,x] <- map read . words <$> getLine
    xlist <- map read . words <$> getLine
    print $ solve x xlist

solve :: Integer -> [Integer] -> Integer
solve x list = foldl (\acc elem -> gcd acc (elem - x)) (abs (head list - x)) (tail list)
