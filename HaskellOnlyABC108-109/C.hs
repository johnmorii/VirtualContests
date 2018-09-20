import Control.Applicative

main = do
    [n,k] <- map read . words <$> getLine
    print $ solve n k

solve n k = if mod k 2 == 0 then (div n k) ^ 3 + (f n k) ^ 3 else (div n k) ^ 3

f n k = div n k + if mod n k >= div k 2 then 1 else 0

