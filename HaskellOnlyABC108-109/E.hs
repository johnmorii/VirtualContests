import Control.Applicative

main = do
    [a,b] <- map read . words <$> getLine
    putStrLn $ if (mod (a*b) 2) == 0 then "No" else "Yes"
