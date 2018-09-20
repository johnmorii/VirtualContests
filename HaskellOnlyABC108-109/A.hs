import Control.Applicative
main = do
    n <- read <$> getLine
    print $ solve n
solve n = m * l
    where m = div n 2
          l = if mod n 2 == 0 then m else m + 1