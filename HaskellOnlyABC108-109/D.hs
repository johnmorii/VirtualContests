import Control.Applicative

nodenum :: Int -> Int -> Int
nodenum n x = if r >= 1 then nodenum (n+1) q else n+1
    where (r,q) = divMod r 2