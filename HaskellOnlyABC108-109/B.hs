import Control.Applicative

rotate :: (Int,Int) -> (Int,Int)
rotate (x,y) = (-y,x)

main = do
    [x1,y1,x2,y2] <- map read . words <$> getLine
    putStrLn $ pprint (solve x1 y1 x2 y2)

pprint (a,b,c,d) = show(a) ++ " " ++ show(b) ++ " " ++ show(c) ++ " " ++ show(d)

solve x1 y1 x2 y2 = (x3,y3,x4,y4)
     where (dx3,dy3) = rotate (x2-x1,y2-y1)
           x3 = x2 + dx3
           y3 = y2 + dy3
           (dx4,dy4) = rotate (x3-x2,y3-y2)
           x4 = x3 + dx4
           y4 = y3 + dy4