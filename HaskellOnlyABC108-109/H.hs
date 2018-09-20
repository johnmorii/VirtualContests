import Control.Applicative
import Control.Monad
import Data.List

main = do
    [h,w] <- map read . words <$> getLine
    a <- replicateM h (map read . words <$> getLine)
    let moves = solve h w (zip [1..h] a)
    print $ length moves
    forM moves (putStrLn . unwords . map show) 


solve :: Int -> Int -> [(Int,[Int])] -> [[Int]]
solve h w list = rmoves ++ lastmoves
    where tuples = map (calcraw w) list
          rmoves = concatMap fst tuples
          rparities = map snd tuples
          (lastmoves,lparity) = calcCol h (w,rparities)

calcraw :: Int -> (Int,[Int]) -> ([[Int]],Int)
calcraw width (y,raw) = (reverse rms, p)
    where (rms,p,col) = foldl (move y width) ([],0,1) raw

move :: Int -> Int -> ([[Int]],Int,Int) -> Int -> ([[Int]],Int,Int)
move y width (moves, parity, column) elem | column < width = if mod (parity + elem) 2 == 0
                                                             then (moves,0,column+1)
                                                             else ([y,column,y,column+1]:moves, 1, column+1)
                                          | otherwise = if mod (parity + elem) 2 == 0
                                                        then (moves,0,column+1)
                                                        else (moves,1,column+1)

moveCol :: Int -> Int -> ([[Int]],Int,Int) -> Int -> ([[Int]],Int,Int)
moveCol x height (moves, parity, raw) elem | raw < height = if mod (parity + elem) 2 == 0
                                                                 then (moves,0,raw+1)
                                                                 else ([raw,x,raw+1,x]:moves, 1, raw+1)
                                              | otherwise = if mod (parity + elem) 2 == 0
                                                            then (moves,0,raw+1)
                                                            else (moves,1,raw+1)
calcCol :: Int -> (Int,[Int]) -> ([[Int]],Int)
calcCol height (x,col) = (reverse rms, p)
    where (rms,p,raw) = foldl (moveCol x height) ([],0,1) col