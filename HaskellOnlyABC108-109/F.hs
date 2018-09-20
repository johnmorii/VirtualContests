import Control.Applicative
import Control.Monad
import Data.Set as Set

main = do
    n <- read <$> getLine
    list <- replicateM n getLine
    putStrLn $ solve list

solve :: [String] -> String
solve list | Nothing == (fsolve list) = "No"
           | otherwise = "Yes"

fsolve :: [String] -> Maybe (String, Set.Set String)
fsolve list = Prelude.foldl (\acc elem -> acc >>= shiritori elem) (Just (initword,initset)) (tail list)
    where initword = head list
          initset = Set.fromList [initword]

shiritori :: String ->  (String, Set String) -> Maybe (String, Set String)
shiritori wd (lastwd, s)  | Set.member wd s = Nothing
                          | (head wd) /= (head (reverse lastwd)) = Nothing
                          | otherwise = Just (wd, Set.insert wd s)

