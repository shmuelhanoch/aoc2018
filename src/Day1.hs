import qualified Data.IntSet as IS

getData :: IO [Int]
getData = map parseInt . lines <$> readFile "input"
  where
    parseInt ('+':s) = read s
    parseInt s = read s

firstDup :: [Int] -> Int
firstDup = go IS.empty . scanl (+) 0 . cycle where
  go s (x:xs) =
    if IS.member x s
      then x
      else go (IS.insert x s) xs

main :: IO ()
--Part 1:
--main = print . sum =<< getData

--Part 2:
--main = print . firstDup =<< getData
