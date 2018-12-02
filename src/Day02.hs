import Data.List

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ part1 input
  print $ part2 input

part1 :: [String] -> Int
part1 xss = count (p 2) xss * count (p 3) xss
  where
    count = (length .) . filter
    p n = elem n . mkHist  
    mkHist = map length . group . sort

part2 :: [String] -> String
part2 xss = 
  let  dist xs ys = sum $ map fromEnum $ zipWith (/=) xs ys
       (s1, s2) = head [(s1, s2) | s1 <- xss, s2 <- xss, dist s1 s2 == 1]
  in intersect s1 s2
