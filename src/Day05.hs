import Data.Char (toLower)

match :: Char -> Char -> Bool
match x y = x /= y && toLower x == toLower y

reduce :: String -> String
reduce = reduceWith (const False)

reduceWith :: (Char -> Bool) -> String -> String
reduceWith p = foldr step []
  where
    step x stack
      | p x = stack
      | otherwise = case stack of
          (y:ys) | x `match` y -> ys
          ys  -> x:ys

main :: IO ()
main = do
  s <- reduce <$> readFile "input"
  print $ length s
  let preds = zipWith (\ a b x -> x == a || x == b) ['a'..'z'] ['A'..'Z']
  print $ minimum $ map (length . flip reduceWith s) preds
