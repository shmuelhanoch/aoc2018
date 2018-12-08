{-# LANGUAGE DeriveFoldable #-}

import Text.Parsec (Parsec, ParseError, digit, parse, count, many1)
import Text.Parsec.Char (spaces)

type Parser = Parsec String ()

data Tree a = Tree [Tree a] [a]
  deriving (Foldable)

main :: IO ()
main = do
  t <- parse_ tree <$> readFile "input"
  print $ sum t
  print $ value t

tree :: Parser (Tree Int)
tree = do
  nChildren <- num
  nMeta <-  num
  Tree <$> count nChildren tree <*> count nMeta num
    where num = read <$> many1 digit <* spaces

parse_ :: Parser a -> String -> a
parse_ p s = fromEither $ parse p "" s
  where fromEither (Right x) = x

value :: Tree Int -> Int
value (Tree children meta)
  | null children = sum meta
  | otherwise = sum $ map (childVals !!) meta
  where childVals = 0 : map value children ++ repeat 0
