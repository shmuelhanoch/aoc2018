module Util
  ( Parser
  , parse
  , unsafeParse
  , number
  , string
  )
where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse p ""

unsafeParse :: Parser a -> String -> a
unsafeParse = (fromEither .) . parse

number :: (Read a, Integral a) => Parser a
number = read <$> P.many1 P.digit

string :: String -> Parser String
string = P.string

-- unsafe! for use with validated input
fromEither :: Either e a -> a
fromEither (Right x) = x
