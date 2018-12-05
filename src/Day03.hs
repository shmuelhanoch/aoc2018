import qualified Data.Set as S
import Util

type PSet = S.Set (Int, Int)

data Claim = Claim { cID, cX, cY, cW, cH :: Int }

main :: IO ()
main = do
  pss <- map (toSet . unsafeParse parseClaim) . lines <$> readFile "input"
  let cp = commonPoints pss
  print $ S.size cp
  print $ lonely pss cp

commonPoints :: [PSet] -> PSet
commonPoints = snd . foldr step (S.empty, S.empty) where
  step ps (once, twice) =
    let s = S.intersection ps once
    in (S.union ps once, S.union s twice)

lonely :: [PSet] -> PSet -> Int
lonely = go 1 where
  go id (ps : rest) cp = 
    if S.size (S.intersection ps cp) == 0
      then id
      else go (id + 1) rest cp

toSet :: Claim -> PSet
toSet (Claim _ x y a b) =
  S.fromList [(u, v) | u <- [x..x+a-1], v <- [y..y+b-1]]

parseClaim :: Parser Claim
parseClaim = Claim 
  <$ string "#"   <*> number
  <* string " @ " <*> number
  <* string ","   <*> number
  <* string ": "  <*> number
  <* string "x"   <*> number
