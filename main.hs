module Main where

data TSymbol = TOn | TOff | TNot
  deriving Eq
data TDirection = TRight | TLeft
data TRule = TRule String TSymbol TSymbol TDirection String
type Ruleset = [TRule]
type Tape a = [a]
data TMove = TMove String TDirection TSymbol
data THead = THead String (Tape TSymbol) Int

instance Show TSymbol where
  show TOn  = "1"
  show TOff = "0"

table = [(TRule "A" TOff TOff TRight "Z"),
         (TRule "A" TOn  TOn  TRight "C"),
         (TRule "Z" TOff TOff TRight "H"),
         (TRule "Z" TOn  TOn  TLeft  "N"),
         (TRule "C" TOff TOn  TLeft  "N"),
         (TRule "C" TOn  TOff TLeft  "Y"),
         (TRule "N" TNot TOff TRight "H"),
         (TRule "Y" TNot TOn  TRight "H")]

ruleMatches :: String -> TSymbol -> TRule -> Bool
ruleMatches st sym (TRule st' TNot _ _ _) = st' == st
ruleMatches st sym (TRule st' sym' _ _ _) = st' == st && sym' == sym

getMove :: Ruleset -> THead -> TMove
getMove rs (THead st tape n) =
  let TRule _ _ sym' dir st' = (filter (ruleMatches st (tape !! n)) rs) !! 0
  in TMove st' dir sym'

moveDirection :: TDirection -> Int -> Int
moveDirection TRight = (+1)
moveDirection TLeft  = ((-)1)

set n x xs = let (fr, (b:bs)) = splitAt n xs in fr ++ [x] ++ bs

applyMove :: THead -> TMove -> THead
applyMove (THead st tape index) (TMove st' dir sym') =
  THead st' (set index sym' tape) (moveDirection dir index)

step :: Ruleset -> THead -> Tape TSymbol
step table head@(THead st tape n) =
  let head'@(THead st' t' n') = applyMove head (getMove table head)
  in if st' == "H"
     then t'
     else step table head'

main =
  putStrLn . show . take 10 $ step table (THead "A" ([TOn, TOff] ++ (repeat TOff)) 0)

