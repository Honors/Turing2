module Main where

import Parse
import Types

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
moveDirection TStay  = id

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

simulateMachine :: Either ParseError (Tape TSymbol, Ruleset) -> Tape TSymbol
simulateMachine (Right (tape, table)) =
  step table (THead "A" tape 0)
simulateMachine (Left error) = []

main =
  do ruleFile <- readFile "../examples/bit_add.turing"
     putStrLn . show . take 10 $ simulateMachine (parseRulefile ruleFile)

