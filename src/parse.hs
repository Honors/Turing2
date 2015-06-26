module Parse (parseRulefile, ParseError) where

import Text.ParserCombinators.Parsec
import Types

symbolFromStr "0" = TOff
symbolFromStr "1" = TOn
symbolFromStr "_" = TNot

directionFromStr "R" = TRight
directionFromStr "L" = TLeft
directionFromStr "N" = TStay

eol :: GenParser Char st Char
eol = char '\n'

ruleFile :: GenParser Char st (Tape TSymbol, Ruleset)
ruleFile = 
  do tape <- parseTape
     rules <- many parseRule
     eof
     return (tape, rules)

parseTape :: GenParser Char st (Tape TSymbol)
parseTape =
  do result <- parseSymbols
     eol
     return result
                          
parseSymbols :: GenParser Char st [TSymbol]
parseSymbols = 
  do first <- oneOf "01_"
     next <- remainingSymbols
     return ((symbolFromStr [first]) : next)

remainingSymbols :: GenParser Char st [TSymbol]
remainingSymbols =
    (char ' ' >> parseSymbols)
    <|> (return [])

parseRule :: GenParser Char st TRule
parseRule = 
  do st <- many alphaNum
     char ' '
     sym <- oneOf "01_"
     char ' '
     sym' <- oneOf "01_"
     char ' '
     dir <- oneOf "RLN"
     char ' '
     st' <- many alphaNum
     eol
     return $ TRule st (symbolFromStr [sym]) (symbolFromStr [sym']) (directionFromStr [dir]) st'

parseRulefile :: String -> Either ParseError (Tape TSymbol, Ruleset)
parseRulefile input = parse ruleFile "(unknown)" input

