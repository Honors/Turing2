module Parse (parseRulefile, ParseError) where

import Text.ParserCombinators.Parsec
import Types

symbolOpts = "01_"
directionOpts = "RLN"

symbolFromStr '0' = TOff
symbolFromStr '1' = TOn
symbolFromStr '_' = TNot

directionFromStr 'R' = TRight
directionFromStr 'L' = TLeft
directionFromStr 'N' = TStay

eol = char '\n'

tap :: Monad m => (a -> m b) -> a -> m a
tap fn c = do fn c; return c
blindTap fn c = do fn; return c

spaceThen e = char ' ' >> e

ruleFile :: GenParser Char st (Tape TSymbol, Ruleset)
ruleFile = 
  do tape <- parseSymbols >>= (blindTap eol)
     rules <- many parseRule
     eof >> return (tape, rules)

parseDirection = fmap directionFromStr $ oneOf directionOpts
parseSymbol = fmap symbolFromStr $ oneOf symbolOpts
parseSymbols = 
  do first <- parseSymbol
     rest <- spaceThen parseSymbols <|> (return [])
     return (first : rest)

parseRule :: GenParser Char st TRule
parseRule = 
  do st <- many alphaNum
     sym <- spaceThen $ parseSymbol
     sym' <- spaceThen $ parseSymbol
     dir <- spaceThen $ parseDirection
     st' <- spaceThen $ many alphaNum
     (>>) eol $ return $ TRule st sym sym' dir st'

parseRulefile :: String -> Either ParseError (Tape TSymbol, Ruleset)
parseRulefile input = parse ruleFile "(unknown)" input

