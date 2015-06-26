module Types where

data TSymbol = TOn | TOff | TNot
  deriving Eq
data TDirection = TRight | TLeft | TStay
data TRule = TRule String TSymbol TSymbol TDirection String
type Ruleset = [TRule]
type Tape a = [a]
data TMove = TMove String TDirection TSymbol
data THead = THead String (Tape TSymbol) Int

instance Show TSymbol where
  show TOn  = "1"
  show TOff = "0"


