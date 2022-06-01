module AST where

import Prelude
import Data.Monoid.Dual
import Data.List
import Data.Show
import Data.Either

type Program = (List Element)

data Element = LoopElement Loop | VariableGlobal Variable

data Loop = Loop Number (List Action) 

--data Action = Action String 

data Action = Play String | VariableAction Variable 

data Variable = Variable String Number

instance showElement :: Show Element where
    show (LoopElement x) = "[" <>show x<> "]"
    show (VariableGlobal x) = "[" <>show x<> "]"


instance showLoop :: Show Loop where
    show (Loop n xs) = "(Every " <> show n <> " " <> show xs <> ")"

instance showPlay :: Show Action where
    show (Play x) = "play.(" <> show x <> ")"
    show (VariableAction x) = "(" <>show x<> ")"

instance showVariable :: Show Variable where
    show (Variable x xs) = show x <> "=" <> show xs