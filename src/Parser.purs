module Parser where

import Prelude

import Parsing
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Parsing.Combinators
import Data.List.NonEmpty
import Data.Identity
import Effect.Console
import Data.Int (toNumber)
import Data.Array (many)
import Data.List
import Data.Either

import AST


type P a = ParserT String Identity a

-- Dictionary
tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["every","play."],
  reservedOpNames = [",","=","+","-","*","/"]
  }

-- PROGRAM 


parsetest :: String -> Either ParseError (List Element)
parsetest x = do
    runParser x program


program :: P Program
--listOfActions = sepBy action (reservedOp ",")
program = sepBy elementChoice (whiteSpace)


elementChoice :: P Element
elementChoice = choice [
      loopElement,
      variableGlobal
]

loopElement :: P Element
loopElement = do
  l <- loop
  pure $ LoopElement l

-- tokenParser
loop :: P Loop
loop = do
  reserved "every"
  n <- detNum
  xs <- listOfActions
  pure $ Loop n xs

variableGlobal :: P Element
variableGlobal = do
  v <- variable
  pure $ VariableGlobal v

listOfActions :: P (List Action)
--listOfActions = sepBy action (reservedOp ",")
listOfActions = sepBy action (whiteSpace)

action :: P Action
action = choice [
      playAction,
      variableAction
]

playAction :: P Action
playAction = do
  reserved "play"
  reservedOp "."
  x <- identifier -- :: P String
  pure $ Play x

variableAction :: P Action
variableAction = do
  v <- variable
  pure $ VariableAction v

variable :: P Variable
variable = do
  x <- identifier
  reservedOp "="
  xs <- detNum
  pure $ Variable x xs 


--convert to Number

naturalOrFloatToNumber :: Either Int Number -> Number
naturalOrFloatToNumber (Left i) = toNumber i
naturalOrFloatToNumber (Right n) = n

-- Type Decleration

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

detNum :: P Number 
detNum = naturalOrFloatToNumber <$> tokenParser.naturalOrFloat

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace
