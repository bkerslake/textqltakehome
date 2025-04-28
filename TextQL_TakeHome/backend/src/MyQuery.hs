{-# LANGUAGE OverloadedStrings #-}
module MyQuery
  ( Query(..)
  , Field(..)
  , queryP
  , evalCond
  , project
  ) where

-- ─────────────────────  base  ──────────────────────────────
import           Control.Applicative        (empty, (<|>))
import           Control.Monad              (void)
import           Data.Void                  (Void)

-- ─────────────────────  text / number  ────────────────────
import           Data.Text                  (Text, pack)
import           Data.Scientific            (Scientific)

-- ─────────────────────  aeson-2 keymap  ───────────────────
import           Data.Aeson                 (Value(..), Object)
import qualified Data.Aeson.KeyMap         as KM
import qualified Data.Aeson.Key            as K

-- ─────────────────────  megaparsec  ───────────────────────
import           Text.Megaparsec            (Parsec, between, eof, optional)
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar
                                            , space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
                                            (Operator(..), makeExprParser)
import           Control.Monad.Combinators  (many, manyTill, sepBy1)

-- ═════════════════════  AST  ══════════════════════════════

data JVal  = JString Text | JNumber Scientific
  deriving (Eq, Show)

data Expr  = Col Text | Lit JVal
  deriving Show

data Comp  = CEq | CNeq | CLt | CGt
  deriving Show

data Cond  = CBin Comp Expr Expr
           | CAnd Cond Cond
           | COr  Cond Cond
  deriving Show

data Field = Star | Field Text
  deriving Show

data Query = Query
  { qFields :: [Field]
  , qCond   :: Maybe Cond
  , qLim    :: Maybe Int
  } deriving Show

-- ═════════════════════  PARSER  ═══════════════════════════

type Parser = Parsec Void String

sc       :: Parser ()           ; sc     = L.space space1 empty empty
lexeme   :: Parser a -> Parser a; lexeme = L.lexeme sc
symbol   :: String -> Parser String
symbol   = L.symbol sc

identifier :: Parser Text
identifier =
  pack <$> lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

strLit, numLit :: Parser Expr
strLit = Lit . JString . pack <$> lexeme
           (char '\'' *> manyTill L.charLiteral (char '\''))
numLit = Lit . JNumber <$> lexeme L.scientific

expr :: Parser Expr
expr =  strLit
    <|> numLit
    <|> (Col <$> identifier)

compOp :: Parser Comp
compOp =   CEq  <$ symbol "="
       <|> CNeq <$ symbol "!="
       <|> CLt  <$ symbol "<"
       <|> CGt  <$ symbol ">"

cmp :: Parser Cond
cmp = do
  lhs <- expr
  op  <- compOp
  rhs <- expr
  pure (CBin op lhs rhs)

cond :: Parser Cond
cond = makeExprParser term
        [ [InfixL (CAnd <$ symbol "AND")]
        , [InfixL (COr  <$ symbol "OR")]
        ]
  where
    term = between (symbol "(") (symbol ")") cond <|> cmp

fieldList :: Parser [Field]
fieldList =
      ([Star] <$ symbol "*")
  <|> sepBy1 (Field <$> identifier) (symbol ",")

queryP :: Parser Query
queryP = do
  void (symbol "SELECT")
  fs   <- fieldList
  void (symbol "FROM" >> symbol "table")
  mbW  <- optional (symbol "WHERE" >> cond)
  mbL  <- optional (symbol "LIMIT" >> lexeme L.decimal)
  void (optional (char ';') >> eof)
  pure $ Query fs mbW mbL

-- ═════════════════════  EVALUATION  ═══════════════════════

type Row = Object   -- = KeyMap Value (aeson-2.x)

key :: Text -> K.Key
key = K.fromText

toJVal :: Value -> JVal
toJVal (String t) = JString t
toJVal (Number n) = JNumber n
toJVal v          = error $ "unsupported JSON value: " <> show v

evalExpr :: Expr -> Row -> JVal
evalExpr (Lit v) _   = v
evalExpr (Col n) row =
  maybe (error $ "unknown column " <> show n) toJVal
        (KM.lookup (key n) row)

-- helper for numeric comparisons
numeric :: (Scientific -> Scientific -> Bool) -> JVal -> JVal -> Bool
numeric f (JNumber a) (JNumber b) = f a b
numeric _ _            _          = error "numeric comparison on non-numbers"

cmpJ :: Comp -> JVal -> JVal -> Bool
cmpJ CEq  = (==)
cmpJ CNeq = (/=)
cmpJ CLt  = numeric (<)
cmpJ CGt  = numeric (>)

evalCond :: Cond -> Row -> Bool
evalCond (CBin op l r) row = cmpJ op (evalExpr l row) (evalExpr r row)
evalCond (CAnd a b)    row = evalCond a row && evalCond b row
evalCond (COr  a b)    row = evalCond a row || evalCond b row

project :: [Field] -> Row -> Row
project [Star] r = r
project fs      r = KM.fromList
  [ (key n, v) | Field n <- fs, Just v <- [KM.lookup (key n) r] ]
