import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Control.Monad (sequence, when)

data OperatorType = 
    LeftParenthes
  | RightParenthes 
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Semicolon
  deriving (Show, Eq)

data Token =   Constant String
             | Identifier String
             | StringLiteral String
             | Operator OperatorType
  deriving (Show, Eq)

data PosToken = PosToken {
  get_pos   :: SourcePos,
  get_token :: Token 
} deriving (Show, Eq)

run input = case parse (spaces *> token_parser <* eof) "" input of
              Right val -> putStrLn $ show $ parse min_parser "" val
              Left  err -> putStrLn $ show err

token_parser = many1 $ PosToken <$> getPosition <*> makeToken <* spaces
  where
    makeToken =     try (Constant <$> many1 digit)
                <|> try identifier
                <|> try (StringLiteral <$> string_literal)
                <|> Operator <$> operator
    operator  =     char '+' *> return Plus
                <|> char '-' *> return Minus
                <|> char '*' *> return Asterisk
                <|> char '/' *> return Slash
                <|> char '(' *> return LeftParenthes
                <|> char ')' *> return RightParenthes
    identifier = fmap Identifier identifier_str
    identifier_str = (:) <$> nondigit <*> many (nondigit <|> digit)
    nondigit = letter <|> char '_'
    string_literal = char '"' *> s_char_sequence <* char '"'
    s_char_sequence = many1 (s_char <|> escape_sequence)
    s_char = noneOf "\n\"\\"
    escape_sequence =     try (string "\\'"  *> return '\'')
                      <|> try (string "\\\"" *> return '"' )
                      <|> try (string "\\a"  *> return '\a')
                      <|> try (string "\\b"  *> return '\b')
                      <|> try (string "\\f"  *> return '\f')
                      <|> try (string "\\n"  *> return '\n')
                      <|> try (string "\\r"  *> return '\r')
                      <|> try (string "\\t"  *> return '\t')
                      <|>      string "\\v"  *> return '\v'

gen_p m = tokenPrim (\c -> show c) (\pos c _cs -> get_pos c) $ m . get_token
p_const  = gen_p $ \n -> case n of
              Constant s -> Just s
              _          -> Nothing
p_ident  = gen_p $ \n -> case n of
              Identifier s -> Just s
              _            -> Nothing
p_string = gen_p $ \n -> case n of
              StringLiteral s -> Just s
              _               -> Nothing
p_op p   = gen_p $ \n -> if n == Operator p
              then Just p
              else Nothing


data Expr =   EIdent  String
            | EConst  String
            | EString String
            | BinOp OperatorType Expr Expr
            | UnaryOp OperatorType Expr
  deriving (Show, Eq)


expr = chainl1 term $ plus_op <|> minus_op
  where
    plus_op  = p_op Plus  *> return (BinOp Plus)
    minus_op = p_op Minus *> return (BinOp Minus)

term = chainl1 factor $ mul_op <|> div_op
  where
    mul_op = p_op Asterisk *> return (BinOp Asterisk)
    div_op = p_op Slash    *> return (BinOp Slash)

factor =     try parenthes_op
         <|> try unary_plus
         <|> try unary_minus 
         <|> leaf
  where
    parenthes_op = p_op LeftParenthes *> expr <* p_op RightParenthes 
    unary_plus   = UnaryOp <$> p_op Plus  <*> factor
    unary_minus  = UnaryOp <$> p_op Minus <*> factor

leaf =     try e_const <|> try e_ident <|> e_string
  where
    e_const  = p_const  >>= return . EConst
    e_ident  = p_ident  >>= return . EIdent
    e_string = p_string >>= return . EString

min_parser = expr <* eof

main = getContents >>= run



