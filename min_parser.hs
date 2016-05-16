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


gen_p m = tokenPrim (\c -> show c) (\pos c _cs -> get_pos c) $ m . get_token
p_const = gen_p $ \n -> case n of
              Constant s -> Just s
              _          -> Nothing
p_ident = gen_p $ \n -> case n of
              Identifier s -> Just s
              _            -> Nothing
p_op p  = gen_p $ \n -> if n == Operator p
              then Just p
              else Nothing


data Expr = EIdent String | EConst String | BinOp OperatorType Expr Expr | UnaryOp OperatorType Expr
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
         <|> try plus_op
         <|> try minus_op
         <|> leaf
  where
    parenthes_op = p_op LeftParenthes *> expr <* p_op RightParenthes 
    plus_op      = UnaryOp <$> p_op Plus  <*> factor
    minus_op     = UnaryOp <$> p_op Minus <*> factor

leaf =     try e_const <|> e_ident
  where
    e_const = p_const >>= return . EConst
    e_ident = p_ident >>= return . EIdent

min_parser = expr <* eof

main = getContents >>= run



