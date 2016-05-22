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
  | PlusPlus
  | MinusMinus
  | And
  | Tilde
  | Exclamation
  | Percent 
  | LeftShift
  | RightShift
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | EqualEqual
  | NotEqual
  | Hat
  | Or
  | AndAnd
  | OrOr
  | Colon
  | Question
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
    operator  =     try (string "++" *> return PlusPlus)
                <|> try (string "--" *> return MinusMinus)
                <|> try (string "<<" *> return LeftShift)
                <|> try (string ">>" *> return RightShift)
                <|> try (string "<=" *> return LessThanEqual)
                <|> try (string ">=" *> return GreaterThanEqual)
                <|> try (string "!=" *> return NotEqual)
                <|> try (string "==" *> return EqualEqual)
                <|> try (string "&&" *> return AndAnd)
                <|> try (string "||" *> return OrOr)
                <|> char '+' *> return Plus
                <|> char '-' *> return Minus
                <|> char '*' *> return Asterisk
                <|> char '/' *> return Slash
                <|> char '%' *> return Percent
                <|> char '<' *> return LessThan
                <|> char '>' *> return GreaterThan
                <|> char '&' *> return And
                <|> char '|' *> return Or
                <|> char '^' *> return Hat
                <|> char ':' *> return Colon
                <|> char '?' *> return Question
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
            | TernaryOp Expr Expr Expr
            | BinOp     OperatorType Expr Expr
            | UnaryOp   OperatorType Expr
  deriving (Show, Eq)

binOp p = p_op p *> return (BinOp p)

expr = cond_e

cond_e = lor_e >>= rest
  where
    rest p =     try (TernaryOp p <$> (p_op Question *> expr) <*> (p_op Colon *> cond_e))
             <|> return p

lor_e   = chainl1 land_e     $ binOp OrOr

land_e  = chainl1 or_e       $ binOp AndAnd

or_e    = chainl1 exor_e     $ binOp Or

exor_e  = chainl1 and_e      $ binOp Hat

and_e   = chainl1 equal_e    $ binOp And

equal_e = chainl1 relation_e $ binOp EqualEqual <|> binOp NotEqual

relation_e = chainl1 shift_e $ binOp LessThan         <|>
                               binOp GreaterThan      <|>
                               binOp LessThanEqual    <|>
                               binOp GreaterThanEqual

shift_e          = chainl1 additive_e $ binOp LeftShift <|> binOp RightShift

additive_e       = chainl1 multiplicative_e $ binOp Plus <|> binOp Minus

multiplicative_e = chainl1 factor $ binOp Asterisk <|> binOp Slash <|> binOp Percent

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



