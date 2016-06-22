import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Control.Monad (sequence, when)

data OperatorType = 
    LeftParenthes
  | RightParenthes 
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
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
  | Equal
  | AsteriskEqual
  | SlashEqual
  | PercentEqual
  | PlusEqual
  | MinusEqual
  | LeftShiftEqual
  | RightShiftEqual
  | AndEqual
  | HatEqual
  | OrEqual
  | Comma
  | ThreeDots
  deriving (Show, Eq)

data KeywordType = 
    If
  | Else
  deriving (Show, Eq)

keyword_dictionary = [
  ("if"  , If),
  ("else", Else) ]

data Token =   Constant String
             | Identifier String
             | StringLiteral String
             | Operator OperatorType
             | Keyword KeywordType
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
                <|> try keyword_or_identifier
                <|> try (StringLiteral <$> string_literal)
                <|> Operator <$> operator
    operator  =     try (string "..." *> return ThreeDots)
                <|> try (string "++"  *> return PlusPlus)
                <|> try (string "--"  *> return MinusMinus)
                <|> try (string "<<"  *> return LeftShift)
                <|> try (string ">>"  *> return RightShift)
                <|> try (string "<="  *> return LessThanEqual)
                <|> try (string ">="  *> return GreaterThanEqual)
                <|> try (string "!="  *> return NotEqual)
                <|> try (string "=="  *> return EqualEqual)
                <|> try (string "&&"  *> return AndAnd)
                <|> try (string "||"  *> return OrOr)
                <|> try (string "*="  *> return AsteriskEqual)
                <|> try (string "/="  *> return SlashEqual)
                <|> try (string "%="  *> return PercentEqual)
                <|> try (string "+="  *> return PlusEqual)
                <|> try (string "-="  *> return MinusEqual)
                <|> try (string "<<=" *> return LeftShiftEqual)
                <|> try (string ">>=" *> return RightShiftEqual)
                <|> try (string "&="  *> return AndEqual)
                <|> try (string "^="  *> return HatEqual)
                <|> try (string "|="  *> return OrEqual)
                <|> char '=' *> return Equal
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
                <|> char ';' *> return Semicolon
                <|> char '?' *> return Question
                <|> char '(' *> return LeftParenthes
                <|> char ')' *> return RightParenthes
                <|> char '{' *> return LeftBrace
                <|> char '}' *> return RightBrace
                <|> char '[' *> return LeftBracket
                <|> char ']' *> return RightBracket
                <|> char ',' *> return Comma

keyword_or_identifier = fmap choose identifier_str
  where
    choose s = case lookup s keyword_dictionary of
                 Just k  -> Keyword    k
                 Nothing -> Identifier s
    identifier_str = (:) <$> nondigit <*> many (nondigit <|> digit)
    nondigit = letter <|> char '_'

string_literal = char '"' *> s_char_sequence <* char '"'
  where
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
p_kwd p  = gen_p $ \n -> if n == Keyword p
              then Just p
              else Nothing
p_ident_s s = gen_p $ \n -> case n of
                Identifier x -> if x == s then Just x else Nothing
                _            -> Nothing
 
data Expr =   EIdent  String
            | EConst  String
            | EString String
            | TernaryOp Expr Expr Expr
            | BinOp     OperatorType Expr Expr
            | UnaryOp   OperatorType Expr
            | PostfixOp OperatorType Expr
            | NullExpr
  deriving (Show, Eq)

binOp     p   = p_op p *> return (BinOp p)
postfixOp p e = p_op p *> return (PostfixOp p e)

expr = chainl1 assignment_e $ binOp Comma

assignment_e = try assign_e <|> cond_e
  where
    assign_e = do u  <- unary_e
                  op <- assign_op
                  op u <$> assignment_e
    assign_op = binOp Equal           <|>
                binOp AsteriskEqual   <|>
                binOp SlashEqual      <|>
                binOp PercentEqual    <|>
                binOp PlusEqual       <|>
                binOp MinusEqual      <|>
                binOp LeftShiftEqual  <|>
                binOp RightShiftEqual <|>
                binOp AndEqual        <|>
                binOp HatEqual        <|>
                binOp OrEqual

cond_e = lor_e >>= rest
  where
    rest p    = try (ternary p) <|> return p
    ternary p = TernaryOp p <$> (p_op Question *> expr <* p_op Colon) <*> cond_e

lor_e = chainl1 land_e
    $ binOp OrOr

land_e = chainl1 or_e
    $ binOp AndAnd

or_e = chainl1 exor_e
    $ binOp Or

exor_e = chainl1 and_e
    $ binOp Hat

and_e = chainl1 equal_e
    $ binOp And

equal_e = chainl1 relation_e
    $ binOp EqualEqual
  <|> binOp NotEqual

relation_e = chainl1 shift_e
    $ binOp LessThan
  <|> binOp GreaterThan
  <|> binOp LessThanEqual
  <|> binOp GreaterThanEqual

shift_e = chainl1 additive_e
    $ binOp LeftShift
  <|> binOp RightShift

additive_e = chainl1 multiplicative_e
    $ binOp Plus
  <|> binOp Minus

multiplicative_e = chainl1 cast_e
    $ binOp Asterisk
  <|> binOp Slash
  <|> binOp Percent

cast_e = unary_e

unary_e = try (UnaryOp <$> p_op PlusPlus   <*> unary_e) <|>
          try (UnaryOp <$> p_op MinusMinus <*> unary_e) <|>
          postfix_e

postfix_e = primary_e >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    helper x =     postfixOp PlusPlus x
               <|> postfixOp MinusMinus x

primary_e = try e_const <|> try e_ident <|> try e_string <|> e_parenthes
  where
    e_const  = p_const  >>= return . EConst
    e_ident  = p_ident  >>= return . EIdent
    e_string = p_string >>= return . EString
    e_parenthes = p_op LeftParenthes *> expr <* p_op RightParenthes

data BlockItem = StatementBlockItem Statement
  deriving (Show, Eq)

data Statement =   ExprStatement Expr
                 | IfStatement Expr Statement Statement
                 | BlkStatement [BlockItem]
                 | NullStatement
  deriving (Show, Eq)

statement = try selection_stmt <|> try block_stmt <|> expr_stmt

selection_stmt = do p_kwd If
                    e  <- p_op LeftParenthes *> expr <* p_op RightParenthes
                    s1 <- statement
                    s2 <- option NullStatement (p_kwd Else *> statement)
                    return $ IfStatement e s1 s2

block_stmt = p_op LeftBrace *> block_items <* p_op RightBrace

block_items = fmap BlkStatement $ many (StatementBlockItem <$> statement)

expr_stmt = fmap ExprStatement (option NullExpr expr <* p_op Semicolon)

declaration = do declaration_spec
                 sepBy1 init_declarator (p_op Comma)
                 p_op Semicolon

declaration_spec = many1 $
  (try type_spec <|> try type_qualifier <|> storage_class_spec)

type_spec =     try (p_ident_s "void")
            <|> try (p_ident_s "char")
            <|> try (p_ident_s "short")
            <|> try (p_ident_s "int")
            <|> try (p_ident_s "long")
            <|> try (p_ident_s "float")
            <|> try (p_ident_s "double")
            <|> try (p_ident_s "signed")
            <|> p_ident_s "unsigned"

type_qualifier =     try (p_ident_s "const")
                 <|> try (p_ident_s "restrict")
                 <|> try (p_ident_s "volatile")

storage_class_spec =     try (p_ident_s "typedef")
                     <|> try (p_ident_s "extern")
                     <|> try (p_ident_s "static")
                     <|> try (p_ident_s "auto")
                     <|> p_ident_s "register"

type_qualifier_list = many1 type_qualifier

pointer = many1 $ p_op Asterisk *> many type_qualifier

init_declarator = declarator

declarator = optional pointer *> direct_declarator

direct_declarator = do ident_or_decl >>= rest
  where
    ident_or_decl = try p_ident <|> p_op LeftParenthes *> declarator <* p_op RightParenthes
    rest p   = try (helper p >>= rest) <|> return p
    helper p =     try (p_op LeftBracket *> (option NullExpr assignment_e) *> p_op RightBracket *> return p)
               <|> try (p_op LeftParenthes *> parameter_type_list *> p_op RightParenthes *> return p)
               <|> try (p_op LeftParenthes *> opt_ident_list *> p_op RightParenthes *> return p)
    opt_ident_list = sepBy p_ident (p_op Comma)
    

parameter_type_list = do p <- parameter_list
                         try (parameter_list *> (optional $ p_op Comma *> p_op ThreeDots) *> return p)
                           <|> return p
                         

parameter_list = sepBy1 parameter_declaration (p_op Comma)

parameter_declaration = declaration_spec *> declarator

initializer = assignment_e

function_definition = do
  declaration_spec
  declarator
  many declaration
  block_stmt

external_declaration = try (function_definition *> return ()) <|> (declaration *> return ())

translation_unit = many1 external_declaration


min_parser = statement <* eof

main = getContents >>= run

