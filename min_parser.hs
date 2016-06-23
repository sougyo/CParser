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
    K_If
  | K_Else 
  | K_Void
  | K_Char
  | K_Short
  | K_Int
  | K_Long
  | K_Float
  | K_Double
  | K_Signed
  | K_Unsigned
  | K_Struct
  | K_Union
  | K_Enum
  | K_Typedef
  | K_Extern
  | K_Static
  | K_Auto
  | K_Register
  | K_Const
  | K_Volatile
  deriving (Show, Eq)

keyword_dictionary = [
  ("if"      , K_If      ),
  ("else"    , K_Else    ),
  ("void"    , K_Void    ),
  ("char"    , K_Char    ),
  ("short"   , K_Short   ),
  ("int"     , K_Int     ),
  ("long"    , K_Long    ),
  ("float"   , K_Float   ),
  ("double"  , K_Double  ),
  ("signed"  , K_Signed  ),
  ("unsigned", K_Unsigned),
  ("struct"  , K_Struct  ),
  ("union"   , K_Union   ),
  ("enum"    , K_Enum    ),
  ("typedef" , K_Typedef ),
  ("extern"  , K_Extern  ),
  ("static"  , K_Static  ),
  ("auto"    , K_Auto    ),
  ("register", K_Register),
  ("const"   , K_Const   ),
  ("volatile", K_Volatile)
  ]

data CType = 
    Cvoid
  | Cchar
  | Cshort
  | Cint
  | Clong
  | Cfloat
  | Cdouble
  | Csigned
  | Cunsigned

data C


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

selection_stmt = do p_kwd K_If 
                    e  <- p_op LeftParenthes *> expr <* p_op RightParenthes
                    s1 <- statement
                    s2 <- option NullStatement (p_kwd K_Else *> statement)
                    return $ IfStatement e s1 s2

block_stmt = p_op LeftBrace *> block_items <* p_op RightBrace

block_items = fmap BlkStatement $ many (StatementBlockItem <$> statement)

expr_stmt = fmap ExprStatement (option NullExpr expr <* p_op Semicolon)

declaration = do declaration_spec
                 sepBy1 init_declarator (p_op Comma)
                 p_op Semicolon

data DeclarationSpecifier =
    DS_Type      TypeSpecifier
  | DS_Qualifier TypeQualifier
  | DS_Storage   TypeStorage
  deriving (Show, Eq)

declaration_spec = many1 $
      try (DS_Type      <$> type_spec)
  <|> try (DS_Qualifier <$> type_qualifier)
  <|>      DS_Storage   <$> storage_class_spec

data TypeSpecifier =
    TypeKeyword KeywordType
  deriving (Show, Eq)

data TypeQualifier =
    QualifierKeyword KeywordType
  deriving (Show, Eq)

data TypeStorage =
    StorageKeyword KeywordType
  deriving (Show, Eq)

type_spec = fmap TypeKeyword $ 
      try (p_kwd K_Void)
  <|> try (p_kwd K_Char)
  <|> try (p_kwd K_Short)
  <|> try (p_kwd K_Int)
  <|> try (p_kwd K_Long)
  <|> try (p_kwd K_Float)
  <|> try (p_kwd K_Double)
  <|> try (p_kwd K_Signed)
  <|>      p_kwd K_Unsigned

type_qualifier = fmap QualifierKeyword $
      try (p_kwd K_Const)
  <|>      p_kwd K_Volatile

storage_class_spec = fmap StorageKeyword $
      try (p_kwd K_Typedef)
  <|> try (p_kwd K_Extern)
  <|> try (p_kwd K_Static)
  <|> try (p_kwd K_Auto)
  <|>      p_kwd K_Register

type_qualifier_list = many1 type_qualifier

data Pointer = Pointer [[TypeQualifier]]
  deriving (Show, Eq)

pointer = fmap Pointer $ many1 $ p_op Asterisk *> many type_qualifier

data Declarator =
    DeclaratorN DirectDeclarator
  | DeclaratorP Pointer DirectDeclarator
  deriving (Show, Eq)

declarator = optional pointer *> direct_declarator

data DirectDeclarator = DirectDeclarator
  deriving (Show, Eq)

direct_declarator = do ident_or_decl >>= rest
  where
    ident_or_decl = try p_ident <|> p_op LeftParenthes *> declarator <* p_op RightParenthes
    rest p   = try (helper p >>= rest) <|> return p
    helper p =     try (p_op LeftBracket *> (option NullExpr assignment_e) *> p_op RightBracket *> return p)
               <|> try (p_op LeftParenthes *> parameter_type_list *> p_op RightParenthes *> return p)
               <|> try (p_op LeftParenthes *> opt_ident_list *> p_op RightParenthes *> return p)
    opt_ident_list = sepBy p_ident (p_op Comma)
    

init_declarator = declarator

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

