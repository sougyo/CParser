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
    e_const  = EConst  <$> p_const
    e_ident  = EIdent  <$> p_ident
    e_string = EString <$> p_string
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

declaration_specs = many1 declaration_spec

declaration_spec = 
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

pointer = Pointer <$> many1 helper
  where
    helper = p_op Asterisk *> many type_qualifier

data Declarator =
    DeclaratorN DirectDeclarator
  | DeclaratorP Pointer DirectDeclarator
  deriving (Show, Eq)

declarator = try decl_p <|> decl_n
  where
    decl_p = DeclaratorP <$> pointer <*> direct_declarator
    decl_n = DeclaratorN <$> direct_declarator

data DirectDeclarator = 
    DD_Ident String
  | DD_Declarator Declarator
  | DD_BracketE   DirectDeclarator Expr
  | DD_ParenthesP DirectDeclarator ParameterTypeList
  | DD_ParenthesI DirectDeclarator [String]
  deriving (Show, Eq)

direct_declarator = (try ident <|> decl) >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    ident    = DD_Ident <$> p_ident
    decl     = parenthes $ DD_Declarator <$> declarator
    helper p =     try (bracket   $ return (DD_BracketE p NullExpr))
               <|> try (bracket   $ DD_BracketE   <$> return p <*> cond_e)
               <|> try (parenthes $ return (DD_ParenthesP p $ ParameterTypeList False []))
               <|> try (parenthes $ DD_ParenthesP <$> return p <*> parameter_type_list)
               <|>     (parenthes $ DD_ParenthesI <$> return p <*> sepBy p_ident (p_op Comma))
    bracket   = between (p_op LeftBracket)   (p_op RightBracket)
    parenthes = between (p_op LeftParenthes) (p_op RightParenthes)

data AbstractDeclarator = AbstractDeclarator Pointer DirectAbstractDeclarator
  deriving (Show, Eq)

abstract_declarator =     try (AbstractDeclarator <$> pointer             <*> direct_abstract_declarator)
                      <|> try (AbstractDeclarator <$> pointer             <*> return AD_Null)
                      <|>      AbstractDeclarator <$> return (Pointer []) <*> direct_abstract_declarator

data DirectAbstractDeclarator =
    AD_Abstract  AbstractDeclarator
  | AD_Bracket   DirectAbstractDeclarator Expr
  | AD_Parenthes DirectAbstractDeclarator ParameterTypeList
  | AD_Null
  deriving (Show, Eq)

direct_abstract_declarator = head_decl >>= rest
  where
    rest p    = try (post p >>= rest) <|> return p
    head_decl =     try (AD_Abstract <$> parenthes abstract_declarator)
                <|> post AD_Null
    bracket   = between (p_op LeftBracket)   (p_op RightBracket)
    parenthes = between (p_op LeftParenthes) (p_op RightParenthes)
    post p    =     try (bracket   $ return (AD_Bracket p NullExpr))
                <|> try (bracket   $ AD_Bracket   <$> return p <*> cond_e)
                <|> try (parenthes $ return (AD_Parenthes p $ ParameterTypeList False []))
                <|>     (parenthes $ AD_Parenthes <$> return p <*> parameter_type_list)

init_declarator = declarator

data ParameterTypeList = ParameterTypeList Bool [ParameterDeclaration]
  deriving (Show, Eq)

parameter_type_list = parameter_list >>= \p ->
                        try (ellipsis *> return (ParameterTypeList True  p)) <|>
                                         return (ParameterTypeList False p)
  where
    ellipsis = p_op Comma *> p_op ThreeDots
  
parameter_list = sepBy1 parameter_declaration $ p_op Comma

data ParameterDeclaration =
    ParameterDeclarationD [DeclarationSpecifier] Declarator
  | ParameterDeclarationA [DeclarationSpecifier] AbstractDeclarator
  | ParameterDeclarationN [DeclarationSpecifier]
  deriving (Show, Eq)

parameter_declaration =  declaration_specs >>= \d ->
                              try    (ParameterDeclarationD d <$> declarator)
                          <|> try    (ParameterDeclarationA d <$> abstract_declarator)
                          <|> return (ParameterDeclarationN d)

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

