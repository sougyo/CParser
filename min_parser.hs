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
  | K_Switch
  | K_While
  | K_Do
  | K_For
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
  | K_Goto
  | K_Continue
  | K_Break
  | K_Return
  | K_Case
  | K_Default
  deriving (Show, Eq)

keyword_dictionary = [
  ("if"      , K_If      ),
  ("else"    , K_Else    ),
  ("switch"  , K_Switch  ),
  ("while"   , K_While   ),
  ("do"      , K_Do      ),
  ("for"     , K_For     ),
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
  ("volatile", K_Volatile),
  ("goto"    , K_Goto    ),
  ("continue", K_Continue),
  ("break"   , K_Break   ),
  ("return"  , K_Return  ),
  ("case"    , K_Case    ),
  ("default" , K_Default )
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

data BlockItem =
    BlockItemS Statement
  | BlockItemD Declaration
  deriving (Show, Eq)

data Statement = 
    StatementExpr     Expr
  | StatementIf       Expr Statement Statement
  | StatementSwitch   Expr Statement
  | StatementDo       Statement Expr
  | StatementWhile    Expr Statement
  | StatementFor      Statement Statement Expr Statement
  | StatementGoto     String
  | StatementContinue
  | StatementBreak
  | StatementReturn   Expr
  | StatementCase     Expr Statement
  | StatementDefault  Statement
  | StatementId       String Statement
  | StatementB        [BlockItem]
  | StatementN
  deriving (Show, Eq)

statement =     try labeled_statement
            <|> try compound_statement
            <|> try expression_statement
            <|> try selection_statement
            <|> try iteration_statement
            <|>     jump_statement

labeled_statement =     try case_stmt
                    <|> try default_stmt
                    <|>     id_stmt
  where
    case_stmt    = StatementCase    <$> (p_kwd K_Case *> cond_e <* p_op Colon) <*> statement
    default_stmt = StatementDefault <$> (p_kwd K_Default *> p_op Colon *> statement)
    id_stmt      = StatementId      <$> (p_ident <* p_op Colon) <*> statement

selection_statement = try if_stmt <|> switch_stmt
  where
    if_stmt     = p_kwd K_If     *> (StatementIf     <$> expr_p <*> statement <*> else_p)
    switch_stmt = p_kwd K_Switch *> (StatementSwitch <$> expr_p <*> statement)
    expr_p      = between (p_op LeftParenthes) (p_op RightParenthes) expr
    else_p      = option StatementN (p_kwd K_Else *> statement)

iteration_statement =     try while_stmt 
                      <|> try do_stmt
                      <|>     for_stmt
  where
    while_stmt  = p_kwd K_While *> (StatementWhile <$> expr_p <*> statement)
    do_stmt     = p_kwd K_Do    *> do_inner <* p_kwd K_While
                    <*> expr_p <* p_op Semicolon
    for_stmt    = p_kwd K_For   *>
                        between (p_op LeftParenthes) (p_op RightParenthes) for_expr
                    <*> statement
    do_inner    = StatementDo  <$> statement
    for_expr    = StatementFor <$> expression_statement <*> expression_statement <*> option NullExpr expr
    expr_p      = between (p_op LeftParenthes) (p_op RightParenthes) expr

jump_statement = helper <* p_op Semicolon
  where
    helper      =     try goto_stmt
                  <|> try cont_stmt
                  <|> try break_stmt
                  <|>     return_stmt
    goto_stmt   = StatementGoto   <$> (p_kwd K_Goto *> p_ident)
    cont_stmt   = p_kwd K_Continue *> return StatementContinue
    break_stmt  = p_kwd K_Break    *> return StatementBreak
    return_stmt = StatementReturn <$> (p_kwd K_Return *> option NullExpr expr)

compound_statement = p_op LeftBrace *> block_item_list <* p_op RightBrace

block_item_list = fmap StatementB $ many block_item
  where
    block_item =     try (BlockItemD <$> declaration)
                 <|>     (BlockItemS <$> statement)

expression_statement = StatementExpr <$> (option NullExpr expr <* p_op Semicolon)


data Declaration = Declaration DeclarationSpecifier [InitDeclarator]
  deriving (Show, Eq)

declaration = Declaration <$> declaration_spec <*> init_declarator_list <* p_op Semicolon
  where
    init_declarator_list = sepBy init_declarator (p_op Comma)

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
      p_kwd K_Void
  <|> p_kwd K_Char
  <|> p_kwd K_Short
  <|> p_kwd K_Int
  <|> p_kwd K_Long
  <|> p_kwd K_Float
  <|> p_kwd K_Double
  <|> p_kwd K_Signed
  <|> p_kwd K_Unsigned

type_qualifier = fmap QualifierKeyword $
      p_kwd K_Const
  <|> p_kwd K_Volatile

storage_class_spec = fmap StorageKeyword $
      p_kwd K_Typedef
  <|> p_kwd K_Extern
  <|> p_kwd K_Static
  <|> p_kwd K_Auto
  <|> p_kwd K_Register

type_qualifier_list = many1 type_qualifier

data Pointer =
    Pointer  [[TypeQualifier]]
  | PointerN
  deriving (Show, Eq)

pointer = Pointer <$> many1 helper
  where
    helper = p_op Asterisk *> many type_qualifier

data Declarator = Declarator Pointer DirectDeclarator
  deriving (Show, Eq)

declarator = Declarator <$> option PointerN pointer <*> direct_declarator

data DirectDeclarator = 
    DirectDeclaratorI String
  | DirectDeclaratorD Declarator
  | DirectDeclaratorE DirectDeclarator Expr
  | DirectDeclaratorP DirectDeclarator ParameterTypeList
  | DirectDeclaratorL DirectDeclarator [String]
  deriving (Show, Eq)

direct_declarator = (try ident <|> decl) >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    ident    = DirectDeclaratorI <$> p_ident
    decl     = parenthes $ DirectDeclaratorD <$> declarator
    helper p =     try (bracket   $ DirectDeclaratorE <$> return p <*> option NullExpr cond_e)
               <|> try (parenthes $ DirectDeclaratorL <$> return p <*> sepBy p_ident (p_op Comma))
               <|> try (parenthes $ DirectDeclaratorP <$> return p <*> option null_parm parameter_type_list)
    bracket   = between (p_op LeftBracket)   (p_op RightBracket)
    parenthes = between (p_op LeftParenthes) (p_op RightParenthes)
    null_parm = ParameterTypeList False []

data AbstractDeclarator = AbstractDeclarator Pointer DirectAbstractDeclarator
  deriving (Show, Eq)

abstract_declarator =     try (AbstractDeclarator <$> pointer             <*> direct_abstract_declarator)
                      <|> try (AbstractDeclarator <$> pointer             <*> return DirectAbstractN)
                      <|>      AbstractDeclarator <$> return (Pointer []) <*> direct_abstract_declarator

data DirectAbstractDeclarator =
    DirectAbstractA AbstractDeclarator
  | DirectAbstractB DirectAbstractDeclarator Expr
  | DirectAbstractP DirectAbstractDeclarator ParameterTypeList
  | DirectAbstractN
  deriving (Show, Eq)

direct_abstract_declarator = head_decl >>= rest
  where
    rest p    = try (post p >>= rest) <|> return p
    head_decl =     try (DirectAbstractA <$> parenthes abstract_declarator)
                <|> post DirectAbstractN
    bracket   = between (p_op LeftBracket)   (p_op RightBracket)
    parenthes = between (p_op LeftParenthes) (p_op RightParenthes)
    post p    =     try (bracket   $ DirectAbstractB <$> return p <*> option NullExpr  cond_e)
                <|>     (parenthes $ DirectAbstractP <$> return p <*> option null_parm parameter_type_list)
    null_parm = ParameterTypeList False []

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

data InitDeclarator =
    InitDeclaratorD Declarator
  | InitDeclaratorI Declarator Initializer
  deriving (Show, Eq)

init_declarator = declarator >>= \d ->
                        try    (InitDeclaratorI d <$> (p_op Equal *> initializer))
                    <|> return (InitDeclaratorD d)

data Initializer = 
    InitializerA Expr
  | InitializerI [Initializer]
  deriving (Show, Eq)

initializer =     try (InitializerA <$> assignment_e)
              <|> p_op LeftBrace *> initializer_list <* optional (p_op Comma) <* p_op RightBrace
  where
    initializer_list = InitializerI <$> (sepBy1 initializer $ p_op Comma)

data FunctionDefinition = FunctionDefinition [DeclarationSpecifier] Declarator [Declaration] Statement
  deriving (Show, Eq)

function_definition = FunctionDefinition <$>
      many declaration_spec
  <*> declarator
  <*> many declaration
  <*> compound_statement

data ExternalDeclaration =
    ExternalDeclarationF FunctionDefinition
  | ExternalDeclarationD Declaration
  deriving (Show, Eq)

external_declaration =      try (ExternalDeclarationF <$> function_definition)
                       <|>      (ExternalDeclarationD <$> declaration)

translation_unit = many1 external_declaration

min_parser = translation_unit <* eof

main = getContents >>= run

