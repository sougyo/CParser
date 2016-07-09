import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Control.Monad (sequence, when)
import Control.Monad.State.Lazy
import Text.Parsec

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
  | Dot
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
  | Ptr
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
  | K_Sizeof
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
  ("default" , K_Default ),
  ("sizeof"  , K_Sizeof  )
  ]

data Token =
    Constant String
  | Identifier String
  | StringLiteral String
  | Operator OperatorType
  | Keyword KeywordType
  | NullToken
  deriving (Show, Eq)

data PosToken = PosToken {
  get_pos   :: SourcePos,
  get_token :: Token 
} deriving (Show, Eq)

token_parser = fmap reject_null . many1 $ PosToken <$> getPosition <*> makeToken <* spaces
  where
    reject_null = filter $ \t -> get_token t /= NullToken
    makeToken =
          try comment
      <|> try (Constant <$> many1 digit)
      <|> try keyword_or_identifier
      <|> try (StringLiteral <$> string_literal)
      <|> Operator <$> operator
    operator  =
          try (string "..." *> return ThreeDots)
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
      <|> try (string "->"  *> return Ptr)
      <|> char '=' *> return Equal
      <|> char '+' *> return Plus
      <|> char '-' *> return Minus
      <|> char '*' *> return Asterisk
      <|> char '/' *> return Slash
      <|> char '%' *> return Percent
      <|> char '<' *> return LessThan
      <|> char '>' *> return GreaterThan
      <|> char '~' *> return Tilde
      <|> char '&' *> return And
      <|> char '|' *> return Or
      <|> char '^' *> return Hat
      <|> char '.' *> return Dot
      <|> char ':' *> return Colon
      <|> char ';' *> return Semicolon
      <|> char '?' *> return Question
      <|> char '!' *> return Exclamation
      <|> char '(' *> return LeftParenthes
      <|> char ')' *> return RightParenthes
      <|> char '{' *> return LeftBrace
      <|> char '}' *> return RightBrace
      <|> char '[' *> return LeftBracket
      <|> char ']' *> return RightBracket
      <|> char ',' *> return Comma

comment = string "/*" *> helper
  where
    helper = try (string "*/" *> return NullToken) <|> anyChar *> helper

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
    escape_sequence =
          try (string "\\'"  *> return '\'')
      <|> try (string "\\\"" *> return '"' )
      <|> try (string "\\a"  *> return '\a')
      <|> try (string "\\b"  *> return '\b')
      <|> try (string "\\f"  *> return '\f')
      <|> try (string "\\n"  *> return '\n')
      <|> try (string "\\r"  *> return '\r')
      <|> try (string "\\t"  *> return '\t')
      <|>      string "\\v"  *> return '\v'

gen_p f  = tokenPrim (\c -> show c) (\pos c _cs -> get_pos c) $ f . get_token
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
oneOf_kwd kwds = gen_p $ \n -> case n of
              Keyword k -> if elem k kwds then Just k else Nothing
              _         -> Nothing
parse_fail = gen_p $ \_ -> Nothing
 
data Expr =
    EIdent    String
  | EConst    String
  | EString   String
  | TernaryOp Expr Expr Expr
  | BinOp     OperatorType Expr Expr
  | PostfixE  Expr Expr
  | PostfixA  Expr [Expr]
  | PostfixD  Expr String
  | PostfixP  Expr String
  | PostfixO  Expr OperatorType
  | UnaryC    OperatorType Expr
  | UnaryO    OperatorType Expr
  | UnaryS    Expr
  | UnaryT    TypeName
  | CastU     Expr
  | CastT     TypeName Expr
  | NullExpr
  deriving (Show, Eq)

data Declaration = Declaration [DeclarationSpecifier] [InitDeclarator]
  deriving (Show, Eq)

data InitDeclarator =
    InitDeclaratorD Declarator
  | InitDeclaratorI Declarator Initializer
  deriving (Show, Eq)

data StructDeclaration = StructDeclaration [DeclarationSpecifier] [StructDeclarator]
  deriving (Show, Eq)

data StructDeclarator = StructDeclarator Declarator Expr
  deriving (Show, Eq)

data DeclarationSpecifier =
    TypeSpecifier KeywordType
  | TypeQualifier KeywordType
  | TypeStorage   KeywordType
  | TypeStructUnion KeywordType String [StructDeclaration]
  | TypeTypedef   String
  deriving (Show, Eq)

data Declarator =
    Declarator  Pointer DirectDeclarator
  | DeclaratorN
  deriving (Show, Eq)

data DirectDeclarator = 
    DirectDeclaratorI String
  | DirectDeclaratorD Declarator
  | DirectDeclaratorE DirectDeclarator Expr
  | DirectDeclaratorP DirectDeclarator ParameterTypeList
  | DirectDeclaratorL DirectDeclarator [String]
  deriving (Show, Eq)

data Pointer =
    Pointer  [[DeclarationSpecifier]]
  | PointerN
  deriving (Show, Eq)

data ParameterTypeList = ParameterTypeList Bool [ParameterDeclaration]
  deriving (Show, Eq)

data ParameterDeclaration =
    ParameterDeclarationD [DeclarationSpecifier] Declarator
  | ParameterDeclarationA [DeclarationSpecifier] AbstractDeclarator
  | ParameterDeclarationN [DeclarationSpecifier]
  deriving (Show, Eq)

data TypeName = TypeName [DeclarationSpecifier] AbstractDeclarator
  deriving (Show, Eq)

data AbstractDeclarator =
    AbstractDeclarator  Pointer DirectAbstractDeclarator
  | AbstractDeclaratorN
  deriving (Show, Eq)

data DirectAbstractDeclarator =
    DirectAbstractA AbstractDeclarator
  | DirectAbstractB DirectAbstractDeclarator Expr
  | DirectAbstractP DirectAbstractDeclarator ParameterTypeList
  | DirectAbstractN
  deriving (Show, Eq)

data Initializer = 
    InitializerA Expr
  | InitializerI [Initializer]
  deriving (Show, Eq)

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

data ExternalDeclaration =
    ExternalDeclarationF FunctionDefinition
  | ExternalDeclarationD Declaration
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition [DeclarationSpecifier] Declarator [Declaration] Statement
  deriving (Show, Eq)


binOp p = BinOp <$> p_op p
between_brackets  = between (p_op LeftBracket)   (p_op RightBracket)
between_braces    = between (p_op LeftBrace)     (p_op RightBrace)
between_parenthes = between (p_op LeftParenthes) (p_op RightParenthes)
expr_between_parenthes = between_parenthes expression

primary_e =
      try (EConst  <$> p_const)
  <|> try (EIdent  <$> p_ident)
  <|> try (EString <$> p_string)
  <|>      expr_between_parenthes

postfix_e = primary_e >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    helper p = 
          try (PostfixE p <$> between_brackets expression)
      <|> try (PostfixA p <$> between_parenthes argument_expression_list)
      <|> try (PostfixD p <$> (p_op Dot *> p_ident))
      <|> try (PostfixP p <$> (p_op Ptr *> p_ident))
      <|>     (PostfixO p <$> (p_op PlusPlus <|> p_op MinusMinus))
    argument_expression_list = sepBy assignment_e (p_op Comma)

unary_e =
      try postfix_e
  <|> try (UnaryO <$> p_op PlusPlus   <*> unary_e)
  <|> try (UnaryO <$> p_op MinusMinus <*> unary_e)
  <|> try (UnaryC <$> unary_operator  <*> cast_e )
  <|> try (UnaryS <$> (p_kwd K_Sizeof  *> unary_e))
  <|>     (UnaryT <$> (p_kwd K_Sizeof  *>
             between_parenthes type_name))
  where
    unary_operator =
          p_op And
      <|> p_op Asterisk
      <|> p_op Plus
      <|> p_op Minus
      <|> p_op Tilde
      <|> p_op Exclamation

cast_e = try unary_p <|> type_p
  where
    unary_p = CastU <$> unary_e
    type_p  = CastT <$> between_parenthes type_name <*> cast_e

multiplicative_e = chainl1 cast_e
    $ binOp Asterisk
  <|> binOp Slash
  <|> binOp Percent

additive_e = chainl1 multiplicative_e
    $ binOp Plus
  <|> binOp Minus

shift_e = chainl1 additive_e
    $ binOp LeftShift
  <|> binOp RightShift

relation_e = chainl1 shift_e
    $ binOp LessThan
  <|> binOp GreaterThan
  <|> binOp LessThanEqual
  <|> binOp GreaterThanEqual

equal_e = chainl1 relation_e
    $ binOp EqualEqual
  <|> binOp NotEqual

and_e = chainl1 equal_e
    $ binOp And

exor_e = chainl1 and_e
    $ binOp Hat

or_e = chainl1 exor_e
    $ binOp Or

land_e = chainl1 or_e
    $ binOp AndAnd

lor_e = chainl1 land_e
    $ binOp OrOr

cond_e = lor_e >>= rest
  where
    rest p    = try (ternary p) <|> return p
    ternary p = TernaryOp p <$> (p_op Question *> expression <* p_op Colon) <*> cond_e

assignment_e = try assign_e <|> cond_e
  where
    assign_e = do u  <- unary_e
                  op <- assign_op
                  op u <$> assignment_e
    assign_op =
          binOp Equal
      <|> binOp AsteriskEqual
      <|> binOp SlashEqual
      <|> binOp PercentEqual
      <|> binOp PlusEqual
      <|> binOp MinusEqual
      <|> binOp LeftShiftEqual
      <|> binOp RightShiftEqual
      <|> binOp AndEqual
      <|> binOp HatEqual
      <|> binOp OrEqual

expression = chainl1 assignment_e $ binOp Comma


-- Declarations

declaration = _declaration >>= search_and_put_typedef_type
  where
    _declaration = Declaration <$> declaration_specs <*> init_declarator_list <* p_op Semicolon
    init_declarator_list = sepBy init_declarator (p_op Comma)

search_and_put_typedef_type d = search_and_put_ident d >> return d 
  where
    search_and_put_ident (Declaration specs idecls) =
        if elem (TypeStorage K_Typedef) specs then put_idecls idecls else return ()
    put_idecls idecls = let new_typedefs = concat $ map search_idecl idecls in
                          lift get >>= \old_typedefs -> (lift . put $ old_typedefs ++ new_typedefs)
    search_idecl (InitDeclaratorD d  ) = search_decl  d
    search_idecl (InitDeclaratorI d _) = search_decl  d
    search_decl  (Declarator      _ d) = search_ddecl d
    search_decl  _                     = []
    search_ddecl ddecl = case ddecl of
      DirectDeclaratorI i   -> [i]
      DirectDeclaratorD d   -> search_decl  d
      DirectDeclaratorE d _ -> search_ddecl d
      DirectDeclaratorP d _ -> search_ddecl d
      DirectDeclaratorL d _ -> search_ddecl d

declaration_specs = many1 $
      try type_qualifier
  <|> try storage_class_spec
  <|>     type_spec

init_declarator = declarator >>= \d ->
      try (InitDeclaratorI d <$> (p_op Equal *> initializer))
  <|>      return (InitDeclaratorD d)

storage_class_spec = fmap TypeStorage $ oneOf_kwd
  [ K_Typedef ,
    K_Extern  ,
    K_Static  ,
    K_Auto    ,
    K_Register
  ]

type_spec =
      try (TypeSpecifier <$> k_type_specifier)
  <|> try (struct_or_union_specifier)
  <|>      typedef_p
  where
    k_type_specifier = oneOf_kwd
      [ K_Void    ,
        K_Char    ,
        K_Short   ,
        K_Int     ,
        K_Long    ,
        K_Float   ,
        K_Double  ,
        K_Signed  ,
        K_Unsigned
      ]
    typedef_p = try $
      do p   <- p_ident
         kws <- lift get
         if elem p kws then return (TypeTypedef p) else parse_fail

struct_or_union_specifier = oneOf_kwd [K_Struct, K_Union] >>= \k ->
      try (TypeStructUnion k <$> option [] p_ident <*> between_braces (many1 struct_declaration))
  <|>     (TypeStructUnion k <$> p_ident           <*> return [])

struct_declaration = StructDeclaration <$> specifier_qualifier_list
                       <*> sepBy1 struct_declarator (p_op Comma) <* p_op Semicolon

specifier_qualifier_list = many1 $ try type_qualifier <|> type_spec

struct_declarator =
      try (StructDeclarator <$> option DeclaratorN declarator <* p_op Colon <*> cond_e)
  <|>     (StructDeclarator <$> declarator <*> return NullExpr)

type_qualifier = fmap TypeQualifier $ oneOf_kwd [K_Const, K_Volatile]

declarator = Declarator <$> option PointerN pointer <*> direct_declarator

direct_declarator = (try ident <|> decl) >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    ident    = DirectDeclaratorI <$> p_ident
    decl     = between_parenthes $ DirectDeclaratorD <$> declarator
    helper p =
          try (between_brackets  $ DirectDeclaratorE p <$> option NullExpr cond_e)
      <|> try (between_parenthes $ DirectDeclaratorL p <$> sepBy p_ident (p_op Comma))
      <|>     (between_parenthes $ DirectDeclaratorP p <$> option null_parm parameter_type_list)
    null_parm = ParameterTypeList False []

pointer = Pointer <$> many1 (p_op Asterisk *> many type_qualifier)

type_qualifier_list = many1 type_qualifier

parameter_type_list = parameter_list >>= \p ->
      try (ellipsis *> return (ParameterTypeList True  p))
  <|>                  return (ParameterTypeList False p)
  where
    ellipsis = p_op Comma *> p_op ThreeDots

parameter_list = sepBy1 parameter_declaration $ p_op Comma

parameter_declaration =  declaration_specs >>= \d ->
      try    (ParameterDeclarationD d <$> declarator)
  <|> try    (ParameterDeclarationA d <$> abstract_declarator)
  <|> return (ParameterDeclarationN d)

type_name = TypeName <$>
              specifier_qualifier_list <*> option AbstractDeclaratorN abstract_declarator

abstract_declarator = 
      try (AbstractDeclarator <$> pointer             <*> direct_abstract_declarator)
  <|> try (AbstractDeclarator <$> pointer             <*> return DirectAbstractN)
  <|>      AbstractDeclarator <$> return (Pointer []) <*> direct_abstract_declarator

direct_abstract_declarator = head_decl >>= rest
  where
    rest p    = try (post p >>= rest) <|> return p
    head_decl =
          try (DirectAbstractA <$> between_parenthes abstract_declarator)
      <|> post DirectAbstractN
    post p    =
          try (between_brackets  $ DirectAbstractB p <$> option NullExpr  cond_e)
      <|>     (between_parenthes $ DirectAbstractP p <$> option null_parm parameter_type_list)
    null_parm = ParameterTypeList False []

initializer =
      try (InitializerA <$> assignment_e)
  <|>      between_braces (initializer_list <* optional (p_op Comma))
  where
    initializer_list = InitializerI <$> (sepBy1 initializer $ p_op Comma)


-- Statements

statement =
      try labeled_statement
  <|> try compound_statement
  <|> try expression_statement
  <|> try selection_statement
  <|> try iteration_statement
  <|>     jump_statement

labeled_statement =
      try case_stmt
  <|> try default_stmt
  <|>     id_stmt
  where
    case_stmt    = StatementCase    <$> (p_kwd K_Case *> cond_e <* p_op Colon) <*> statement
    default_stmt = StatementDefault <$> (p_kwd K_Default *> p_op Colon *> statement)
    id_stmt      = StatementId      <$> (p_ident <* p_op Colon) <*> statement

compound_statement = between_braces block_item_list

block_item_list = fmap StatementB $ many block_item
  where
    block_item =
          try (BlockItemD <$> declaration)
      <|>     (BlockItemS <$> statement)

expression_statement = StatementExpr <$> (option NullExpr expression <* p_op Semicolon)

selection_statement = try if_stmt <|> switch_stmt
  where
    if_stmt     = p_kwd K_If     *> (StatementIf     <$> expr_between_parenthes <*> statement <*> else_p)
    switch_stmt = p_kwd K_Switch *> (StatementSwitch <$> expr_between_parenthes <*> statement)
    else_p      = option StatementN (p_kwd K_Else *> statement)

iteration_statement =
      try while_stmt 
  <|> try do_stmt
  <|>     for_stmt
  where
    while_stmt  = p_kwd K_While *> (StatementWhile <$> expr_between_parenthes <*> statement)
    do_stmt     = p_kwd K_Do    *> (StatementDo  <$> statement)
                    <* p_kwd K_While <*> expr_between_parenthes <* p_op Semicolon
    for_stmt    = p_kwd K_For   *> between_parenthes for_expr <*> statement
    for_expr    = StatementFor <$> expression_statement <*> expression_statement <*> option NullExpr expression

jump_statement = helper <* p_op Semicolon
  where
    helper =
          try goto_stmt
      <|> try cont_stmt
      <|> try break_stmt
      <|>     return_stmt
    goto_stmt   = StatementGoto   <$> (p_kwd K_Goto *> p_ident)
    cont_stmt   = p_kwd K_Continue *> return StatementContinue
    break_stmt  = p_kwd K_Break    *> return StatementBreak
    return_stmt = StatementReturn <$> (p_kwd K_Return *> option NullExpr expression)


-- External definitions

translation_unit = many1 external_declaration

external_declaration =
      try (ExternalDeclarationF <$> function_definition)
  <|>     (ExternalDeclarationD <$> declaration)

function_definition = FunctionDefinition <$>
      declaration_specs
  <*> declarator
  <*> many declaration
  <*> compound_statement


-- Main

min_parser = translation_unit <* eof

run input = case parse (spaces *> token_parser <* eof) "" input of
              Right val -> putStrLn $ show $ helper val
              Left  err -> putStrLn $ show err
  where
    helper val = evalState (runParserT min_parser () "" val) []

main = getContents >>= run

