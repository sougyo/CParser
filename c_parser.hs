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
  ("__const" , K_Const   ),
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
    Constant      ConstantType
  | Identifier    String
  | StringLiteral String
  | Operator      OperatorType
  | Keyword       KeywordType
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
      <|> try ignore_attribute_or_asm
      <|> try ignore_extension
      <|> try ignore_restrict
      <|> try (string "inline" *> return NullToken)
      <|> try (Constant <$> constant)
      <|> try keyword_or_identifier
      <|> try string_literal
      <|>     (Operator <$> operator)
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

comment = string "/*" *> rest
  where
    rest = try (string "*/" *> return NullToken) <|> anyChar *> rest

keyword_or_identifier = fmap choose identifier_str
  where
    choose s = case lookup s keyword_dictionary of
                 Just k  -> Keyword    k
                 Nothing -> Identifier s
    identifier_str = (:) <$> nondigit <*> many (nondigit <|> digit)
    nondigit = letter <|> char '_'

ignore_attribute_or_asm = attribute_or_asm *> spaces
                            *> between (char '(') (char ')') any_parenthes *> return NullToken
  where
    attribute_or_asm = try (string "__attribute__") <|> string "__asm__"
    any_parenthes = many helper *> return ()
    helper =
          try (between (char '(') (char ')') any_parenthes)
      <|> try (many1 (noneOf "()") *> return ())

ignore_extension = string "__extension__" *> return NullToken

ignore_restrict  = string "__restrict" *> return NullToken

data ConstantType =
    IntegerDecimalConstant      String [IntegerSuffix]
  | IntegerOctalConstant        String [IntegerSuffix]
  | IntegerHexadecimalConstant  String [IntegerSuffix]
  | FloatingDecimalConstant     String String String FloatingSuffix
  | FloatingHexadecimalConstant String String String FloatingSuffix
  | CharacterConstant           String
  deriving (Show, Eq)

data IntegerSuffix = UnsignedSuffix | LongSuffix | LongLongSuffix
  deriving (Show, Eq)

data FloatingSuffix = 
    FSuffix
  | LSuffix
  | NullFloatingSuffix
  deriving (Show, Eq)

constant =
      try floating_constant
  <|> try integer_constant
  <|>     character_constant

integer_constant = constant_part <*> option [] integer_suffix
  where
    constant_part =
          try (IntegerHexadecimalConstant <$> hexadecimal_constant)
      <|> try (IntegerOctalConstant       <$> octal_constant      )
      <|>     (IntegerDecimalConstant     <$> decimal_constant    )

decimal_constant     = (:) <$> nonzero_digit <*> many digit

octal_constant       = char '0'               *> many octDigit

hexadecimal_constant = hexadecimal_prefix     *> many hexDigit

hexadecimal_prefix = try (string "0x") <|> (string "0X")

integer_suffix =
      try ((++) <$> unsigned_p  <*> long_long_p           )
  <|> try ((++) <$> unsigned_p  <*> (option [] long_p)    )
  <|> try ((++) <$> long_long_p <*> (option [] unsigned_p))
  <|>     ((++) <$> long_p      <*> (option [] unsigned_p))
  where
    unsigned_suffix  = char        'u'   <|> char   'U'
    long_suffix      = char        'l'   <|> char   'L'
    long_long_suffix = try (string "ll") <|> string "LL"
    unsigned_p  = unsigned_suffix  *> return [UnsignedSuffix]
    long_p      = long_suffix      *> return [LongSuffix]
    long_long_p = long_long_suffix *> return [LongLongSuffix]

nonzero_digit = oneOf "123456789"

floating_constant = 
      try hexadecimal_floating_constant
  <|>     decimal_floating_constant 

construct_floating_constant_p f_type digit_p exp_prefix =
       try (f_type <$> (many  digit_p <* char '.') <*> many1 digit_p <*> option "" exponent_part <*> floating_suffix)
  <|>  try (f_type <$> (many1 digit_p <* char '.') <*> return ""     <*> option "" exponent_part <*> floating_suffix)
  <|>      (f_type <$>  many1 digit_p              <*> return ""     <*>           exponent_part <*> floating_suffix)
  where
    exponent_part = oneOf exp_prefix *> ((++) <$> option "" (string "+" <|> string "-") <*> many1 digit)

decimal_floating_constant =
    construct_floating_constant_p FloatingDecimalConstant digit "eE"

hexadecimal_floating_constant = hexadecimal_prefix *> 
    construct_floating_constant_p FloatingHexadecimalConstant hexDigit "pP"

floating_suffix = option NullFloatingSuffix $
      try (oneOf "lL" *> return LSuffix)
  <|>     (oneOf "fF" *> return FSuffix)

character_constant = CharacterConstant <$> helper
  where
    helper = optional (string "L")
               *> between (char '\'') (char '\'') c_char_sequence
    c_char_sequence = many1 c_char
    c_char = try (noneOf "'\\\n") <|> escape_sequence

string_literal = StringLiteral <$> helper
  where
    helper = optional (string "L")
               *> between (char '"') (char '"') s_char_sequence
    s_char_sequence = many s_char
    s_char = try (noneOf "\"\\\n") <|> escape_sequence

escape_sequence =
      try simple_escape_sequence
  <|> try octal_escape_sequence
  <|> try hexadecimal_escape_sequence
  <|>     universal_character_name
    
simple_escape_sequence =
      try (string "\\'"  *> return '\'')
  <|> try (string "\\\"" *> return '"' )
  <|> try (string "\\a"  *> return '\a')
  <|> try (string "\\b"  *> return '\b')
  <|> try (string "\\f"  *> return '\f')
  <|> try (string "\\n"  *> return '\n')
  <|> try (string "\\r"  *> return '\r')
  <|> try (string "\\t"  *> return '\t')
  <|>      string "\\v"  *> return '\v'

octal_escape_sequence =
      try (char '\\' *> octDigit *> octDigit *> octDigit *> return '?')
  <|> try (char '\\' *> octDigit *> octDigit             *> return '?')
  <|> try (char '\\' *> octDigit                         *> return '?')

hexadecimal_escape_sequence = string "\\x" *> many1 hexDigit *> return '?'

universal_character_name = (try (string "\\u") <|> string "\\U") *> many1 hex_quad *> return '?'
  where
    hex_quad = hexDigit *> hexDigit *> hexDigit *> hexDigit


gen_p f  = tokenPrim (\c -> show c) (\pos c _cs -> get_pos c) $ f . get_token
p_const  = gen_p $ \n -> case n of
              Constant s -> Just s
              _          -> Nothing
p_ident   = do gen_p $ \n -> case n of
                 Identifier s -> Just s
                 _            -> Nothing
p_typedef = do typedef_typef <- lift get
               gen_p $ \n -> case n of
                 Identifier s -> if elem s typedef_typef then Just s else Nothing
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
 
data Expr =
    EIdent    String
  | EConst    ConstantType
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

data Enumerator = Enumerator String Expr
  deriving (Show, Eq)

data DeclarationSpecifier =
    TypeSpecifier KeywordType
  | TypeQualifier KeywordType
  | TypeStorage   KeywordType
  | TypeStructUnion KeywordType String [StructDeclaration]
  | TypeEnum      String [Enumerator]
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

primary_expression =
      try (EConst  <$> p_const)
  <|> try (EIdent  <$> p_ident)
  <|> try (EString <$> p_string)
  <|>      expr_between_parenthes

postfix_expression = primary_expression >>= rest
  where
    rest p   = try (helper p >>= rest) <|> return p
    helper p = 
          try (PostfixE p <$> between_brackets expression)
      <|> try (PostfixA p <$> between_parenthes argument_expression_list)
      <|> try (PostfixD p <$> (p_op Dot *> p_ident))
      <|> try (PostfixP p <$> (p_op Ptr *> p_ident))
      <|>     (PostfixO p <$> (p_op PlusPlus <|> p_op MinusMinus))
    argument_expression_list = sepBy assignment_expression (p_op Comma)

unary_expression =
      try postfix_expression
  <|> try (UnaryO <$> p_op PlusPlus   <*> unary_expression)
  <|> try (UnaryO <$> p_op MinusMinus <*> unary_expression)
  <|> try (UnaryC <$> unary_operator  <*> cast_expression )
  <|> try (UnaryS <$> (p_kwd K_Sizeof  *> unary_expression))
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

cast_expression = try type_p <|> unary_p
  where
    unary_p = CastU <$> unary_expression
    type_p  = CastT <$> between_parenthes type_name <*> cast_expression

multiplicative_expression = chainl1 cast_expression
    $ binOp Asterisk
  <|> binOp Slash
  <|> binOp Percent

additive_expression = chainl1 multiplicative_expression
    $ binOp Plus
  <|> binOp Minus

shift_expression = chainl1 additive_expression
    $ binOp LeftShift
  <|> binOp RightShift

relation_expression = chainl1 shift_expression
    $ binOp LessThan
  <|> binOp GreaterThan
  <|> binOp LessThanEqual
  <|> binOp GreaterThanEqual

equal_expression = chainl1 relation_expression
    $ binOp EqualEqual
  <|> binOp NotEqual

and_expression = chainl1 equal_expression
    $ binOp And

exor_expression = chainl1 and_expression
    $ binOp Hat

or_expression = chainl1 exor_expression
    $ binOp Or

land_expression = chainl1 or_expression
    $ binOp AndAnd

lor_expression = chainl1 land_expression
    $ binOp OrOr

condition_expression = lor_expression >>= rest
  where
    rest p    = try (ternary p) <|> return p
    ternary p = TernaryOp p <$> (p_op Question *> expression <* p_op Colon) <*> condition_expression

assignment_expression = try assign_e <|> condition_expression
  where
    assign_e = do u  <- unary_expression
                  op <- assign_op
                  op u <$> assignment_expression
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

expression = chainl1 assignment_expression $ binOp Comma


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
  [ K_Typedef
  , K_Extern
  , K_Static
  , K_Auto
  , K_Register
  ]

type_spec =
      try (TypeSpecifier <$> k_type_specifier)
  <|> try struct_or_union_specifier
  <|> try enum_specifier
  <|>     TypeTypedef <$> p_typedef
  where
    k_type_specifier = oneOf_kwd
      [ K_Void
      , K_Char
      , K_Short
      , K_Int
      , K_Long
      , K_Float
      , K_Double
      , K_Signed
      , K_Unsigned
      ]

struct_or_union_specifier = oneOf_kwd [K_Struct, K_Union] >>= \k ->
      try (TypeStructUnion k <$> option [] p_ident <*> between_braces (many1 struct_declaration))
  <|>     (TypeStructUnion k <$> p_ident           <*> return [])

struct_declaration = StructDeclaration <$> specifier_qualifier_list
                       <*> sepBy1 struct_declarator (p_op Comma) <* p_op Semicolon

specifier_qualifier_list = many1 $ try type_qualifier <|> type_spec

struct_declarator =
      try (StructDeclarator <$> option DeclaratorN declarator <* p_op Colon <*> condition_expression)
  <|>     (StructDeclarator <$> declarator <*> return NullExpr)

enum_specifier = p_kwd K_Enum *> (try ident_and_elist <|> ident)
  where
    ident_and_elist = TypeEnum <$> option "" p_ident <*> enumerator_list_between_braces
    ident           = TypeEnum <$>           p_ident <*> return []
    enumerator_list_between_braces = between_braces $ sepEndBy1 enumerator (p_op Comma)

enumerator = Enumerator <$> p_ident <*> option_rhs
  where
    option_rhs = option NullExpr $ p_op Equal *> condition_expression

type_qualifier = fmap TypeQualifier $ oneOf_kwd [K_Const, K_Volatile]

declarator = Declarator <$> option PointerN pointer <*> direct_declarator

direct_declarator = (try ident <|> between_parenthes decl) >>= rest
  where
    ident    = DirectDeclaratorI <$> p_ident
    decl     = DirectDeclaratorD <$> declarator
    rest p   = try (rest_helper p >>= rest) <|> return p
    rest_helper p =
          try (between_brackets  $ DirectDeclaratorE p <$> option NullExpr condition_expression)
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

my_sepBy1 p sep = do x  <- p
                     xs <- rest
                     return (x:xs)
  where
    rest =     try (sep >> ((:) <$> p <*> rest))
           <|> return []

parameter_list = my_sepBy1 parameter_declaration $ p_op Comma

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
          try (between_brackets  $ DirectAbstractB p <$> option NullExpr  condition_expression)
      <|>     (between_parenthes $ DirectAbstractP p <$> option null_parm parameter_type_list)
    null_parm = ParameterTypeList False []

initializer =
      try (InitializerA <$> assignment_expression)
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
    case_stmt    = StatementCase    <$> (p_kwd K_Case *> condition_expression <* p_op Colon) <*> statement
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
              --Right val -> putStrLn $ analyze_translation_unit $ helper val
              Right val -> putStrLn $ show $ helper val
              Left  err -> putStrLn $ show err
  where
    helper val = evalState (runParserT min_parser () "" val) ["__builtin_va_list"]


analyze_translation_unit (Right exts) = show $ concat $ fmap analyze_external_declaration exts
analyze_translation_unit (Left  err)  = show err

analyze_external_declaration (ExternalDeclarationF fd) = analyze_function_definition fd
analyze_external_declaration (ExternalDeclarationD d)  = []

analyze_function_definition (FunctionDefinition _ declarator _ _) = analyze_declarator declarator

analyze_declarator (Declarator _ dd) = analyze_direct_declarator dd
analyze_declarator _ = []

analyze_direct_declarator dd = case dd of
  DirectDeclaratorI i   -> [i]
  DirectDeclaratorD d   -> analyze_declarator d
  DirectDeclaratorE d _ -> analyze_direct_declarator d
  DirectDeclaratorP d _ -> analyze_direct_declarator d
  DirectDeclaratorL d _ -> analyze_direct_declarator d


main = getContents >>= run

