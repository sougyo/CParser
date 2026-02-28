{- Author: Shogo Matsumoto -}
{- C99-faithful C parser (ISO/IEC 9899:1999) -}

import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Control.Monad (when)
import Control.Monad.State.Lazy
import Text.Parsec
import System.Exit
import System.IO

-- ===========================================================================
-- Operator Types
-- ===========================================================================

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

-- ===========================================================================
-- Keyword Types
-- ===========================================================================

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
  | K_Bool       -- C99: _Bool
  | K_Complex    -- C99: _Complex
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
  | K_Restrict   -- C99: restrict
  | K_Inline     -- C99: inline (function specifier)
  | K_Goto
  | K_Continue
  | K_Break
  | K_Return
  | K_Case
  | K_Default
  | K_Sizeof
  deriving (Show, Eq)

keyword_dictionary :: [(String, KeywordType)]
keyword_dictionary =
  [ ("if"          , K_If      )
  , ("else"        , K_Else    )
  , ("switch"      , K_Switch  )
  , ("while"       , K_While   )
  , ("do"          , K_Do      )
  , ("for"         , K_For     )
  , ("void"        , K_Void    )
  , ("char"        , K_Char    )
  , ("short"       , K_Short   )
  , ("int"         , K_Int     )
  , ("long"        , K_Long    )
  , ("float"       , K_Float   )
  , ("double"      , K_Double  )
  , ("signed"      , K_Signed  )
  , ("unsigned"    , K_Unsigned)
  , ("_Bool"       , K_Bool    )
  , ("_Complex"    , K_Complex )
  , ("struct"      , K_Struct  )
  , ("union"       , K_Union   )
  , ("enum"        , K_Enum    )
  , ("typedef"     , K_Typedef )
  , ("extern"      , K_Extern  )
  , ("static"      , K_Static  )
  , ("auto"        , K_Auto    )
  , ("register"    , K_Register)
  , ("const"       , K_Const   )
  , ("volatile"    , K_Volatile)
  , ("restrict"    , K_Restrict)
  , ("inline"      , K_Inline  )
  , ("goto"        , K_Goto    )
  , ("continue"    , K_Continue)
  , ("break"       , K_Break   )
  , ("return"      , K_Return  )
  , ("case"        , K_Case    )
  , ("default"     , K_Default )
  , ("sizeof"      , K_Sizeof  )
  -- GCC/Clang extensions: map to their standard equivalents
  , ("__const"     , K_Const   )
  , ("__const__"   , K_Const   )
  , ("__volatile__", K_Volatile)
  , ("__restrict"  , K_Restrict)
  , ("__restrict__", K_Restrict)
  , ("__signed__"  , K_Signed  )
  , ("__inline"    , K_Inline  )
  , ("__inline__"  , K_Inline  )
  ]

-- ===========================================================================
-- Token Types
-- ===========================================================================

data ConstantType =
    IntegerDecimalConstant      String [IntegerSuffix]
  | IntegerOctalConstant        String [IntegerSuffix]
  | IntegerHexadecimalConstant  String [IntegerSuffix]
  | FloatingDecimalConstant     String String String FloatingSuffix
  | FloatingHexadecimalConstant String String String FloatingSuffix
  | CharacterConstant           Bool String   -- Bool: True = wide (L prefix)
  deriving (Show, Eq)

data IntegerSuffix = UnsignedSuffix | LongSuffix | LongLongSuffix
  deriving (Show, Eq)

data FloatingSuffix =
    FSuffix
  | LSuffix
  | NullFloatingSuffix
  deriving (Show, Eq)

data Token =
    Constant      ConstantType
  | Identifier    String
  | StringLiteral Bool String   -- Bool: True = wide (L prefix)
  | Operator      OperatorType
  | Keyword       KeywordType
  | NullToken
  deriving (Show, Eq)

data PosToken = PosToken
  { get_pos   :: SourcePos
  , get_token :: Token
  } deriving (Show, Eq)

-- ===========================================================================
-- Character-level Lexer
-- ===========================================================================

token_parser :: Parsec String () [PosToken]
token_parser = fmap reject_null . many1 $
    PosToken <$> getPosition <*> makeToken <* spaces
  where
    reject_null = filter $ \t -> get_token t /= NullToken
    makeToken =
          try block_comment
      <|> try line_comment
      <|> try preprocessor_line
      <|> try ignore_attribute_or_asm
      <|> try ignore_extension
      <|> try (Constant <$> constant)
      <|> try keyword_or_identifier
      <|> try string_literal
      <|>     (Operator <$> operator)

-- /* ... */
block_comment :: Parsec String () Token
block_comment = string "/*" *> rest
  where rest = try (string "*/" *> return NullToken) <|> anyChar *> rest

-- // ...
line_comment :: Parsec String () Token
line_comment = string "//" *> many (noneOf "\n") *> return NullToken

-- # directive lines (preprocessor output that leaked through)
preprocessor_line :: Parsec String () Token
preprocessor_line = char '#' *> many (noneOf "\n") *> return NullToken

keyword_or_identifier :: Parsec String () Token
keyword_or_identifier = fmap choose identifier_str
  where
    choose s = case lookup s keyword_dictionary of
                 Just k  -> Keyword    k
                 Nothing -> Identifier s
    identifier_str = (:) <$> nondigit <*> many (nondigit <|> digit)
    nondigit = letter <|> char '_'

-- __attribute__((...)) and __asm__(...)
ignore_attribute_or_asm :: Parsec String () Token
ignore_attribute_or_asm =
    attribute_or_asm *> spaces *> between (char '(') (char ')') any_parens
    *> return NullToken
  where
    attribute_or_asm = try (string "__attribute__") <|> string "__asm__"
    any_parens       = many helper *> return ()
    helper =
          try (between (char '(') (char ')') any_parens)
      <|> try (between (char '[') (char ']') any_brackets)
      <|> try (many1 (noneOf "()[]") *> return ())
    any_brackets = many (try (many1 (noneOf "[]")) *> return ()) *> return ()

-- __extension__ is a GCC marker to suppress extension warnings; discard it
ignore_extension :: Parsec String () Token
ignore_extension = string "__extension__" *> return NullToken

-- ===========================================================================
-- Constant Lexers
-- ===========================================================================

constant :: Parsec String () ConstantType
constant =
      try floating_constant
  <|> try integer_constant
  <|>     character_constant

integer_constant :: Parsec String () ConstantType
integer_constant = constant_part <*> option [] integer_suffix
  where
    constant_part =
          try (IntegerHexadecimalConstant <$> hexadecimal_constant)
      <|> try (IntegerOctalConstant       <$> octal_constant      )
      <|>     (IntegerDecimalConstant     <$> decimal_constant    )

-- Decimal: nonzero-digit digit*
decimal_constant :: Parsec String () String
decimal_constant = (:) <$> nonzero_digit <*> many digit

-- Octal: 0 octal-digit* (preserves the leading '0')
octal_constant :: Parsec String () String
octal_constant = (:) <$> char '0' <*> many octDigit

-- Hexadecimal: 0x hex-digit+ (preserves the "0x" prefix)
hexadecimal_constant :: Parsec String () String
hexadecimal_constant = (++) <$> hexadecimal_prefix <*> many1 hexDigit

hexadecimal_prefix :: Parsec String () String
hexadecimal_prefix = try (string "0x") <|> string "0X"

-- C99 §6.4.4.1: integer-suffix
-- Handles: u/U, l/L, ll/LL/lL/Ll and their combinations
integer_suffix :: Parsec String () [IntegerSuffix]
integer_suffix =
      try ((++) <$> unsigned_p  <*> long_long_p            )
  <|> try ((++) <$> unsigned_p  <*> option [] long_p       )
  <|> try ((++) <$> long_long_p <*> option [] unsigned_p   )
  <|>     ((++) <$> long_p      <*> option [] unsigned_p   )
  where
    unsigned_suffix  = char 'u' <|> char 'U'
    long_suffix      = char 'l' <|> char 'L'
    -- C99 permits ll, LL, lL, Ll
    long_long_suffix = try (string "ll") <|> try (string "LL")
                   <|> try (string "lL") <|>      string "Ll"
    unsigned_p  = unsigned_suffix  *> return [UnsignedSuffix]
    long_p      = long_suffix      *> return [LongSuffix]
    long_long_p = long_long_suffix *> return [LongLongSuffix]

nonzero_digit :: Parsec String () Char
nonzero_digit = oneOf "123456789"

floating_constant :: Parsec String () ConstantType
floating_constant =
      try hexadecimal_floating_constant
  <|>     decimal_floating_constant

construct_floating_constant_p :: (String -> String -> String -> FloatingSuffix -> ConstantType)
                               -> Parsec String () Char
                               -> String
                               -> Parsec String () ConstantType
construct_floating_constant_p f_type digit_p exp_prefix =
       try (f_type <$> (many  digit_p <* char '.') <*> many1 digit_p <*> option "" exponent_part <*> floating_suffix)
  <|>  try (f_type <$> (many1 digit_p <* char '.') <*> return ""     <*> option "" exponent_part <*> floating_suffix)
  <|>      (f_type <$>  many1 digit_p              <*> return ""     <*>           exponent_part <*> floating_suffix)
  where
    exponent_part = (:) <$> oneOf exp_prefix
                        <*> ((++) <$> option "" (string "+" <|> string "-") <*> many1 digit)

decimal_floating_constant :: Parsec String () ConstantType
decimal_floating_constant =
    construct_floating_constant_p FloatingDecimalConstant digit "eE"

hexadecimal_floating_constant :: Parsec String () ConstantType
hexadecimal_floating_constant = hexadecimal_prefix *>
    construct_floating_constant_p FloatingHexadecimalConstant hexDigit "pP"

floating_suffix :: Parsec String () FloatingSuffix
floating_suffix = option NullFloatingSuffix $
      try (oneOf "lL" *> return LSuffix)
  <|>     (oneOf "fF" *> return FSuffix)

-- C99 §6.4.4.4: character-constant (preserves L-prefix flag)
character_constant :: Parsec String () ConstantType
character_constant = do
    wide <- option False (string "L" *> return True)
    cs   <- between (char '\'') (char '\'') (many1 c_char)
    return $ CharacterConstant wide cs
  where
    c_char = try (noneOf "'\\\n") <|> escape_sequence

-- C99 §6.4.5: string-literal (preserves L-prefix flag)
string_literal :: Parsec String () Token
string_literal = do
    wide <- option False (string "L" *> return True)
    s    <- between (char '"') (char '"') (many s_char)
    return $ StringLiteral wide s
  where
    s_char = try (noneOf "\"\\\n") <|> escape_sequence

escape_sequence :: Parsec String () Char
escape_sequence =
      try simple_escape_sequence
  <|> try octal_escape_sequence
  <|> try hexadecimal_escape_sequence
  <|>     universal_character_name

simple_escape_sequence :: Parsec String () Char
simple_escape_sequence =
      try (string "\\\\" *> return '\\')
  <|> try (string "\\'"  *> return '\'')
  <|> try (string "\\\"" *> return '"' )
  <|> try (string "\\a"  *> return '\a')
  <|> try (string "\\b"  *> return '\b')
  <|> try (string "\\f"  *> return '\f')
  <|> try (string "\\n"  *> return '\n')
  <|> try (string "\\r"  *> return '\r')
  <|> try (string "\\t"  *> return '\t')
  <|>      string "\\v"  *> return '\v'

octal_escape_sequence :: Parsec String () Char
octal_escape_sequence = char '\\' *>
  (     try (octDigit *> octDigit *> octDigit *> return '?')
    <|> try (octDigit *> octDigit             *> return '?')
    <|>     (octDigit                         *> return '?'))

hexadecimal_escape_sequence :: Parsec String () Char
hexadecimal_escape_sequence = string "\\x" *> many1 hexDigit *> return '?'

-- C99 §6.4.3: \uXXXX = 4 hex digits, \UXXXXXXXX = 8 hex digits
universal_character_name :: Parsec String () Char
universal_character_name =
      try (string "\\u" *> count 4 hexDigit *> return '?')
  <|>     (string "\\U" *> count 8 hexDigit *> return '?')

operator :: Parsec String () OperatorType
operator =
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

-- ===========================================================================
-- Token-level Parser Primitives
-- ===========================================================================

-- Typedef scope stack: head = innermost scope.
-- Typedef names are pushed onto the innermost scope and removed when
-- the scope (compound statement) is exited.
type TypedefState = [[String]]

type CParser a = ParsecT [PosToken] () (Control.Monad.State.Lazy.State TypedefState) a

gen_p :: (Token -> Maybe a) -> CParser a
gen_p f = tokenPrim (\c -> show c) (\_ c _ -> get_pos c) $ f . get_token

p_const :: CParser ConstantType
p_const = gen_p $ \n -> case n of
    Constant s -> Just s
    _          -> Nothing

p_ident :: CParser String
p_ident = gen_p $ \n -> case n of
    Identifier s -> Just s
    _            -> Nothing

-- Matches only identifiers that have been declared as typedef names.
p_typedef :: CParser String
p_typedef = do
    scopes <- lift get
    let all_typedefs = concat scopes
    gen_p $ \n -> case n of
      Identifier s -> if s `elem` all_typedefs then Just s else Nothing
      _            -> Nothing

p_string :: CParser String
p_string = gen_p $ \n -> case n of
    StringLiteral _ s -> Just s
    _                 -> Nothing

p_op :: OperatorType -> CParser OperatorType
p_op p = gen_p $ \n -> if n == Operator p then Just p else Nothing

p_kwd :: KeywordType -> CParser KeywordType
p_kwd p = gen_p $ \n -> if n == Keyword p then Just p else Nothing

oneOf_kwd :: [KeywordType] -> CParser KeywordType
oneOf_kwd kwds = gen_p $ \n -> case n of
    Keyword k -> if k `elem` kwds then Just k else Nothing
    _         -> Nothing

-- ===========================================================================
-- AST Types
-- ===========================================================================

data Expr =
    EIdent    String
  | EConst    ConstantType
  | EString   String
  | TernaryOp Expr Expr Expr
  | BinOp     OperatorType Expr Expr
  | PostfixE  Expr Expr             -- a[i]
  | PostfixA  Expr [Expr]           -- f(args)
  | PostfixD  Expr String           -- a.member
  | PostfixP  Expr String           -- a->member
  | PostfixO  Expr OperatorType     -- a++ / a--
  | UnaryC    OperatorType Expr     -- unary-operator cast-expr
  | UnaryO    OperatorType Expr     -- ++/-- unary-expr
  | UnaryS    Expr                  -- sizeof unary-expr
  | UnaryT    TypeName              -- sizeof(type-name)
  | CastU     Expr                  -- unary-expr used as cast-expr (no-op wrapper)
  | CastT     TypeName Expr         -- (type-name) cast-expr
  | CompoundLiteral TypeName Initializer  -- C99: (type-name) { init-list }
  | NullExpr
  deriving (Show, Eq)

-- C99 §6.8.5.3: for-loop init can be a declaration or expression
data ForInit =
    ForInitDecl Declaration
  | ForInitExpr Expr
  | ForInitNull
  deriving (Show, Eq)

-- C99 §6.7.8: designated initializer components
data Designator =
    DesignatorIndex Expr    -- [ constant-expression ]
  | DesignatorField String  -- . identifier
  deriving (Show, Eq)

data Declaration = Declaration [DeclarationSpecifier] [InitDeclarator]
  deriving (Show, Eq)

data InitDeclarator =
    InitDeclaratorD Declarator
  | InitDeclaratorI Declarator Initializer
  deriving (Show, Eq)

data StructDeclaration = StructDeclaration [DeclarationSpecifier] [StructDeclarator]
  deriving (Show, Eq)

-- C99 §6.7.2.1: struct-declarator
data StructDeclarator =
    StructDeclaratorF Declarator               -- plain field
  | StructDeclaratorB (Maybe Declarator) Expr  -- bitfield (declarator is optional for anonymous)
  deriving (Show, Eq)

data Enumerator = Enumerator String Expr
  deriving (Show, Eq)

-- C99 §6.7: declaration-specifiers
data DeclarationSpecifier =
    TypeSpecifier    KeywordType
  | TypeQualifier    KeywordType
  | TypeStorage      KeywordType
  | FunctionSpecifier KeywordType               -- C99: inline
  | TypeStructUnion  KeywordType String [StructDeclaration]
  | TypeEnum         String [Enumerator]
  | TypeTypedef      String
  deriving (Show, Eq)

data Declarator =
    Declarator  Pointer DirectDeclarator
  | DeclaratorN
  deriving (Show, Eq)

-- C99 §6.7.5: direct-declarator
data DirectDeclarator =
    DirectDeclaratorI String
  | DirectDeclaratorD Declarator
  | DirectDeclaratorE DirectDeclarator Expr        -- [ assignment-expr? ]
  | DirectDeclaratorV DirectDeclarator             -- C99: [ * ] VLA in prototype
  | DirectDeclaratorP DirectDeclarator ParameterTypeList
  | DirectDeclaratorL DirectDeclarator [String]    -- K&R identifier list (obsolescent)
  deriving (Show, Eq)

-- Each element of the list is one '*' level with its qualifier list
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
  | InitializerD [Designator] Initializer   -- C99: designated initializer
  | InitializerI [Initializer]              -- { initializer-list }
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
  | StatementFor      ForInit Expr Expr Statement  -- C99: init can be declaration
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

data FunctionDefinition =
    FunctionDefinition [DeclarationSpecifier] Declarator [Declaration] Statement
  deriving (Show, Eq)

-- ===========================================================================
-- Parser Combinators
-- ===========================================================================

binOp :: OperatorType -> CParser (Expr -> Expr -> Expr)
binOp p = BinOp <$> p_op p

between_brackets :: CParser a -> CParser a
between_brackets  = between (p_op LeftBracket)   (p_op RightBracket)

between_braces :: CParser a -> CParser a
between_braces    = between (p_op LeftBrace)     (p_op RightBrace)

between_parenthes :: CParser a -> CParser a
between_parenthes = between (p_op LeftParenthes) (p_op RightParenthes)

expr_between_parenthes :: CParser Expr
expr_between_parenthes = between_parenthes expression

-- ===========================================================================
-- Expression Parsers  (C99 §6.5)
-- ===========================================================================

primary_expression :: CParser Expr
primary_expression =
      try (EConst  <$> p_const)
  <|> try (EIdent  <$> p_ident)
  <|> try (EString <$> fmap concat (many1 p_string))
  <|>      expr_between_parenthes

-- C99 §6.5.2: postfix-expression
-- Compound literals (type-name){init-list} are postfix expressions.
postfix_expression :: CParser Expr
postfix_expression = (try compound_literal <|> primary_expression) >>= rest
  where
    -- C99 §6.5.2.5: compound literal
    compound_literal =
        CompoundLiteral <$> try (between_parenthes type_name)
                        <*> brace_initializer
    rest p = try (helper p >>= rest) <|> return p
    helper p =
          try (PostfixE p <$> between_brackets expression)
      <|> try (PostfixA p <$> between_parenthes (sepBy assignment_expression (p_op Comma)))
      <|> try (PostfixD p <$> (p_op Dot *> p_ident))
      <|> try (PostfixP p <$> (p_op Ptr *> p_ident))
      <|>     (PostfixO p <$> (p_op PlusPlus <|> p_op MinusMinus))

-- C99 §6.5.3: unary-expression
-- sizeof(type-name) is tried before sizeof unary-expression to correctly
-- handle the case where the argument is a typedef name.
unary_expression :: CParser Expr
unary_expression =
      try postfix_expression
  <|> try (UnaryO <$> p_op PlusPlus   <*> unary_expression)
  <|> try (UnaryO <$> p_op MinusMinus <*> unary_expression)
  <|> try (UnaryC <$> unary_operator  <*> cast_expression)
  <|> try (UnaryT <$> (p_kwd K_Sizeof *> between_parenthes type_name))
  <|>     (UnaryS <$> (p_kwd K_Sizeof *> unary_expression))
  where
    unary_operator =
          p_op And
      <|> p_op Asterisk
      <|> p_op Plus
      <|> p_op Minus
      <|> p_op Tilde
      <|> p_op Exclamation

-- C99 §6.5.4: cast-expression
-- try wraps the entire type_p so that if cast_expression fails after
-- consuming (type-name), we backtrack and try unary_p.
-- This correctly handles (T){...} as a compound literal via unary_p.
cast_expression :: CParser Expr
cast_expression = try type_p <|> unary_p
  where
    unary_p = CastU <$> unary_expression
    type_p  = CastT <$> between_parenthes type_name <*> cast_expression

multiplicative_expression :: CParser Expr
multiplicative_expression = chainl1 cast_expression $
      binOp Asterisk
  <|> binOp Slash
  <|> binOp Percent

additive_expression :: CParser Expr
additive_expression = chainl1 multiplicative_expression $
      binOp Plus
  <|> binOp Minus

shift_expression :: CParser Expr
shift_expression = chainl1 additive_expression $
      binOp LeftShift
  <|> binOp RightShift

relational_expression :: CParser Expr
relational_expression = chainl1 shift_expression $
      binOp LessThan
  <|> binOp GreaterThan
  <|> binOp LessThanEqual
  <|> binOp GreaterThanEqual

equality_expression :: CParser Expr
equality_expression = chainl1 relational_expression $
      binOp EqualEqual
  <|> binOp NotEqual

and_expression :: CParser Expr
and_expression = chainl1 equality_expression $ binOp And

xor_expression :: CParser Expr
xor_expression = chainl1 and_expression $ binOp Hat

or_expression :: CParser Expr
or_expression = chainl1 xor_expression $ binOp Or

logical_and_expression :: CParser Expr
logical_and_expression = chainl1 or_expression $ binOp AndAnd

logical_or_expression :: CParser Expr
logical_or_expression = chainl1 logical_and_expression $ binOp OrOr

-- C99 §6.5.15: conditional-expression
conditional_expression :: CParser Expr
conditional_expression = logical_or_expression >>= rest
  where
    rest p    = try (ternary p) <|> return p
    ternary p = TernaryOp p
      <$> (p_op Question *> expression <* p_op Colon)
      <*> conditional_expression

-- C99 §6.5.16: assignment-expression
assignment_expression :: CParser Expr
assignment_expression = try assign_e <|> conditional_expression
  where
    assign_e = do
      u  <- unary_expression
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

-- C99 §6.5.17: expression (comma-expression)
expression :: CParser Expr
expression = chainl1 assignment_expression $ binOp Comma

-- ===========================================================================
-- Declaration Parsers  (C99 §6.7)
-- ===========================================================================

declaration :: CParser Declaration
declaration = do
    d <- Declaration <$> declaration_specs <*> init_declarator_list <* p_op Semicolon
    search_and_put_typedef_type d
    return d
  where
    init_declarator_list = sepBy init_declarator (p_op Comma)

-- Register typedef names into the innermost scope of the scope stack.
search_and_put_typedef_type :: Declaration -> CParser ()
search_and_put_typedef_type (Declaration specs idecls) =
    when (TypeStorage K_Typedef `elem` specs) $ do
      let new_typedefs = concatMap search_idecl idecls
      lift $ modify $ \scopes -> case scopes of
        (scope : rest) -> (new_typedefs ++ scope) : rest
        []             -> [new_typedefs]
  where
    search_idecl (InitDeclaratorD d  ) = search_decl d
    search_idecl (InitDeclaratorI d _) = search_decl d
    search_decl  (Declarator _ d)      = search_ddecl d
    search_decl  _                     = []
    search_ddecl ddecl = case ddecl of
      DirectDeclaratorI i   -> [i]
      DirectDeclaratorD d   -> search_decl  d
      DirectDeclaratorE d _ -> search_ddecl d
      DirectDeclaratorV d   -> search_ddecl d
      DirectDeclaratorP d _ -> search_ddecl d
      DirectDeclaratorL d _ -> search_ddecl d

-- C99 §6.7: declaration-specifiers
declaration_specs :: CParser [DeclarationSpecifier]
declaration_specs = many1 $
      try type_qualifier
  <|> try storage_class_spec
  <|> try function_specifier
  <|>     type_spec

init_declarator :: CParser InitDeclarator
init_declarator = declarator >>= \d ->
      try (InitDeclaratorI d <$> (p_op Equal *> initializer))
  <|>      return (InitDeclaratorD d)

-- C99 §6.7.1: storage-class-specifier
storage_class_spec :: CParser DeclarationSpecifier
storage_class_spec = TypeStorage <$> oneOf_kwd
  [K_Typedef, K_Extern, K_Static, K_Auto, K_Register]

-- C99 §6.7.4: function-specifier (inline)
function_specifier :: CParser DeclarationSpecifier
function_specifier = FunctionSpecifier <$> oneOf_kwd [K_Inline]

-- C99 §6.7.2: type-specifier
type_spec :: CParser DeclarationSpecifier
type_spec =
      try (TypeSpecifier <$> k_type_specifier)
  <|> try struct_or_union_specifier
  <|> try enum_specifier
  <|>     (TypeTypedef <$> p_typedef)
  where
    k_type_specifier = oneOf_kwd
      [ K_Void, K_Char, K_Short, K_Int, K_Long
      , K_Float, K_Double, K_Signed, K_Unsigned
      , K_Bool, K_Complex
      ]

-- C99 §6.7.2.1: struct-or-union-specifier
struct_or_union_specifier :: CParser DeclarationSpecifier
struct_or_union_specifier = oneOf_kwd [K_Struct, K_Union] >>= \k ->
      try (TypeStructUnion k <$> option "" p_ident <*> between_braces (many struct_declaration))
  <|>     (TypeStructUnion k <$> p_ident           <*> return [])

-- C99 §6.7.2.1: struct-declaration
-- Using sepBy (not sepBy1) allows anonymous struct/union members (C11/GCC ext.)
struct_declaration :: CParser StructDeclaration
struct_declaration = StructDeclaration
    <$> specifier_qualifier_list
    <*> sepBy struct_declarator (p_op Comma)
    <*  p_op Semicolon

specifier_qualifier_list :: CParser [DeclarationSpecifier]
specifier_qualifier_list = many1 $ try type_qualifier <|> type_spec

-- C99 §6.7.2.1: struct-declarator
-- Handles plain fields, bitfields, and anonymous bitfields (int :N;)
struct_declarator :: CParser StructDeclarator
struct_declarator =
      try (StructDeclaratorB <$> optionMaybe declarator <* p_op Colon <*> conditional_expression)
  <|>     (StructDeclaratorF <$> declarator)

-- C99 §6.7.2.2: enum-specifier
enum_specifier :: CParser DeclarationSpecifier
enum_specifier = p_kwd K_Enum *> (try ident_and_elist <|> ident)
  where
    ident_and_elist = TypeEnum <$> option "" p_ident <*> enumerator_list_between_braces
    ident           = TypeEnum <$>           p_ident <*> return []
    enumerator_list_between_braces = between_braces $ sepEndBy1 enumerator (p_op Comma)

enumerator :: CParser Enumerator
enumerator = Enumerator <$> p_ident <*> option_rhs
  where
    option_rhs = option NullExpr $ p_op Equal *> conditional_expression

-- C99 §6.7.3: type-qualifier (const, volatile, restrict)
type_qualifier :: CParser DeclarationSpecifier
type_qualifier = TypeQualifier <$> oneOf_kwd [K_Const, K_Volatile, K_Restrict]

-- C99 §6.7.5: declarator
declarator :: CParser Declarator
declarator = Declarator <$> option PointerN pointer <*> direct_declarator

-- C99 §6.7.5: direct-declarator
-- Parameter-type-list is tried before identifier-list to correctly resolve
-- the prototype vs. K&R ambiguity when typedef names are present.
direct_declarator :: CParser DirectDeclarator
direct_declarator =
    (try (DirectDeclaratorI <$> p_ident)
     <|> (DirectDeclaratorD <$> between_parenthes declarator))
    >>= rest
  where
    rest p = try (rest_helper p >>= rest) <|> return p
    rest_helper p =
          -- C99: [*] VLA parameter array
          try (DirectDeclaratorV p <$  between_brackets (many type_qualifier *> p_op Asterisk))
          -- Array with optional type-qualifiers and size
      <|> try (between_brackets
                 (DirectDeclaratorE p <$> (many type_qualifier *> option NullExpr assignment_expression)))
          -- Prototype-style: (parameter-type-list) — tried before K&R list
      <|> try (between_parenthes $ DirectDeclaratorP p <$> parameter_type_list)
          -- K&R-style: (identifier-list?)
      <|>     (between_parenthes $ DirectDeclaratorL p <$> sepBy p_ident (p_op Comma))

-- C99 §6.7.5: pointer  (each '*' gets its own qualifier list)
pointer :: CParser Pointer
pointer = Pointer <$> many1 (p_op Asterisk *> many type_qualifier)

-- C99 §6.7.5: parameter-type-list
parameter_type_list :: CParser ParameterTypeList
parameter_type_list = parameter_list >>= \p ->
      try (ellipsis *> return (ParameterTypeList True  p))
  <|>                  return (ParameterTypeList False p)
  where
    ellipsis = p_op Comma *> p_op ThreeDots

-- sepBy1 variant that does not consume the separator on the last element
my_sepBy1 :: CParser a -> CParser sep -> CParser [a]
my_sepBy1 p sep = (:) <$> p <*> rest
  where rest = try (sep >> ((:) <$> p <*> rest)) <|> return []

parameter_list :: CParser [ParameterDeclaration]
parameter_list = my_sepBy1 parameter_declaration (p_op Comma)

parameter_declaration :: CParser ParameterDeclaration
parameter_declaration = declaration_specs >>= \d ->
      try    (ParameterDeclarationD d <$> declarator)
  <|> try    (ParameterDeclarationA d <$> abstract_declarator)
  <|> return (ParameterDeclarationN d)

-- C99 §6.7.6: type-name
type_name :: CParser TypeName
type_name = TypeName
    <$> specifier_qualifier_list
    <*> option AbstractDeclaratorN abstract_declarator

-- C99 §6.7.6: abstract-declarator
-- Fixed: use PointerN (not Pointer []) for the no-pointer case
abstract_declarator :: CParser AbstractDeclarator
abstract_declarator =
      try (AbstractDeclarator <$> pointer <*> direct_abstract_declarator)
  <|> try (AbstractDeclarator <$> pointer <*> return DirectAbstractN)
  <|>     (AbstractDeclarator PointerN    <$> direct_abstract_declarator)

-- C99 §6.7.6: direct-abstract-declarator
direct_abstract_declarator :: CParser DirectAbstractDeclarator
direct_abstract_declarator = head_decl >>= rest
  where
    rest p    = try (post p >>= rest) <|> return p
    head_decl =
          try (DirectAbstractA <$> between_parenthes abstract_declarator)
      <|> post DirectAbstractN
    post p    =
          try (between_brackets  $ DirectAbstractB p <$> option NullExpr conditional_expression)
      <|>     (between_parenthes $ DirectAbstractP p <$> option null_parm parameter_type_list)
    null_parm = ParameterTypeList False []

-- C99 §6.7.8: Braced initializer  { init-list [,] }
brace_initializer :: CParser Initializer
brace_initializer = between_braces $
    InitializerI <$> sepEndBy1 init_list_item (p_op Comma)

-- C99 §6.7.8: item in an initializer list: [designation =] initializer
init_list_item :: CParser Initializer
init_list_item =
      try (InitializerD <$> many1 designator <* p_op Equal <*> initializer)
  <|> initializer

-- C99 §6.7.8: designator
designator :: CParser Designator
designator =
      try (DesignatorIndex <$> between_brackets conditional_expression)
  <|>     (DesignatorField <$> (p_op Dot *> p_ident))

-- C99 §6.7.8: initializer
initializer :: CParser Initializer
initializer =
      try (InitializerA <$> assignment_expression)
  <|> brace_initializer

-- ===========================================================================
-- Statement Parsers  (C99 §6.8)
-- ===========================================================================

statement :: CParser Statement
statement =
      try labeled_statement
  <|> try compound_statement
  <|> try expression_statement
  <|> try selection_statement
  <|> try iteration_statement
  <|>     jump_statement

-- C99 §6.8.1: labeled-statement
labeled_statement :: CParser Statement
labeled_statement =
      try case_stmt
  <|> try default_stmt
  <|>     id_stmt
  where
    case_stmt    = StatementCase   <$> (p_kwd K_Case *> conditional_expression <* p_op Colon) <*> statement
    default_stmt = StatementDefault <$> (p_kwd K_Default *> p_op Colon *> statement)
    id_stmt      = StatementId      <$> (p_ident <* p_op Colon) <*> statement

-- C99 §6.8.2: compound-statement
-- Manages typedef scope: push a new scope on '{', pop on '}'.
compound_statement :: CParser Statement
compound_statement = do
    _ <- p_op LeftBrace
    lift $ modify ([] :)        -- push new scope
    items <- many block_item
    _ <- p_op RightBrace
    lift $ modify (drop 1)      -- pop scope (drop 1 is safe on empty list)
    return $ StatementB items

block_item :: CParser BlockItem
block_item =
      try (BlockItemD <$> declaration)
  <|>     (BlockItemS <$> statement)

-- C99 §6.8.3: expression-statement
expression_statement :: CParser Statement
expression_statement = StatementExpr <$> (option NullExpr expression <* p_op Semicolon)

-- C99 §6.8.4: selection-statement
selection_statement :: CParser Statement
selection_statement = try if_stmt <|> switch_stmt
  where
    if_stmt     = p_kwd K_If     *> (StatementIf    <$> expr_between_parenthes <*> statement <*> else_p)
    switch_stmt = p_kwd K_Switch *> (StatementSwitch <$> expr_between_parenthes <*> statement)
    else_p      = option StatementN (p_kwd K_Else *> statement)

-- C99 §6.8.5: iteration-statement
iteration_statement :: CParser Statement
iteration_statement =
      try while_stmt
  <|> try do_stmt
  <|>     for_stmt
  where
    while_stmt = p_kwd K_While *>
        (StatementWhile <$> expr_between_parenthes <*> statement)
    do_stmt    = p_kwd K_Do   *>
        (StatementDo <$> statement) <* p_kwd K_While
        <*> expr_between_parenthes  <* p_op Semicolon
    -- C99 §6.8.5.3: for-statement
    -- First clause may be a declaration (C99) or expression or empty.
    for_stmt = do
        _ <- p_kwd K_For
        (ini, cond, upd) <- between_parenthes for_header
        body <- statement
        return $ StatementFor ini cond upd body
    for_header = do
        ini  <- for_init
        cond <- option NullExpr expression <* p_op Semicolon
        upd  <- option NullExpr expression
        return (ini, cond, upd)
    for_init =
          try (ForInitDecl <$> declaration)          -- declaration (includes ';')
      <|> try (ForInitExpr <$> expression <* p_op Semicolon)
      <|> (ForInitNull <$ p_op Semicolon)

-- C99 §6.8.6: jump-statement
jump_statement :: CParser Statement
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

-- ===========================================================================
-- External Definitions  (C99 §6.9)
-- ===========================================================================

translation_unit :: CParser [ExternalDeclaration]
translation_unit = many1 external_declaration

external_declaration :: CParser ExternalDeclaration
external_declaration =
      try (ExternalDeclarationF <$> function_definition)
  <|>     (ExternalDeclarationD <$> declaration)

-- C99 §6.9.1: function-definition
-- Also supports K&R-style parameter declarations.
function_definition :: CParser FunctionDefinition
function_definition = FunctionDefinition
    <$> declaration_specs
    <*> declarator
    <*> many declaration
    <*> compound_statement

-- ===========================================================================
-- Main
-- ===========================================================================

min_parser :: CParser [ExternalDeclaration]
min_parser = translation_unit <* eof

run :: String -> IO ()
run input = case parse (spaces *> token_parser <* eof) "" input of
    Right val -> helper val
    Left  err -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)
  where
    -- Initial typedef scope: one global scope with common GCC built-ins
    init_state :: TypedefState
    init_state = [["__builtin_va_list"]]
    helper val = case evalState (runPT min_parser () "" val) init_state of
        Right val2 -> putStrLn $ show val2
        Left  err  -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 1)

main :: IO ()
main = getContents >>= run
