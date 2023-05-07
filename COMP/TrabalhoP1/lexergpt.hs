import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token as Token

-- Definição dos tokens da linguagem
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser haskellDef
  { Token.reservedNames = ["int", "double", "string", "void", "return", "if", "else", "while", "print", "read"]
  , Token.reservedOpNames = ["+", "-", "*", "/", "<", ">", "<=", ">=", "==", "/=", "&&", "||", "!"]
  , Token.identStart = letter <|> char '_'
  , Token.identLetter = alphaNum <|> char '_'
  , Token.caseSensitive = True
  }

-- Tipos de token reconhecidos
data Token
  = TId String
  | TNumInt Integer
  | TNumDouble Double
  | TString String
  | TTipo String
  | TVoid
  | TReturn
  | TIf
  | TElse
  | TWhile
  | TPrint
  | TRead
  | TOpenPar
  | TClosePar
  | TOpenBrace
  | TCloseBrace
  | TComma
  | TSemicolon
  | TAssign
  | TAdd
  | TSub
  | TMul
  | TDiv
  | TLt
  | TGt
  | TLeq
  | TGeq
  | TEq
  | TNeq
  | TAnd
  | TOr
  | TNot

-- Parser para identificadores
pId :: Parser String
pId = Token.identifier lexer

-- Parser para números inteiros
pNumInt :: Parser Integer
pNumInt = Token.integer lexer

-- Parser para números de ponto flutuante
pNumDouble :: Parser Double
pNumDouble = Token.float lexer

-- Parser para strings delimitadas por aspas duplas
pString :: Parser String
pString = Token.stringLiteral lexer

-- Parser para o tipo void
pVoid :: Parser ()
pVoid = Token.reserved lexer "void" >> return ()

-- Parser para a palavra-chave return
pReturn :: Parser ()
pReturn = Token.reserved lexer "return" >> return ()

-- Parser para a palavra-chave if
pIf :: Parser ()
pIf = Token.reserved lexer "if" >> return ()

-- Parser para a palavra-chave else
pElse :: Parser ()
pElse = Token.reserved lexer "else" >> return ()

-- Parser para a palavra-chave while
pWhile :: Parser ()
pWhile = Token.reserved lexer "while" >> return ()

-- Parser para a palavra-chave print
pPrint :: Parser ()
pPrint = Token.reserved lexer "print" >> return ()

-- Parser para a palavra-chave read
pRead :: Parser ()
pRead = Token.reserved lexer "read" >> return ()

-- Função que ignora espaços em branco, tabs e quebras de linha
espaços :: Parser ()
espaços = skipMany (oneOf " \t\n")
