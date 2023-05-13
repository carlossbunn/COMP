import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import qualified Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Data.Maybe


type Id = String
data Tipo = TDouble | TInt | TString | TVoid deriving Show
data TCons = CDouble Double | CInt Integer deriving Show
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] | Lit String deriving Show
data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr |Expr :<=: Expr | Expr :>=: Expr deriving Show
data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show
data Var = Id :#: Tipo deriving Show
data Funcao = Id :->: ([Var], Tipo) deriving Show
data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show
type Bloco = [Comando]
data Comando = If ExprL Bloco Bloco
                | While ExprL Bloco
                | Atrib Id Expr
                | Leitura Id
                | Imp Expr
                | Ret (Maybe Expr)
                | Proc Id [Expr]
                | Print Expr
                | ReadAction String
                    deriving Show

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["{","}","+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&", "|", "!"]
            , T.reservedNames   = ["while", "return", "int", "string", "double"]
          }

lexico = T.makeTokenParser lingDef
symbol = T.symbol lexico
reserved      = T.reserved lexico
reservedOp    = T.reservedOp lexico
parens        = T.parens lexico
natural       = T.natural lexico
identifier   = T.identifier lexico

tabela = [[prefix "-" Neg]
          , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
          , [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
         ]
tabelaL   = [[prefix "!" Not]
            , [binario "&" (:&:) AssocLeft]
            , [binario "|" (:|:) AssocLeft]
            ]
opR = do {reservedOp "=="; return (:==:)}
    <|> do {reservedOp "/="; return (:/=:)}
    <|> do {reservedOp "<"; return (:<:)}
    <|> do {reservedOp ">"; return (:>:)}
    <|> do {reservedOp ">="; return (:>=:)}
    <|> do {reservedOp "<="; return (:<=:)}

prefix   name fun = Prefix (do {reservedOp name; return fun })
binario name fun = Infix (do {reservedOp name; return fun })

fator = parens expr
       <|> do {Const . CInt <$> natural;}
       <?> "simple expression"

expr = buildExpressionParser tabela fator <?> "expression"

exprL = buildExpressionParser tabelaL exprR
exprR = do {e1 <- expr; o <- opR; Rel . o e1 <$> expr;}

programa :: Parser Programa
programa = do
  funcoes <- listaFuncoes
  Prog funcoes [] [] <$> blocoPrincipal

listaFuncoes :: Parser [Funcao]
listaFuncoes = sepEndBy funcao (symbol ";")

funcao :: Parser Funcao
funcao = do
  tipo <- tipoRetorno
  id <- identifier
  parametros <- parens declParametros
  let idsParametros = map (\(t, v) -> v :#: t) parametros
  blocoPrincipal <- bloco
  return $ id :->: (idsParametros, tipo)

tipoRetorno :: Parser Tipo
tipoRetorno = tipo <|> (reserved "void" >> return TVoid)

declParametros :: Parsec String u [(Tipo, String)]
declParametros = Text.Parsec.try (do
  tipoParam <- tipo
  id <- identifier
  [outrosParams] <- parametros
  return $ (tipoParam, id) : outrosParams)
  <|> return []

parametros :: ParsecT String u Data.Functor.Identity.Identity [[(Tipo, String)]]
parametros = option [] $ symbol "," >> sepBy1 declParametros (symbol ",")

blocoPrincipal :: Parser Bloco
blocoPrincipal = do
  symbol "{"
  cmds <- blocoPrincipal'
  symbol "}"
  return cmds

blocoPrincipal' :: Parser Bloco
blocoPrincipal' = do
  decls <- declaracoes <|> return []
  cmds <- listaCmd
  return (concat decls ++ cmds)

declaracoes = sepEndBy declaracao (symbol ";")

declaracao = do
  tipo <- tipo
  ids <- listaId
  symbol ";"
  return $ map (\id -> Atrib id (defaultValue tipo)) ids

defaultValue :: Tipo -> Expr
defaultValue TInt = Const (CInt 0)
defaultValue TDouble = Const (CDouble 0.0)
defaultValue TString = Lit ""
defaultValue TVoid = error "Tipo void não tem valor padrão"

tipo :: Parsec String u Tipo
tipo = choice
  [ reserved "int" >> return TInt
  , reserved "string" >> return TString
  , reserved "double" >> return TDouble
  ]

listaId = do
  id <- identifier
  lista <- listaId' <|> return []
  return (id : lista)

listaId' = do
  symbol ","
  id <- identifier
  lista <- listaId' <|> return []
  return (id : lista)
  <|> return []

bloco :: Parser Bloco
bloco = do
  symbol "{"
  cmds <- listaCmd
  symbol "}"
  return cmds

listaCmd :: Parser [Comando]
listaCmd = sepEndBy comando (symbol ";")

chamadaFuncao :: Parsec String u Comando
chamadaFuncao = do
  nome <- identifier
  args <- parens (listaParametros'' <|> return [])
  symbol ";"
  return $ Proc nome args

listaParametros :: ParsecT String u Data.Functor.Identity.Identity [Expr]
listaParametros = parens (expr `sepBy` symbol ",") <|> return []

listaParametros' :: Parsec String u [Expr]
listaParametros' = expr `sepBy` symbol ","

listaParametros'' :: Parsec String u [Expr]
listaParametros'' = (symbol "," >> listaParametros')
                    <|> return []

comando :: Parser Comando
comando = choice [ Text.Parsec.try $ string "return" >> spaces >> (Ret <$> optionMaybe expr) <* char ';'
                 , Text.Parsec.try $ do
                     string "if"
                     spaces
                     condicao <- exprL
                     spaces
                     bloco1 <- bloco
                     spaces
                     elseOp <- optionMaybe (string "else" >> spaces >> bloco)
                     return $ If condicao bloco1 (fromMaybe [] elseOp)
                 , Text.Parsec.try $ do
                     string "while"
                     spaces
                     condicao <- exprL
                     spaces
                     While condicao <$> bloco
                 , Text.Parsec.try $ do
                     ident <- identifier
                     spaces
                     char '='
                     spaces
                     exp <- expr
                     char ';'
                     return $ Atrib ident exp
                 , Text.Parsec.try $ do
                     string "print"
                     spaces
                     char '('
                     exp <- expr
                     char ')'
                     char ';'
                     return $ Print exp
                 , Text.Parsec.try $ do
                     string "read"
                     spaces
                     char '('
                     ident <- identifier
                     char ')'
                     char ';'
                     return $ Leitura ident
                 , chamadaFuncao
                 ]

tvzExpressao :: Parser (Maybe Expr)
tvzExpressao = optionMaybe expr

senao :: Parser Bloco
senao = (reserved "else" >> listaCmd) <|> return []

-- Função para analisar o programa e retornar a árvore sintática abstrata
parsePrograma :: String -> Either ParseError Programa
parsePrograma = parse programa ""

-- Função de teste do analisador
testParser :: String -> IO ()
testParser input = case parsePrograma input of
  Left err -> putStrLn $ "Erro de análise: " ++ show err
  Right ast -> putStrLn $ "Árvore sintática abstrata:\n" ++ show ast


main = do putStr "Expressão:"
          e <- getLine
          testParser e

{- main :: IO ()
main = do
  let programaExemplo = unlines
        [ "int soma(int a, int b) {"
        , "    return a + b;"
        , "}"
        , ""
        , "int main() {"
        , "    int x = 10;"
        , "    int y = 20;"
        , "    int z = soma(x, y);"
        , "    print(z);"
        , "}"
        ]
  testParser programaExemplo -}