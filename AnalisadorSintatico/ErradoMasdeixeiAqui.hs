import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language ( emptyDef )
import qualified Data.Functor.Identity
import Text.ParserCombinators.Parsec ( Parser )
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft),
      Operator(Infix, Prefix) )
import Data.Maybe ( fromMaybe )

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
            , T.reservedNames   = ["while", "return", "if", "int", "double", "string", "void", "if", "else", "print", "read"]
          }

lexico = T.makeTokenParser lingDef
symbol = T.symbol lexico
reserved      = T.reserved lexico
reservedOp    = T.reservedOp lexico
parens        = T.parens lexico
natural       = T.natural lexico
identifier   = T.identifier lexico
palavra = T.stringLiteral lexico
num = T.naturalOrFloat lexico
command = T.comma lexico
pv = T.semi lexico
braces = T.braces lexico

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

list p = sepBy p comando

fator :: Parsec String () Expr
fator = parens expr
     <|> do {c <- num; case c of Left  n -> return (Const (CInt n)); Right n -> return (Const (CDouble n))}
     <|> do { i <- identifier; args <- parens (list expr); return (Chamada i args); }
     <|> do { Lit <$> palavra; }
     <|> do { IdVar <$> identifier; }
     <?> "simple expression"

expr = buildExpressionParser tabela fator <?> "expression"

exprL = do {parens logico
        <|> Rel
        <$> exprR}
exprR = do {e1 <- expr; o <- opR; o e1 <$> expr;}
logico = buildExpressionParser tabelaL exprL
      <?> "logical expression"

programa:: Parser Programa
programa = do
  funcoes <- listaFuncoes
  bloco <- blocoPrincipal
  return $ Prog (fst funcoes) (snd funcoes) (fst bloco) (snd bloco)

{- programa = do
  funcoes <- listaFuncoes
  m <- blocoPrincipal
  Prog funcoes [] [] <$> blocoPrincipal -}

listaFuncoes :: ParsecT String () Data.Functor.Identity.Identity ([Funcao], [(String, [Var], [Comando])])
listaFuncoes = do {f <- many funcao;
                  return (unzip f)}

funcao ::  ParsecT String () Data.Functor.Identity.Identity(Funcao, (String, [Var], [Comando]))
funcao = do
    tipo <- tipoRetorno
    id <- identifier
    p <- parens parametros
    blocoCmds <- braces bloco
    return (id :->: (p, tipo), (id, fst blocoCmds, snd blocoCmds))

tipoRetorno = tipo <|> (reserved "void" >> return TVoid)

declParametros = try (do
    param <- parametro
    outrosParams <- parametros
    pv
    return (param : outrosParams)
  ) <|> return []

parametros = sepBy parametro (symbol ",")

parametro = do
  tipoParam <- tipo
  nome <- identifier
  return (nome :#: tipoParam)

blocoPrincipal :: ParsecT String () Data.Functor.Identity.Identity ([Var], [Comando])
blocoPrincipal = do
  symbol "{"
  blocoPrincipal' <- blocoPrincipal'
  symbol "}"
  return blocoPrincipal'

blocoPrincipal' :: ParsecT String () Data.Functor.Identity.Identity ([Var], [Comando])
blocoPrincipal' = do
  decls <- declaracoes
  cmds <- listaCmd
  return (decls,snd cmds)
{- blocoPrincipal' = do
  declaracoes <- declaracoes
  listaCmd <- listaCmd
  return (declaracoes ++ listaCmd) -}

declaracoes :: Parser [Var]
declaracoes = do
  tipo <- tipo
  ids <- listaId
  pv
  return (map (:#: tipo) ids)

tipo = do {( reserved "int" >> return TInt)
   <|> (reserved "string" >> return TString)
   <|> (reserved "double" >> return TDouble)
   <?> "type"}

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


bloco = do
  symbol "{"
  cmds <- listaCmd
  symbol "}"
  return cmds

listaCmd :: Parser ([Var], [Comando])
listaCmd = do
  decls <- many declaracoes
  cmds <- many comando
  return (concat decls, cmds)
{- listaCmd = do
          d <- many declaracoes
          c <- many comando
          return (concat d ++ c) -}

chamadaFuncao = do
  nome <- identifier
  args <- parens (listaParametros'' <|> return [])
  symbol ";"
  return $ Proc nome args

listaParametros = parens (expr `sepBy` symbol ",")

listaParametros' = expr `sepBy` symbol ","

listaParametros'' = (symbol "," >> listaParametros')
                    <|> return []

bloco' :: ParsecT String () Data.Functor.Identity.Identity Bloco
bloco' = do
  symbol "{"
  cmds <- listaCmd
  symbol "}"
  return (snd cmds)

comando = choice [ Text.Parsec.try $ string "return" >> spaces >> (Ret <$> optionMaybe expr) <* char ';'
                 , Text.Parsec.try $ do
                     string "if"
                     spaces
                     condicao <- parens logico
                     spaces
                     bloco1 <- bloco
                     spaces
                     elseOp <- optionMaybe (string "else" >> spaces >> bloco)
                     return (If condicao (snd bloco1) [])
                 , Text.Parsec.try $ do
                     string "while"
                     condicao <- exprL
                     While condicao <$> bloco'
                 , Text.Parsec.try $ do
                     ident <- identifier
                     spaces
                     char '='
                     spaces
                     exp <- expr
                     pv
                     return (Atrib ident exp)
                 , Text.Parsec.try $ do
                     string "print"
                     spaces
                     exp <- parens expr
                     pv
                     return (Print exp)
                 , Text.Parsec.try $ do
                     string "read"
                     spaces
                     ident <- parens identifier
                     pv
                     return (Leitura ident)
                 , chamadaFuncao
                 ]


tvzExpressao = optionMaybe expr

senao = reserved "else" >> listaCmd

parsePrograma :: String -> Either ParseError Programa
parsePrograma = parse programa ""

testParser :: String -> IO ()
testParser input = case parsePrograma input of
  Left err -> putStrLn $ "Erro de análise: " ++ show err
  Right ast -> putStrLn $ "Árvore sintática abstrata:\n" ++ show ast

main = do
        e <- readFile "prog1.diq"
        testParser e