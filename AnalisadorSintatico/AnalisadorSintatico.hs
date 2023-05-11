import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Token(identifier, braces)
import qualified Data.Functor.Identity
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft),
      Operator(Infix, Prefix) )

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

expr = buildExpressionParser tabela fator
       <?> "expression"
exprL = buildExpressionParser tabelaL exprR
exprR = do {e1 <- expr; o <- opR; Rel . o e1 <$> expr;}

programa = do
  funcoes <- listaFuncoes
  blocoPrincipal <- bloco
  eof
  return (funcoes, blocoPrincipal)

listaFuncoes = many funcao

funcao :: Parsec String u (String, [String], Bloco)
funcao = do
  tipo <- tipoRetorno
  id <- identifier lexico
  parametros <- declParametros
  let idsParametros = map snd parametros
  blocoPrincipal <- bloco
  return (id, idsParametros, blocoPrincipal)

tipoRetorno :: Parsec String u Tipo
tipoRetorno = tipo <|> (reserved "void" >> return TVoid)

declParametros :: Parsec String u [(Tipo, String)]
declParametros = try (do
  tipoParam <- tipo
  id <- identifier lexico
  [outrosParams] <- parametros
  return $ (tipoParam, id) : outrosParams)
  <|> return []

parametros :: ParsecT String u Data.Functor.Identity.Identity [[(Tipo, String)]]
parametros = option [] $ symbol "," >> many1 declParametros

blocoPrincipal :: Parsec String u Bloco
blocoPrincipal = do
  symbol "{"
  cmds <- blocoPrincipal'
  symbol "}"
  return cmds

blocoPrincipal' :: Parsec String u Bloco
blocoPrincipal' = do
  decls <- option [] declaracoes
  cmds <- listaCmd
  return (concat decls ++ cmds)

declaracoes = many $ try declaracao <|> skip
  where
    declaracao = do
      t <- tipo
      ids <- listaId
      symbol ";"
      return $ map (\id -> Atrib id (defaultValue t)) ids
    skip = manyTill anyChar (lookAhead $ try (tipo >> listaId >> symbol ";")) >> return []

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
  id <- identifier lexico
  lista <- listaId' <|> return []
  return (id : lista)

listaId' = do
  symbol ","
  id <- identifier lexico
  lista <- listaId' <|> return []
  return (id : lista)
  <|> return []

bloco :: Parsec String u Bloco
bloco = do
  symbol "{"
  cmds <- listaCmd
  symbol "}"
  return cmds

listaCmd :: Parsec String u [Comando]
listaCmd = many comando

chamadaFuncao :: Parsec String u Comando
chamadaFuncao = do
  nome <- identifier lexico
  args <- parens (listaParametros <|> return [])
  symbol ";"
  return $ Proc nome args

listaParametros :: Parsec String u [Expr]
listaParametros = try listaParametros' <|> return []

listaParametros' :: Parsec String u [Expr]
listaParametros' = expr `sepBy` symbol ","

listaParametros'' :: Parsec String u [Expr]
listaParametros'' = (symbol "," >> listaParametros')
                    <|> return []

comando = try (do reserved "return"
                  e <- tvzExpr
                  symbol ";"
                  return (Ret e))
        <|> try (do {reserved "if";
                  e <- parens exprL;
                  b1 <- bloco;
                  reserved "else";
                  If e b1 <$> bloco;})
      <|> try (do reserved "while"
                  e <- parens exprL
                  While e <$> bloco)
      <|> try (do i <- identifier lexico
                  reservedOp "="
                  e <- expr
                  symbol ";"
                  return (Atrib i e))
      <|> try (do reserved "print"
                  e <- parens expr
                  symbol ";"
                  return (Print e))
      <|> try (do reserved "read"
                  i <- parens (identifier lexico)
                  symbol ";"
                  return (ReadAction i))
      <|> chamadaFuncao

tvzExpr = optionMaybe expr

senao :: Parsec String u Bloco
senao = (reserved "else" >> listaCmd) <|> return []