import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity(..))


type Id = String
type ConstString = String
type ConstFloat = Float
type ConstInt = Integer

data Tipo = TInt | TDouble | TString | TVoid deriving Show
data TCons = CDouble Double | CInt Int deriving Show
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/:Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] | Lit String deriving Show
data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving Show
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
        deriving Show

programa :: Parsec String () Programa
programa = do
  funcoes <- listaFuncoes
  Prog funcoes [] [] <$> blocoPrincipal

listaFuncoes :: ParsecT String () Identity [Funcao]
listaFuncoes = many funcao

funcao :: Parsec String () Funcao
funcao = do
  tipo <- tipoRetorno
  id <- identifier
  params <- declParametros
  bloco <- blocoPrincipal
  return (id, params :->: tipo)

tipoRetorno :: Parsec String () Tipo
tipoRetorno = try (reserved "int" >> return TInt)
          <|> try (reserved "string" >> return TString)
          <|> try (reserved "double" >> return TDouble)
          <|> (reserved "void" >> return TVoid)

declParametros :: Parsec String () [Var]
declParametros = try (do
  tipo <- tipo
  id <- identifier
  params <- parametros
  return $ (id :#: tipo) : params) <|> return []

parametros :: Parsec String () [Var]
parametros = try (comma >> declParametros) <|> return []

blocoPrincipal :: Parsec String () Bloco
blocoPrincipal = braces blocoPrincipal'

blocoPrincipal' :: Parsec String () Bloco
blocoPrincipal' = do
  decs <- declaracoes
  cmds <- listaCmd
  return $ decs ++ cmds

tipo :: Parsec String () Tipo
tipo = tipoRetorno

declaracoes :: Parsec String () [Comando]
declaracoes = try (do
  tipo <- tipo
  vars <- listaId
  semi
  decs <- declaracoes
  return $ map (\id -> id `Atrib` TConst (CInt 0)) vars ++ decs) <|> return []

listaId :: Parsec String ()[Id]
listaId = do
  id <- identifier
  ids <- listaId'
  return $ id : ids

listaId' :: Parsec String ()[Id]
listaId' = try (comma >> listaId) <|> return []

bloco :: Parsec String ()Bloco
bloco = braces listaCmd

listaCmd :: Parsec String() Bloco
listaCmd = many comando

comando :: Parsec String () Comando
comando = try comandoRetorno <|> try comandoCondicional <|> try comandoWhile <|> try comandoAtribuicao <|> try comandoLeitura <|> try comandoImpressao <|> comandoChamada

comandoRetorno :: Parsec String()Comando
comandoRetorno = do
  reserved "return"
  expr <- optionMaybe tvzExpressao
  semi
  return $ Ret expr

comandoCondicional :: Parsec String ()Comando
comandoCondicional = do
  reserved "if"
  expr <- parens exprLogica
  blocoIf <- bloco
  blocoElse <- option [] (reserved "else" >> bloco)
  return $ If expr blocoIf blocoElse

comandoWhile :: Parsec String ()Comando
comandoWhile = do
  reserved "while"
  expr <- parens exprLogica
  While expr <$> bloco

comandoAtribuicao :: Parsec String ()Comando
comandoAtribuicao = do
  id <- identifier
  reservedOp "="
  expr <- expr
  semi
  return $ Atrib id expr

comandoLeitura :: Parsec String ()Comando
comandoLeitura = do
  reserved "read"
  id <- parens identifier
  semi
  return $ Leitura id

comandoImpressao :: Parsec String ()Comando
comandoImpressao = do
  reserved "print"
  expr <- parens expr
  semi
  return $ Imp expr

comandoChamada :: Parsec String () Comando
comandoChamada = do
  id <- identifier
  args <- parens $ sepBy expr comma
  semi
  return $ Proc id args

chamadaFuncao :: Parsec String ()Expr
chamadaFuncao = do
  id <- identifier
  args <- parens $ sepBy expr comma
  return $ Chamada id args

listaParametros :: Parsec String ()[Expr]
listaParametros = try listaParametros' <|> return []

listaParametros' :: Parsec String ()[Expr]
listaParametros' = try (do
  expr <- expr
  exprs <- listaParametros'
  return $ expr : exprs) <|> return []

listaParametros'' :: Parsec String ()[Expr]
listaParametros'' = try (comma >> listaParametros') <|> return []

exprLogica :: Parsec String ()ExprL
exprLogica = try exprLogicaAnd <|> try exprLogicaOr <|> try exprLogicaNot <|> exprRelacional

exprLogicaAnd :: Parsec String ()ExprL
exprLogicaAnd = do
  left <- exprRelacional
  reservedOp "&&"
  right <- exprLogica
  return $ left :&: right

exprLogicaOr :: Parsec String ()ExprL
exprLogicaOr = do
  left <- exprRelacional
  reservedOp "||"
  right <- exprLogica
  return $ left :|: right

exprLogicaNot :: Parsec String ()ExprL
exprLogicaNot = do
  reservedOp "!"
  Not <$> exprLogica

exprRelacional :: Parsec String ()ExprL
exprRelacional = do
  left <- expr
  op <- opRelacional
  mkRelExpr op left <$> expr

