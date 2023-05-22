-- Alunos: Carlos Bunn e Paulo Ricardo
import Text.Parsec
import Text.Parsec.Language ( emptyDef )
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr

-- Definição de dados
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
                deriving Show

--Definição da linguagem
lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["{","}","+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&", "|", "!"]
            , T.reservedNames   = ["while", "if", "int", "double", "string", "void", "print", "read"]
          }

lexico = T.makeTokenParser lingDef
-- Tokens
symbol = T.symbol lexico
reserved = T.reserved lexico
reservedOp = T.reservedOp lexico
parens = T.parens lexico
natural = T.natural lexico
identifier = T.identifier lexico
palavra = T.stringLiteral lexico
num = T.naturalOrFloat lexico
command = T.comma lexico
pv = T.semi lexico
braces = T.braces lexico

tabela = [[prefix "-" Neg], 
        [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ], 
        [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]]

tabelaL = [[prefix "!" Not],
    [binario "&&" (:&:) AssocLeft,
     binario "||" (:|:) AssocLeft]]

opR = do {(reservedOp "==" >> return (:==:))
      <|> (reservedOp ">=" >> return (:>=:))
      <|> (reservedOp "<=" >> return (:<=:))
      <|> (reservedOp ">" >> return (:>:))
      <|> (reservedOp "<" >> return (:<:))
      <|> (reservedOp "/=" >> return (:/=:))
      <?> "relational operator"}

prefix name fun = Prefix (do {reservedOp name; return fun })
binario name fun = Infix (do {reservedOp name; return fun })

list p = sepBy p command

fator = do parens expr
        <|> constant
        <|> Lit <$> palavra
        <|> try (do
            i <- identifier;
            args <- parens (list expr)
            return (Chamada i args))
        <|> IdVar <$> identifier
        <?> "simple expression"

constant = do {c <- num; case c of Left  n -> return (Const (CInt n)); Right n -> return (Const (CDouble n))}

expr = buildExpressionParser tabela fator 
        <?> "expression"

exprL = parens logico <|> Rel <$> exprR
        <?> "logical expression"
exprR = do {e1 <- expr; o <- opR; o e1 <$> expr;}
logico = buildExpressionParser tabelaL exprL
      <?> "logical expression"

programa :: Parsec String u Programa
programa = do
        listaF <- listaFuncoes
        (var, bloco) <- blocoPrincipal
        let (func, _, _) = unzip3 listaF
            lf = aux listaF
        return $ Prog func lf var bloco

aux [] = []
aux ((id :->: t, v, b) : ts) = (id, v, b) : aux ts

listaFuncoes :: Parsec String u [(Funcao, [Var], [Comando])]
listaFuncoes = do
        func <- funcao
        restoFunc <- listaFuncoes
        return $ func : restoFunc
        <|> return []

funcao :: Parsec String u (Funcao, [Var], [Comando])
funcao = do
    tipoR <- tipoRetorno
    id <- identifier
    p <- parens declParametros
    (vars, bloco) <- blocoPrincipal
    let func = id :->: (p, tipoR)
    return (func, vars, bloco)

tipoRetorno :: Parsec String u Tipo
tipoRetorno = do tipo <|> do reserved "void" >> return TVoid

tipo :: Parsec String u  Tipo
tipo = do {( reserved "int" >> return TInt)
   <|> (reserved "string" >> return TString)
   <|> (reserved "double" >> return TDouble)
   <?> "type"}

declParametros :: Parsec String u [Var]
declParametros = do
    tipo <- tipo
    id <- identifier
    params <- parametros
    return $ (id :#: tipo) : params
    <|> do reserved "void" >> return []

parametros :: Parsec String u [Var]
parametros = do
    command;
    declParametros;
    <|> return []

blocoPrincipal :: Parsec String u ([Var], [Comando])
blocoPrincipal = braces blocoPrincipal'

blocoPrincipal' :: Parsec String u ([Var], [Comando])
blocoPrincipal' = do
    decls <- declaracoes
    cmds <- listaCmd
    return (decls, cmds)

declaracoes :: Parsec String u [Var]
declaracoes = do
  t <- tipo;
  ids <- listaId;
  pv;
  decls <- declaracoes;
  return $ aux2 (t, ids);
  <|> return []

aux2 (tipo, []) = []
aux2 (tipo, id : ids) = (id :#: tipo) : aux2 (tipo, ids)

listaId :: Parsec String u [String]
listaId = do
  id <- identifier
  lista <- listaId'
  return (id : lista)

listaId' :: Parsec String u [String]
listaId' = do
  command
  listaId
  <|> return []

bloco :: Parsec String u [Comando]
bloco = braces listaCmd

listaCmd :: Parsec String u [Comando]
listaCmd = do
  comandos <- comando
  cmds <- listaCmd
  return (comandos: cmds)
  <|> return []

chamadaFuncao :: Parsec String u Comando
chamadaFuncao = do
  nome <- identifier
  args <- parens listaParametros
  pv
  return (Proc nome args)

listaParametros :: Parsec String u [Expr]
listaParametros = do
        listaParametros'
        <|> return []

listaParametros' :: Parsec String u [Expr]
listaParametros' = do
            expressao <- expr
            listaP <- listaParametros''
            return $ expressao : listaP

listaParametros'' :: Parsec String u [Expr]
listaParametros'' = do
            command
            listaParametros'
            <|> return []

tvzExpressao :: Parsec String u (Maybe Expr)
tvzExpressao = do
        Just <$> expr
        <|> return Nothing

senao :: Parsec String u [Comando]
senao = braces (many comando)

comando :: Parsec String u Comando
comando = do
        cif
    <|> cwhile
    <|> try (catriborfunc)
    <|> cread
    <|> cprint
    <|> creturn
    
creturn = try(do
        reserved "return"
        tvz <- tvzExpressao
        pv
        return (Ret tvz))

cprint = do
        reserved "print"
        expre <- parens expr
        pv
        return (Imp expre)

cread = do
        reserved "read"
        id <- parens identifier
        pv
        return (Leitura id)

catriborfunc = try (do
        ident <- identifier
        reserved "="
        expre <- expr
        pv
        return (Atrib ident expre))
        <|> chamadaFuncao

cwhile = do
        reserved "while"
        condicao <- parens logico
        While condicao <$> bloco

cif = try(do
        reserved "if"
        expreLogica <- parens logico
        bloco <- bloco
        reserved "else"
        If expreLogica bloco <$> senao)
        <|> do 
            reserved "if"
            expreLogica <- parens logico
            bloco <- bloco
            return (If expreLogica bloco [])

-- Para executar o arquivo prog1.diq
parserPrograma = do
      e <- programa
      eof
      return e

parser string = case runParser parserPrograma [] "Expressions" string of
    Left error -> print error
    Right x -> print x

main = do
    e <- readFile "prog1.txt"
    parser e
