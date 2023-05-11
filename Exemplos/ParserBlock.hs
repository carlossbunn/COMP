import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

type Id = String

data Type = TDouble | TInt | TString | TVoid deriving Show

data TCons = CDouble Double | CInt Integer | CString String deriving Show

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar String | Chamada Id [Expr] | Lit String deriving Show

data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr  deriving Show

data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show

data Comando = If ExprL Bloco Bloco | While ExprL Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret Expr deriving Show

type Bloco = [Comando]

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!"]
            , T.identStart      = letter <|> char '_'
            , T.identLetter     = alphaNum <|> char '_'
            , T.reservedNames   = ["while", "return"]
          }

lexico = T.makeTokenParser lingDef

natural       = T.natural lexico
float         = T.float lexico
symbol        = T.symbol lexico
parens        = T.parens lexico
reservedOp    = T.reservedOp lexico
reserved      = T.reserved lexico
identifier    = T.identifier lexico


tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
            ]

tabelaL   = [[prefix "!" Not]
            , [binario "&&" (:&:) AssocLeft]
            , [binario "||" (:|:) AssocLeft]
            ]

binario name fun = Infix (do {reservedOp name; return fun })
prefix   name fun       = Prefix (do {reservedOp name; return fun })



expr = buildExpressionParser tabela fator
       <?> "expression"

exprR = do {e1 <- expr; o <- opR; Rel . o e1 <$> expr;}

exprL = buildExpressionParser tabelaL exprR

opR = do {reservedOp "=="; return (:==:)}
    <|> do {reservedOp "/="; return (:/=:)}
    <|> do {reservedOp "<"; return (:<:)}
    <|> do {reservedOp ">"; return (:>:)}
    <|> do {reservedOp ">="; return (:>=:)}
    <|> do {reservedOp "<="; return (:<=:)}

opL = do {reservedOp "&&"; return (:&:)}
    <|> do {reservedOp "||"; return (:|:)}



bloco = do {symbol "{"; cs <- listaCmd; symbol "}"; return cs}

listaCmd = do {c <- comando; cs <- listaCmd; return (c:cs)}
           <|> do {return []}

comando = do {reserved "return"; e <- expr; symbol ";";return (Ret e)}
      <|> do {reserved "while"; e <- parens exprL; While e <$> bloco;}

fator = parens expr
       <|> do {Const . CInt <$> natural;}
       <?> "simple expression"

partida :: Parsec String u Bloco
partida = do {e <- bloco; eof; return e}

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> (print v)

main = do putStr "Expressão:"
          e <- getLine
          parserExpr e
