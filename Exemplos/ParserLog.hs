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

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!"]
          }

lexico = T.makeTokenParser lingDef

natural       = T.natural lexico
symbol        = T.symbol lexico
parens        = T.parens lexico
reservedOp    = T.reservedOp lexico

tabela   = [[prefix "-" Neg]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:) AssocLeft ]
            ]

binario name fun = Infix (do {reservedOp name; return fun })
prefix   name fun       = Prefix (do {reservedOp name; return fun })

expr = buildExpressionParser tabela fator
       <?> "expression"

exprR = do {e1 <- expr; o <- op; o e1 <$> expr;}

op = do {reservedOp "=="; return (:==:)}
    <|> do {reservedOp "/="; return (:/=:)}
    <|> do {reservedOp "<"; return (:<:)}
    <|> do {reservedOp ">"; return (:>:)}
    <|> do {reservedOp ">="; return (:>=:)}
    <|> do {reservedOp "<="; return (:<=:)}


fator = parens expr
       <|> do {Const . CInt <$> natural;}
       <?> "simple expression"

partida :: Parsec String u ExprR
partida = do {e <- exprR; eof; return e}

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Expressão:"
          e <- getLine
          parserExpr e
