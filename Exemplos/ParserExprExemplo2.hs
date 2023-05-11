import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Cons Integer | Neg Expr  deriving Show

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.reservedOpNames = ["+", "-", "/", "*"]
          }

lexico = T.makeTokenParser lingDef

natural       = T.natural lexico
symbol        = T.symbol lexico
parens        = T.parens lexico
reservedOp    = T.reservedOp lexico

tabela   = [[prefix "-" Neg]
            , [binario "*" Mul AssocLeft, binario "/" Div AssocLeft ]
            , [binario "+" Add AssocLeft, binario "-" Sub   AssocLeft ]
           ]

binario name fun = Infix (do {reservedOp name; return fun })
prefix   name fun       = Prefix (do {reservedOp name; return fun })

expr = buildExpressionParser tabela fator
       <?> "expression"

fator = parens expr
       <|> do {Cons <$> natural;}
       <?> "simple expression"

partida :: Parsec String u Expr
partida = do {e <- expr; eof; return e}

parserE = runParser partida [] "Expressoes"

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> print v

main = do putStr "Expressão:"
          e <- getLine
          parserExpr e
