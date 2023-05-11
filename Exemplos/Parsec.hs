import Text.Parsec

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Cons Integer  deriving Show

partida :: Parsec String u Expr
partida = do {e <- expr; eof; return e}

expr = do {spaces; expr'}

expr' = do {char '+'; e1 <- expr; Add e1 <$> expr;}
        <|> do {char '-'; e1 <- expr; Sub e1 <$> expr;}
        <|> do {char '*'; e1 <- expr; Mul e1 <$> expr;}
        <|> do {char '+'; e1 <- expr; Div e1 <$> expr;}
        <|> do {constante}


constante = do {d <- many1 digit; return (Cons (read d))}

parserEP = runParser partida [] "Expressões pre-fixadas"

parserExpr s = case parserEP s of
                     Left er -> print er
                     Right v -> print "resultado" >> print v

main = do putStr "Expressão:"
          e <- getLine
          parserExpr e


