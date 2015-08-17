module Parser where

import Term
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language


potDef = emptyDef
         { commentStart     = "{-|",
           commentEnd       = "|-}",
           commentLine      = "--",
           nestedComments   = True,
           identStart       = lower,
           identLetter      = do alphaNum <|> oneOf "_'",
           reservedNames    = ["let", "in", "where"],
           caseSensitive    = True
         }


{-|
    Parse Pot to Term
|-}

lexer = T.makeTokenParser potDef

symbol      = T.symbol lexer
bracks      = T.parens lexer
semic       = T.semi lexer
comm        = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
natural     = T.natural lexer


list2ConsList [] = ConApp "Nil" []
list2ConsList (t:ts) = ConApp "Cons" [t, (list2ConsList ts)]

conName = do
             c <- upper
             cs <- many alphaNum
             return (c:cs)

makeWhere t@(FVarApp x as) [] = t
makeWhere t@(FVarApp x as) fds = makeFuns [x] (Where (x,as) fds)

makeFuns fnames (FVarApp x ts) = if x `elem` fnames
                                 then FunCall (x, (map (makeFuns fnames) ts))
                                 else FVarApp x (map (makeFuns fnames) ts)
makeFuns fnames (BVarApp i ts) = BVarApp i (map (makeFuns fnames) ts)
makeFuns fnames (ConApp c ts) = ConApp c (map (makeFuns fnames) ts)
makeFuns fnames (Lambda x t) = Lambda x (makeFuns fnames t)
makeFuns fnames (Let x t1 t2) = Let x (makeFuns fnames t1) (makeFuns fnames t2)
makeFuns fnames (FunCall (f, ts)) = FunCall (f, (map (makeFuns fnames) ts))
makeFuns fnames (Where (f, as) fds) = Where (f, as) (map (\(f, ts, t) -> (f, ts, makeFuns fnames t)) fds)

{-|
    Parsers
|-}

parseExpr = parse expr "(ERROR)"

expr = buildExpressionParser prec term

prec = []

term =   do -- Where
            x <- identifier
            as <- many atom
            fds <-  do
                       reserved "where"
                       fds <- sepBy1 fundef (symbol "|")
                       return fds
                <|> do
                       spaces
                       return []
            return (makeWhere (FVarApp x as) fds)
     <|> do -- ConApp
            c <- conName
            as <-  do
                      as <- bracks (sepBy1 expr comm)
                      return as
               <|> do
                      spaces
                      return []
            return (ConApp c as)
     <|> do -- Lambda
            symbol "\\"
            xs <- many1 identifier
            symbol "."
            e <- expr
            return (foldr (\x t -> (Lambda x t)) e xs)
     <|> do -- Let
            reserved "let"
            x <- identifier
            symbol "="
            e1 <- expr
            reserved "in"
            e2 <- expr
            return (Let x e1 e2)
     <|> do -- other expressions
            a <- atom
            return a

atom =   do -- FVarApp
            x <- identifier
            return (FVarApp x [])
     <|> do -- [list]
            symbol "["
            ts <- sepBy expr comm
            symbol "]"
            return (list2ConsList ts)
     <|> do -- (expression)
            e <- bracks expr
            return e

fundef = do
            f <- identifier
            ps <- many pattern
            symbol "="
            e <- expr
            return (f, ps, e)

pattern =   do -- variable
               x <- identifier
               return (FVarApp x [])
        <|> do -- constructor application
               c <- conName
               as <- many pattern
               return (ConApp c as)
        <|> do -- constructor
               c <- conName
               spaces
               return (ConApp c [])
        <|> do -- (pattern)
               pat <- bracks pattern
               return pat