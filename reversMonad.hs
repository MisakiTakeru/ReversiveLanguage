module Main (main) where

import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import Control.Monad
import System.IO
import Text.Parsec.String
import Control.Applicative((<*))
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment(getArgs)
import qualified Data.Map as Map
import Control.Monad.IO.Class

data Val = Number Int
    deriving (Eq, Show, Read)

data Expr = Const Integer | Var String | Binary Operators Expr Expr | Val
    deriving (Eq, Show, Read)


data Operators =  Add
                | Sub
                | Mul
                | Div
                | Mod
                | Eq
                | Lt
                | Or
    deriving (Eq, Show, Read)

data Stmt = If Expr Stmt Stmt Expr
            | Repeat Stmt Expr 
            | Stmt Stmt 
            | Prints String 
            | Read String
            | Seq [Stmt]
            | Expr
            | AddTo String Expr 
            | SubTo String Expr 
            | Printse Expr
    deriving (Eq, Show, Read)


type Env = Map.Map String Integer 
--type Env = [([Char], Integer)]


newtype Evaluate a = Evaluate {runEvaluate :: Env -> Either String (a,Env)}

instance Functor Evaluate where
    fmap f m = m >>= \a -> return (f a)


instance Applicative Evaluate where
    pure = return
    (<*>) = ap

instance Monad Evaluate where
    return a = Evaluate (\env -> return (a,env))
    m >>= f = Evaluate (\x -> case runEvaluate m x of
                                Left a -> Left a
                                Right (a, env') -> runEvaluate (f a) env')
    fail s = Evaluate (\_ -> Left s)

modifyEnvironment :: String -> Integer -> Evaluate()
modifyEnvironment name val = Evaluate $ \env -> Right ((), Map.insert name val env)



getVariable :: String -> Evaluate Integer
getVariable name = Evaluate $ \env -> case Map.lookup name env of
    Nothing -> Left ("Variable" ++ name ++ "is not in the environment")
    Just a -> Right (a, env)
  


variables = [("x",1),("y",0)]

env = Map.fromList variables

occurs var [] = 0
occurs var ((x,y):xs) = (if var == x then 1 else 0) + occurs var xs



languageDef = 
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = [ "if"
                                      , "then"
                                      , "else"
                                      , "fi"
                                      , "repeat"
                                      , "until"
                                      , "call"
                                      , "uncall"
                                      , "print"
                                      , "read"
                                      ]
             , Token.reservedOpNames = ["+","-","*","/","%","<","=","||","+=","-="]
             }



lexer = Token.makeTokenParser languageDef

reserved = Token.reserved lexer
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
-- right now ignoring parens maybe implemented in future implementation
-- parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

expressionParser :: Parser Expr
expressionParser = buildExpressionParser operator term
                 <?> "expression"


operator = [
               [Infix  (reservedOp "%"   >> return (Binary Mod)) AssocLeft]
             , [Infix  (reservedOp "*"   >> return (Binary Mul)) AssocLeft
             ,  Infix  (reservedOp "/"   >> return (Binary Div)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Binary Add)) AssocLeft
             ,  Infix  (reservedOp "-"   >> return (Binary Sub)) AssocLeft]
            ] 


term =  liftM Const integer
    <|> liftM Var identifier
 

parser :: Parser Stmt
parser = whiteSpace >> stmtParser

stmtParser :: Parser Stmt
stmtParser = 
    do  list <- (sepBy1 statement semi)
        return $ if length list == 1 then head list else Seq list

        

statement :: Parser Stmt
statement = ifStmt 
        <|> printStmt
        <|> addStmt
        <|> subStmt
        <|> printExpr
        <|> repeatStmt

ifStmt = do  
        reserved "if";
        expr1 <- expressionParser;
        reserved "then";
        first <- stmtParser;
        reserved "else";
        second <- stmtParser;
        reserved "fi";
        expr2 <- expressionParser;
        return (If expr1 first second expr2)
        
repeatStmt = do
            reserved "repeat";
            stmt <- stmtParser;
            reserved "until";
            expr <- expressionParser;
            return (Repeat stmt expr)
                               
addStmt = try ( do
             var <- identifier;
             reservedOp "+=";
             expr <- expressionParser;
             return (AddTo var expr)
              )     
            
subStmt = try ( do 
             var <- identifier;
             reservedOp "-=";
             expr <- expressionParser;
             return (SubTo var expr)
              )
                   


printStmt = try ( do
            reserved "print";
            var <- identifier;
            return (Prints var)
                )

readStmt = try ( do
            reserved "read";
            var <- identifier;
            return (Read var)
                )

printExpr = try ( do
                reserved "print";
                expr <- expressionParser;
                return (Printse expr)
                )


interpret :: Stmt -> Evaluate()
interpret (If e1 s1 s2 e2) = do
    expr1 <- evalExpr e1;
    expr2 <- evalExpr e2;
    case (expr1 > 0 || expr1 == 0) of
        False -> fail "If expression is not allowed to be lower than 0"
        True -> case expr1 of
            0 -> interpret s2
            _ -> interpret s1

interpret (AddTo s e) = do
    var <- getVariable s
    expr <- evalExpr e
    modifyEnvironment s (var + expr)


interpret (SubTo s e) = do
    var <- getVariable s
    expr <- evalExpr e
    modifyEnvironment s (var - expr)
                                     

interpret (Repeat s e) = do
    expr <- evalExpr e
    case (expr > 0) of
        False -> fail "First expression must be true"
        True -> recursive s e

interpret (Seq(x:xs)) = do 
    interpret x 
    interpret (Seq xs)

interpret (Seq []) = do return ()

{-
interpret (Read s) = do
    var <- unsafePerformIO (readLn :: Evaluate)
    case getVariable s of
        Left _ -> modifyEnvironment var
        Right a -> case a of
            0 -> modifyEnvironment var
            _ -> fail "Variable is a non zero value"


interpret (Prints s) = do
    var <- getVariable s
    putStrLn $ show var



--                    Printse e -> do
--                                      expr1 <- evalExpr e;
--                                      print expr1;
--                                      return expr1;
                                    
--        Prints s -> do 
--            case lookup s vars of
--                Just x -> print x

--                    Read s -> do
--                                      newVars <- if occurs s vars
--                                                    then checkEnvironment s
--                                                    else vars ++ [(s,0)]

                                      
        Seq (s:ss) -> do
            sequences (s:ss) vars
        

sequences [] vars = return () 
sequences (s:ss) vars = do
    (stmt, newVars) <- interpret s vars 
    sequences ss newVars
                                      
-}                                      

recursive s e = do
    interpret s
    expr <- evalExpr e
    if expr == 0
        then interpret s
        else recursive s e


evalInteger op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y
                    return (a `op` b)

evalExpr (Const n)        = return n
evalExpr (Var v)          = getVariable v
evalExpr (Binary Add x y) = do evalInteger (+) x y
evalExpr (Binary Sub x y) = do evalInteger (-) x y
evalExpr (Binary Mul x y) = do evalInteger (*) x y
evalExpr (Binary Div x y) = do evalInteger (div) x y
evalExpr (Binary Mod x y) = do evalInteger (mod) x y


runInput :: String -> Stmt
runInput str = 
    case parse parser "" str of
        Left err -> error $ show  err
        Right r -> r


--runEval :: Stmt -> Either Error Value
--runEval stmt = case runEvaluate (interpret stmt)

main :: IO ()
main = do 
--    args <- getArgs
--    case args of
--        [file] -> do
            s <- readFile "test.hs"
            case parse parser "" (read s) of
                Left e -> error $ show e
                Right s -> case runEvaluate (interpret s) env of
                    Left e -> error $ show e
                    Right (s,_) -> return ()
--        _ -> error "path given is not a file"


{-
              s <- readFile "test.hs"
              putStrLn (show s)
              case parse parser "" (read s) of
                Left e -> error $ show e
                Right s -> runEvaluate (interpret s)

-}
