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
            | Seq [Stmt]
            | Expr
            | AddTo String Expr 
            | SubTo String Expr 
            | Printse Expr
    deriving (Eq, Show, Read)


type Env = Map.Map String Integer 
--type Variables = [([Char], Integer)]


variables = [("x",0)]

env = Map.fromList variables


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

printExpr = try ( do
                reserved "print";
                expr <- expressionParser;
                return (Printse expr)
                )



                

interpret stmt env =
                 case stmt of
                    If e1 s1 s2 e2 -> do 
                                      expr1 <- evalExpr e1;
                                      stmt <- if (expr1 == 0)
                                             then interpret s2 env
                                             else interpret s1 env
                                      expr2 <- evalExpr e2;
                                      return stmt
                                        

--                    AddTo s e -> do
--                                      expr <- evalExpr e;
--                                      updateEnvs s expr;
                                      

--                    Printse e -> do
--                                      expr1 <- evalExpr e;
--                                      print expr1;
--                                      return expr1;
                                    

                    Repeat s e -> do
                                      expr <- evalExpr e;
                                      stmt <- recursive s expr;
                                      return stmt
                                   
                    Prints s -> do 
                                      case Map.lookup s env of
                                        Just x -> print x
            
                                      
                                      

recursive stmt expr = do
    s <- interpret stmt env
    if expr == 0
        then return s
        else recursive stmt expr


--updateEnvs s e = do
--    v <- Map.lookup s env



evalInteger op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y
                    return (a `op` b)

evalExpr (Const n)        = return n
evalExpr (Var v)          = case Map.lookup v env of
    Just x -> return x
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




main = do
              s <- readFile "test.hs"
              putStrLn (show s)
              case parse parser "" (read s) of
                Left e -> error $ show e
                Right s -> interpret s env

