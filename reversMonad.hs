module Main (main) where

import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import Control.Monad.State
import System.IO
import Text.Parsec.String
import Control.Applicative((<*))
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment(getArgs)
import qualified Data.Map as Map
import Control.Monad.IO.Class


data Expr = Const Integer | Var String | Binary Operators Expr Expr
    deriving (Eq, Show, Read)


data Operators =  Add
                | Sub
                | Mul
                | Div
                | Mod
                | Eq
                | Lt
--                | Or
    deriving (Eq, Show, Read)

data Stmt = If Expr Stmt Stmt Expr
            | Repeat Stmt Expr 
--            | Stmt Stmt 
            | Prints String 
            | Read String
            | Seq [Stmt]
--            | Expr
            | AddTo String Expr 
            | SubTo String Expr 
--            | Printse Expr
    deriving (Eq, Show, Read)

data Procedure = Procedure String Stmt
--                 Seq [Procedure]

type VEnv = Map.Map String Integer 
type PEnv = Map.Map String Stmt
type Env = (VEnv, PEnv, Integer)


type MyState a = StateT Env IO a


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
                                      , "procedure"
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
             , [Infix  (reservedOp "="   >> return (Binary Eq )) AssocLeft
             ,  Infix  (reservedOp "<"   >> return (Binary Lt )) AssocLeft]
            ] 


term =  liftM Const integer
    <|> liftM Var identifier
 

parser :: Parser Stmt
parser = whiteSpace >> stmtParser

{-
procedure = do
            reserved "procedure"
            name <- identifier
            stmt <- statement
            return (Procedure name stmt)
-}

stmtParser :: Parser Stmt
stmtParser = 
    do  list <- (sepBy1 statement semi)
        return $ if length list == 1 then head list else Seq list

        

statement :: Parser Stmt
statement = ifStmt 
        <|> printStmt
        <|> addStmt
        <|> subStmt
        <|> repeatStmt
        <|> readStmt

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


interpret :: Stmt -> MyState ()
interpret (If e1 s1 s2 e2) = do
    (_,_,call) <- get
    expr <- if call == 1
        then evalExpr e1;
        else evalExpr e2;
    case (expr > 0 || expr == 0) of
        False -> fail "If expression is not allowed to be lower than 0"
        True -> case expr of
            0 -> interpret s2
            _ -> interpret s1

interpret (AddTo s e) = do
    (_,_,call) <- get
    case call of
        1 -> changeVariable "+" s e
        0 -> changeVariable "-" s e
        


interpret (SubTo s e) = do
    (_,_,call) <- get
    case call of
        1 -> changeVariable "-" s e
        0 -> changeVariable "+" s e






interpret (Repeat s e) = do
    (_,_,call) <- get
    expr <- evalExpr e
    case call of
        1 ->  case (expr > 0) of
                False -> fail "First expression must be true"
                True -> recursive s e
        0 -> case (expr > 0) of
                False -> uncallRecursive s e
                True -> fail "Uncall expression must be false first"

interpret (Seq(x:xs)) = do 
    (_,_,call) <- get
    case call of
        1 -> do interpret x
                interpret (Seq xs)

        0 -> do interpret (Seq xs)
                interpret x

interpret (Seq []) = do return ()


interpret (Read s) = do
    (_,_,call) <- get
    case call of
        1 -> reading s
        0 -> printing s

interpret (Prints s) = do
    (_,_,call) <- get
    case call of
        1 -> printing s
        0 -> reading s

reading :: String -> MyState ()
reading s = do
    (vEnv,pEnv,call) <- get
    x <- lift ( readLn :: IO Integer)
    case Map.member s vEnv of
        False -> put $ (Map.insert s x vEnv,pEnv,call)
        True -> do v <- (gets (\(vEnv,_,_) -> vEnv Map.! s))
                   case v of
                    0 -> put $ (Map.insert s x vEnv,pEnv,call)
                    _ -> fail "existing variable is not 0"


printing :: String -> MyState ()
printing s = do
    (vEnv,pEnv,call) <- get
    var <- gets (\(vEnv,_,_) -> vEnv Map.! s)
    put $ (Map.insert s 0 vEnv,pEnv,call)
    liftIO $ print var


changeVariable op s e = do
    (vEnv,pEnv,call) <- get
    expr <- evalExpr e
    case Map.member s vEnv of
        True -> do vars <- gets (\(vEnv,_,_) -> vEnv Map.! s)
                   case op of
                    "+" -> put $ (Map.insert s (vars + expr) vEnv,pEnv,call)
                    "-" -> put $ (Map.insert s (vars - expr) vEnv,pEnv,call)
                    _ -> fail "only possible to add or substract"

        False -> case op of
            "+" -> put $ (Map.insert s expr vEnv,pEnv,call)
            "-" -> put $ (Map.insert s (-expr) vEnv,pEnv,call)
            _ -> fail "only possible to add or substract"


recursive s e = do
    interpret s
    expr <- evalExpr e
    if expr == 0
        then return ()
        else recursive s e

uncallRecursive s e = do
    interpret s
    expr <- evalExpr e
    if expr == 0
        then recursive s e
        else return ()

evalInteger op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y;
                    return $ (a `op` b)

evalConditionals :: (Integer -> Integer -> Bool) -> Expr -> Expr -> MyState Integer
evalConditionals op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y;
                    case (a `op` b) of
                        True -> return 1
                        False -> return 0

evalExpr :: Expr -> MyState Integer
evalExpr (Const n)        = return n
evalExpr (Var v)          = do s <- gets (\(vEnv,_,_) -> vEnv Map.! v)
                               return s
evalExpr (Binary Add x y) = do evalInteger (+) x y
evalExpr (Binary Sub x y) = do evalInteger (-) x y
evalExpr (Binary Mul x y) = do evalInteger (*) x y
evalExpr (Binary Div x y) = do evalInteger (div) x y
evalExpr (Binary Mod x y) = do evalInteger (mod) x y
evalExpr (Binary Eq  x y) = do evalConditionals (==) x y
evalExpr (Binary Lt  x y) = do evalConditionals (<) x y



runInput :: String -> Stmt
runInput str = 
    case parse parser "" str of
        Left err -> error $ show  err
        Right r -> r


--runEval :: Stmt -> Either Error Value
--runEval stmt = case runEvaluate (interpret stmt)

main :: IO ()
main = do 
    (args: path) <- getArgs
    x <- case args of
        "call" -> return 1
        "uncall" -> return 0 
        _ -> error "neither call nor uncall was called"
--    print x
    case path of
        [file] -> do
            s <- readFile file
            case parse parser "" (read s) of
                Left e -> error $ show e
                Right s -> runStateT (interpret s) (Map.empty,Map.empty,x) >> return ()
        _ -> error "path given is not a file"


{-
              s <- readFile "test.hs"
              putStrLn (show s)
              case parse parser "" (read s) of
                Left e -> error $ show e
                Right s -> runEvaluate (interpret s)

-}
