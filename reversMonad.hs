module Main (main) where

import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
--import Text.ParserCombinators.Parsec
import Control.Monad.State
import System.IO
import Text.Parsec.String
import Control.Applicative((<*))
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment(getArgs)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Text.Parsec

data Expr = Const Integer | Var String | Binary Operators Expr Expr
    deriving (Eq, Show, Read)

data Variable = Val Integer | Arr [Integer]

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
--            | Stmt Stmt 
            | Prints String 
            | Read String
            | Seq [Stmt]
--            | Expr
            | AddTo String Expr 
            | SubTo String Expr 
            | UnCall String
            | Call String
            | Push String Expr
            | Pop String Expr
--            | Printse Expr
    deriving (Eq, Show, Read)

data Procedure = Procedure String Stmt
--                 | [Procedure]

type VEnv = Map.Map String Variable 
type PEnv = Map.Map String Stmt
type Env = (VEnv, PEnv, Integer)


type InterpreterState a = StateT Env IO a
type ParserState a = Parsec String PEnv a

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
                                      , "pop"
                                      , "push"
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

expressionParser :: ParserState Expr
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
             , [Infix  (reservedOp "||"  >> return (Binary Or )) AssocLeft]
            ] 


term =  liftM Const integer
    <|> liftM Var identifier
 

parser :: ParserState PEnv
parser = whiteSpace >> procedureParser

procedureParser :: ParserState PEnv
procedureParser =
    do many1 procedure
       state <- getState
       return state

procedure :: ParserState ()
procedure = do
            reserved "procedure"
            name <- identifier
            stmt <- stmtParser
            modifyState (Map.insert name stmt)


stmtParser :: ParserState Stmt
stmtParser = 
    do  list <- (endBy statement semi)
        return $ Seq list

        

statement :: ParserState Stmt
statement = ifStmt 
        <|> printStmt
        <|> addStmt
        <|> subStmt
        <|> repeatStmt
        <|> readStmt
        <|> callStmt
        <|> unCallStmt
        <|> popStmt
        <|> pushStmt

ifStmt :: ParserState Stmt
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

repeatStmt :: ParserState Stmt
repeatStmt = do
            reserved "repeat";
            stmt <- stmtParser;
            reserved "until";
            expr <- expressionParser;
            return (Repeat stmt expr)
                
addStmt :: ParserState Stmt
addStmt = try ( do
             var <- identifier;
             reservedOp "+=";
             expr <- expressionParser;
             return (AddTo var expr)
              )     

subStmt :: ParserState Stmt
subStmt = try ( do 
             var <- identifier;
             reservedOp "-=";
             expr <- expressionParser;
             return (SubTo var expr)
              )
                   

printStmt :: ParserState Stmt
printStmt = try ( do
            reserved "print";
            var <- identifier;
            return (Prints var)
                )

readStmt :: ParserState Stmt
readStmt = try ( do
            reserved "read";
            var <- identifier;
            return (Read var)
                )

callStmt :: ParserState Stmt
callStmt = do
            reserved "call";
            var <- identifier;
            return (Call var)

unCallStmt :: ParserState Stmt
unCallStmt = do
            reserved "uncall";
            var <- identifier;
            return (UnCall var)

popStmt :: ParserState Stmt
popStmt = do
            reserved "pop";
            var <- identifier;
            expr <- expressionParser;
            return (Pop var expr)

pushStmt :: ParserState Stmt
pushStmt = do
            reserved "push";
            var <- identifier;
            expr <- expressionParser;
            return (Push var expr)


findMain :: PEnv -> Stmt
findMain s = s Map.! "main"   





interpret :: Stmt -> InterpreterState ()
interpret (If e1 s1 s2 e2) = do
    (_,_,call) <- get
    entryExpr <- if call == 1
        then evalExpr e1;
        else evalExpr e2;
    case (entryExpr > 0 || entryExpr == 0) of
        False -> fail "If expression is not allowed to be lower than 0"
        True -> case entryExpr of
            0 -> interpret s2
            _ -> interpret s1
    exitExpr <- if call == 1
        then evalExpr e2;
        else evalExpr e1;
    case (exitExpr > 0 && entryExpr > 0) || (exitExpr == 0 && entryExpr == 0)  of
        True -> return ()
        False -> fail "entry and exit expression does not give the same branch"

interpret (AddTo s e) = do
    (_,_,call) <- get
    case call of
        1 -> changeVariable "+" s e
        -1 -> changeVariable "-" s e
        


interpret (SubTo s e) = do
    (_,_,call) <- get
    case call of
        1 -> changeVariable "-" s e
        -1 -> changeVariable "+" s e






interpret (Repeat s e) = do
    (_,_,call) <- get
    expr <- evalExpr e
    case (expr > 0) of
        False -> fail "First expression must be true"
        True -> recursive s e


interpret (Seq(x:xs)) = do 
    (_,_,call) <- get
    case call of
        1 -> do interpret x
                interpret (Seq xs)

        -1 -> do interpret (Seq xs)
                 interpret x

interpret (Seq []) = do return ()


interpret (Read s) = do
    (_,_,call) <- get
    case call of
        1 -> reading s
        -1 -> printing s

interpret (Prints s) = do
    (_,_,call) <- get
    case call of
        1 -> printing s
        -1 -> reading s

interpret (Call s) = do
    stmt <- gets (\(_, pEnv,_) -> pEnv Map.! s)
    interpret stmt
                        

interpret (UnCall s) = do
    modify (\(vEnv,pEnv,call) -> (vEnv,pEnv,(-call))) 
    stmt <- gets (\(_, pEnv,_) -> pEnv Map.! s)
    interpret stmt    
    modify (\(vEnv,pEnv,call) -> (vEnv,pEnv,(-call))) 


interpret (Push s e) = do
    (vEnv,_,call) <- get
    case call of
        1 -> pushing s e
        -1 -> popping s e

interpret (Pop s e) = do
    (vEnv,_,call) <- get
    case call of
        1 -> popping s e
        -1 -> pushing s e


pushing :: String -> Expr -> InterpreterState ()
pushing s e = do
    (vEnv,pEnv,call) <- get
    expr <- evalExpr e
    case Map.member s vEnv of
        False -> put $ (Map.insert s (Arr ([] ++ [expr])) vEnv,pEnv,call)
        True -> do v <- (gets (\(vEnv,_,_) -> vEnv Map.! s))
                   case v of 
                    Arr stack -> put $ (Map.insert s (Arr (stack ++ [expr])) vEnv,pEnv,call)
                    Val value -> fail "cannot push to value"


popping :: String -> Expr -> InterpreterState ()
popping s e = do
    (vEnv,pEnv,call) <- get
    expr <- evalExpr e
    case Map.member s vEnv of
        False -> fail "stack does not exist in environment"
        True -> do v <- (gets (\(vEnv,_,_) -> vEnv Map.! s))
                   case v of
                    Arr stack -> case (expr == (last stack)) of
                        True -> put $ (Map.insert s (Arr (init stack)) vEnv,pEnv,call)
                        False -> fail "top of stack must be equal to expression"
                    Val value -> fail "Cannot pop from value"


reading :: String -> InterpreterState ()
reading s = do
    (vEnv,pEnv,call) <- get
    x <- lift ( readLn :: IO Integer)
    case Map.member s vEnv of
        False -> put $ (Map.insert s (Val x) vEnv,pEnv,call)
        True -> do v <- (gets (\(vEnv,_,_) -> vEnv Map.! s))
                   case v of
                    Val 0 -> put $ (Map.insert s (Val x) vEnv,pEnv,call)
                    _ -> fail "existing variable is not 0"


printing :: String -> InterpreterState ()
printing s = do
    (vEnv,pEnv,call) <- get
    var <- gets (\(vEnv,_,_) -> vEnv Map.! s)
    case var of
        Arr v -> fail "cannot print the stack"
        Val v -> do put $ (Map.insert s (Val 0) vEnv,pEnv,call)
                    liftIO $ print v


changeVariable op s e = do
    (vEnv,pEnv,call) <- get
    expr <- evalExpr e
    case Map.member s vEnv of
        True -> do vars <- gets (\(vEnv,_,_) -> vEnv Map.! s)
                   case vars of
                    Arr v -> changeStack op v expr s
                    Val v -> case op of
                        "+" -> put $ (Map.insert s (Val (v + expr)) vEnv,pEnv,call)
                        "-" -> put $ (Map.insert s (Val (v - expr)) vEnv,pEnv,call)
                        _ -> fail "only possible to add or substract"

        False -> case op of
            "+" -> put $ (Map.insert s (Val expr) vEnv,pEnv,call)
            "-" -> put $ (Map.insert s (Val (-expr)) vEnv,pEnv,call)
            _ -> fail "only possible to add or substract"

changeStack :: String -> [Integer] -> Integer -> String -> InterpreterState ()
changeStack op stack expr name = do
    (vEnv,pEnv,call) <- get
    len <- getStackLength stack
    newVal <- getNewStackValue stack expr op
    newStack <- removeFromStack stack
    put $ (Map.insert name (Arr (newStack ++ [newVal])) vEnv,pEnv,call)

getStackLength :: [Integer] -> InterpreterState Int
getStackLength stack = do 
    return $ (length stack)

removeFromStack :: [Integer] -> InterpreterState [Integer]
removeFromStack stack = do
    return $ init stack

getNewStackValue :: [Integer] -> Integer -> String -> InterpreterState Integer
getNewStackValue stack value op = 
    case length stack of
        0 -> fail "stack is empty"
        _ -> case op of
            "+" -> do return $ (last stack) + value
            "-" -> do return $ (last stack) - value
            _ -> fail "only possible to add or substract"
    
                            

recursive s e = do
    interpret s
    expr <- evalExpr e
    if expr == 0 && expr >= 0
        then recursive s e
        else return ()


evalInteger op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y;
                    return $ (a `op` b)

evalConditionals :: (Integer -> Integer -> Bool) -> Expr -> Expr -> InterpreterState Integer
evalConditionals op x y = do
                    a <- evalExpr x;
                    b <- evalExpr y;
                    case (a `op` b) of
                        True -> return 1
                        False -> return 0


evalBool :: Expr -> Expr -> InterpreterState Integer
evalBool x y = do
            a <- evalExpr x;
            b <- evalExpr y;
            case (a, b) of
                (1,_) -> return 1
                (_,1) -> return 1
                _ -> return 0
                


evalExpr :: Expr -> InterpreterState Integer
evalExpr (Const n)        = return n
evalExpr (Var v)          = do s <- gets (\(vEnv,_,_) -> vEnv Map.! v)
                               case s of
                                Arr i -> case (length i) of
                                        0 -> fail "stack is empty"
                                        _ -> return $ last i
                                Val i -> return i
evalExpr (Binary Add x y) = do evalInteger (+) x y
evalExpr (Binary Sub x y) = do evalInteger (-) x y
evalExpr (Binary Mul x y) = do evalInteger (*) x y
evalExpr (Binary Div x y) = do evalInteger (div) x y
evalExpr (Binary Mod x y) = do evalInteger (mod) x y
evalExpr (Binary Eq  x y) = do evalConditionals (==) x y
evalExpr (Binary Lt  x y) = do evalConditionals (<) x y
evalExpr (Binary Or  x y) = do evalBool x y


{-
runInput :: String -> Stmt
runInput str = 
    case parse parser "" str of
        Left err -> error $ show  err
        Right r -> r
-}


main :: IO ()
main = do 
    (args: path) <- getArgs
    x <- case args of
        "call" -> return 1
        "uncall" -> return (-1) 
        _ -> error "neither call nor uncall was called"
    case path of
        [file] -> do
            s <- readFile file

            case runParser parser Map.empty "" (read s) of
                Left e -> error $ show e
                Right s -> let mainStatement = findMain s                   
                    in runStateT (interpret mainStatement) (Map.empty, s ,x) >> return ()

        _ -> error "path given is not a file"


