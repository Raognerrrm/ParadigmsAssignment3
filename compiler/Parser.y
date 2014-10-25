{
module Parser where
import Data.Char
import System.IO
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      FUNCTION        { TokenFunction }
      BEGIN           { TokenBegin }
      END             { TokenEnd }
      VARS            { TokenVars }
      IF              { TokenIf }
      THEN            { TokenThen }
      ELSE            { TokenElse }
      RETURN          { TokenReturn }
      id              { TokenId $$ }
      num             { TokenNum $$ }
      op              { TokenOp $$ }
      ';'             { TokenSemi }
      ','             { TokenComma }
      '='             { TokenEq }
      '('             { TokenOB }
      ')'             { TokenCB }
      
%%

Program : Functions { Program $1 }

Functions : {- empty -} { [] }
          | Function Functions { $1 : $2 }

Function : FUNCTION id Arguments Variables Block { Function $2 $3 $4 $5 }

Arguments : '(' {- empty -} ')' { Arguments [] }
          | '(' IdList ')' { Arguments $2 }

Variables : {- empty -} { Variables [] }
          | VARS IdList ';' { Variables $2 }

IdList  : id { [$1] }
        | id ',' IdList { $1 : $3 }

Block : BEGIN Statements END { Block $2 }

Statements : {- empty -} { [] }
           | Statement ';' Statements { $1 : $3 }

Statement : id '=' Expression { StmtAssign $1 $3 }
          | IF id THEN Block ELSE Block { StmtIfThenElse $2 $4 $6 } 
          | IF id THEN Block { StmtIfThen $2 $4 }
          | RETURN id { StmtReturn $2 }

Expression : num { ExpNum $1 }
           | id Arguments { ExpIdArg $1 $2 }
           | id { ExpId $1 }
           | '(' Expression op Expression ')' { ExpOp $2 $3 $4 }
      
{
parseError :: [Token] -> a
parseError _ = error "Syntax Error."

type Var      = String
type FuncName = String

data Program
  = Program [Function]
  deriving Show
  
data Function
  = Function FuncName Arguments Variables Block
  deriving Show

data Arguments
  = Arguments [Var]
  deriving Show

data Variables
  = Variables [Var]
  deriving Show

data Block
  = Block [Statement]
  deriving Show

data Statement
  = StmtAssign Var Expression
  | StmtIfThen Var Block
  | StmtIfThenElse Var Block Block
  | StmtReturn Var
  deriving Show

data Expression
  = ExpNum Int
  | ExpId Var
  | ExpIdArg FuncName Arguments
  | ExpOp Expression Op Expression
  deriving Show
  
data Op
  = Add
  | Sub
  | Mul
  | Div
  | LessThan
  | GreaterThan
  | DEq
  deriving Show

data Token
  = TokenFunction
  | TokenBegin
  | TokenEnd
  | TokenVars
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenReturn
  | TokenId Var
  | TokenNum Int
  | TokenOp Op
  | TokenSemi
  | TokenComma
  | TokenEq
  | TokenOB
  | TokenCB
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('-':cs) = lexMinus ('-':cs)
lexer ('=':cs) = lexEq ('=':cs)
lexer (';':cs) = TokenSemi : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('+':cs) = TokenOp Add : lexer cs
lexer ('*':cs) = TokenOp Mul : lexer cs
lexer ('/':cs) = TokenOp Div : lexer cs
lexer ('<':cs) = TokenOp LessThan : lexer cs
lexer ('>':cs) = TokenOp GreaterThan : lexer cs

lexMinus ('-':cs) =
   case span isDigit cs of
      ("",rest)  -> TokenOp Sub : lexer rest
      (num,rest) -> TokenNum (read ('-':num)) : lexer rest -- Negative numbers
      
lexEq ('=':cs) =
   case span (\x -> x == '=') cs of
      ("",rest)  -> TokenEq : lexer rest
      ("=",rest) -> TokenOp DEq : lexer rest -- Double equals ==

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span (\x -> (isDigit x) || (isAlpha x)) cs of
      ("FUNCTION",rest) -> TokenFunction : lexer rest
      ("BEGIN",rest)    -> TokenBegin : lexer rest
      ("END",rest)      -> TokenEnd : lexer rest
      ("IF",rest)       -> TokenIf : lexer rest
      ("THEN",rest)     -> TokenThen : lexer rest
      ("ELSE",rest)     -> TokenElse : lexer rest
      ("RETURN",rest)   -> TokenReturn : lexer rest
      ("VARS",rest)     -> TokenVars : lexer rest
      (id,rest)         -> TokenId id : lexer rest
      
parseFile :: FilePath -> IO Program -- Once an IO, always an IO
parseFile f = do
    contents <- readFile f
    return (calc (lexer contents))
}
