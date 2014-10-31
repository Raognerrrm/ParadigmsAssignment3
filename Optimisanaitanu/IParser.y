{
module IParser where
import Data.Char
import Data.List
import System.IO
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      lc              { TokenLc }
      ld              { TokenLd }
      st              { TokenSt }
      br              { TokenBr }
      ret             { TokenRet }
      call            { TokenCall }
      id              { TokenId $$ }
      num             { TokenNum $$ }
      op              { TokenOp $$ }
      reg             { TokenReg $$ }
      '('             { TokenOB }
      ')'             { TokenCB }
      
%%

Program : '(' Functions ')' { IProgram $2 }

Functions : {- empty -} { [] }
           | Function Functions { $1 : $2 }

Function : '(' id Arguments Blocks ')' { IFunction $2 $3 $4 }

Arguments : '(' IdList ')' { IArguments $2 }

IdList  : {- empty -} { [] }
        | id IdList { $1 : $2 }

Blocks : Block { [$1] }
       | Block Blocks { $1 : $2 }
       
Block : '(' num Instructions ')' { IBlock $2 $3 }

Instructions : Instruction { [$1] }
             | Instruction Instructions { $1 : $2 }
             
Instruction : '(' lc reg num ')' { Ilc $3 $4 }
            | '(' ld reg id ')' { Ild $3 $4 }
            | '(' st id reg ')' { Ist $3 $4 }
            | '(' op reg reg reg ')' { Iop $2 $3 $4 $5 }
            | '(' br reg num num ')' { Ibr $3 $4 $5 }
            | '(' ret reg ')' { Iret $3 }
            | '(' call reg id RegList ')' { Icall $3 $4 $5 }

RegList : {- empty -} { [] }
        | reg RegList { $1 : $2 }
        
{
parseError :: [Token] -> a
parseError _ = error "Syntax Error."

type Reg      = Int
type Var      = String
type FuncName = String
type BlockNum = Int

data IProgram 
	= IProgram [IFunction]
    deriving (Show, Eq)
    
data IFunction
	= IFunction FuncName IArguments [IBlock]
	deriving (Show, Eq)
    
data IArguments
	= IArguments [Var]
	deriving (Show, Eq)

data IBlock
	= IBlock BlockNum [IInstruction]
	deriving (Show, Eq)
    
data IInstruction
	= Ilc Reg Int
	| Ild Reg Var
	| Ist Var Reg
	| Iop Op Reg Reg Reg
	| Ibr Reg BlockNum BlockNum
	| Iret Reg
	| Icall Reg FuncName [Reg]
	deriving (Show, Eq)
  
data Op
  = Add
  | Sub
  | Mul
  | Div
  | LessThan
  | GreaterThan
  | DEq
  deriving (Show, Eq)

data Token
  = TokenLc
  | TokenLd
  | TokenSt
  | TokenOp Op
  | TokenBr
  | TokenRet
  | TokenCall
  | TokenReg Int
  | TokenId String
  | TokenNum Int
  | TokenOB
  | TokenCB
  deriving Show
  
-- For printing
showIProg :: IProgram -> String
showIProg (IProgram funcs) = "(" ++ (intercalate "\n" (map showIFunc funcs)) ++ "  )\n"

showIFunc :: IFunction -> String
showIFunc (IFunction id (IArguments args) iblocks) = "( " ++ id ++ " (" ++ (intercalate " " args) ++ ")\n" ++ (intercalate "\n" (map showBlock iblocks)) ++ "  )"

showBlock :: IBlock -> String
showBlock (IBlock bnum [])    = "  (" ++ (show bnum) ++ "  )"
showBlock (IBlock bnum insts) = "  (" ++ (show bnum) ++ "  " ++ (intercalate "\n      " (map showInst insts)) ++ "  )"

showInst :: IInstruction -> String
showInst inst = case inst of
    Ilc reg const    -> "(lc r" ++ (show reg) ++ " " ++ (show const) ++ ")"
    Ild reg var      -> "(ld r" ++ (show reg) ++ " " ++ var ++ ")"
    Ist var reg      -> "(st " ++ var ++ " r" ++ (show reg) ++ ")"
    Iop op r1 r2 r3  -> "(" ++ (toIop op) ++ " r" ++ (show r1) ++ " r" ++ (show r2) ++ " r" ++ (show r3) ++ ")"
    Ibr cond b1 b2   -> "(br r" ++ (show cond) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ")"
    Iret reg         -> "(ret r" ++ (show reg) ++ ")"
    Icall reg f args -> "(call r" ++ (show reg) ++ " " ++ f ++ " r" ++ (intercalate " r" (map show args)) ++ ")"  
    
toIop :: Op -> String
toIop op = case op of
    Add         -> "add"
    Sub         -> "sub"
    Mul         -> "mul"
    Div         -> "div"
    LessThan    -> "lt"
    GreaterThan -> "gt"
    DEq         -> "cmp"


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('-':cs) = lexMinus ('-':cs)
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (_:cs) = lexer cs

lexMinus ('-':cs) =
   case span isDigit cs of
      (num,rest) -> TokenNum (read ('-':num)) : lexer rest -- Negative numbers

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span (\x -> (isDigit x) || (isAlpha x)) cs of
      ("lc",rest)   -> TokenLc : lexer rest
      ("ld",rest)   -> TokenLd : lexer rest
      ("st",rest)   -> TokenSt : lexer rest
      ("add",rest)  -> TokenOp Add : lexer rest
      ("sub",rest)  -> TokenOp Sub : lexer rest
      ("mul",rest)  -> TokenOp Mul : lexer rest
      ("div",rest)  -> TokenOp Div : lexer rest
      ("lt",rest)   -> TokenOp LessThan : lexer rest
      ("gt",rest)   -> TokenOp GreaterThan : lexer rest
      ("cmp",rest)  -> TokenOp DEq : lexer rest
      ("br",rest)   -> TokenBr : lexer rest
      ("ret",rest)  -> TokenRet : lexer rest
      ("call",rest) -> TokenCall : lexer rest
      (var,rest)    -> lexReg var rest
      
lexReg cs csrest =
   case span isAlpha cs of
      ("r",rest)   -> if x1 == y1 then TokenReg (read x1) : lexer csrest
                      else TokenId cs : lexer csrest
                      where (x1,x2) = span isDigit rest
                            (y1,y2) = span (\x -> (isDigit x) || (isAlpha x)) rest
      (_,rest)     -> TokenId cs : lexer csrest
      
parseFile :: FilePath -> IO IProgram -- Once an IO, always an IO
parseFile f = do
    contents <- readFile f
    return (calc (lexer contents))
}
