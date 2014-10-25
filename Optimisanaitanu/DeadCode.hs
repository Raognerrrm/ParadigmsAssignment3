module DeadCode where
import IParser
import Data.List

type Reg      = Int
type BlockNum = Int
type FuncName = String
type Val      = String

data IProgram 
    = IProgram [IFunction]
    deriving Show
  
-- intercalate joins a list of strings with a separator (courtesy of StackOverflow)  
showIProg :: IProgram -> String
showIProg (IProgram funcs) = "(" ++ (intercalate "\n" (map showIFunc funcs)) ++ "  )\n"

data IFunction
    = IFunction FuncName IArguments [IBlock]
    deriving Show

--Turns a IFunction into a readable string
showIFunc :: IFunction -> String
showIFunc (IFunction id (IArguments args) iblocks) = "( " ++ id ++ " (" ++ (intercalate " " args) ++ ")\n" ++ (intercalate "\n" (map showBlock iblocks)) ++ "  )"

data IArguments
    = IArguments [Var]
    deriving Show

data IBlock
    = IBlock BlockNum [IInstruction]
    deriving Show
    
--Turns a IBlock into a readable string
showBlock :: IBlock -> String
showBlock (IBlock bnum [])    = "  (" ++ (show bnum) ++ "  )"
showBlock (IBlock bnum insts) = "  (" ++ (show bnum) ++ "  " ++ (intercalate "\n      " (map showInst insts)) ++ "  )"

data IInstruction
    = Ilc Reg Int
    | Ild Reg Var
    | Ist Var Reg
    | Iop Op Reg Reg Reg
    | Ibr Reg BlockNum BlockNum
    | Iret Reg
    | Icall Reg FuncName [Reg]
    deriving Show
    
--Turns an operation into a readable string
toIop :: Op -> String
toIop op = case op of
    Add         -> "add"
    Sub         -> "sub"
    Mul         -> "mul"
    Div         -> "div"
    LessThan    -> "lt"
    GreaterThan -> "gt"
    DEq         -> "cmp"

--Turns a IInstruction into a readable string
showInst :: IInstruction -> String
showInst inst = case inst of
    Ilc reg const    -> "(lc r" ++ (show reg) ++ " " ++ (show const) ++ ")"
    Ild reg var      -> "(ld r" ++ (show reg) ++ " " ++ var ++ ")"
    Ist var reg      -> "(st " ++ var ++ " r" ++ (show reg) ++ ")"
    Iop op r1 r2 r3  -> "(" ++ (toIop op) ++ " r" ++ (show r1) ++ " r" ++ (show r2) ++ " r" ++ (show r3) ++ ")"
    Ibr cond b1 b2   -> "(br r" ++ (show cond) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ")"
    Iret reg         -> "(ret r" ++ (show reg) ++ ")"
    Icall reg f args -> "(call r" ++ (show reg) ++ " " ++ f ++ " r" ++ (intercalate " r" (map show args)) ++ ")"

getBlocksInstruction :: IInstruction -> [Int]
getBlocksInstruction (Ibr r b1 b2) = [b1,b2]
getBlocksInstruction _ = []

getBlocksInstructions :: [IInstruction] -> [Int]
getBlocksInstructions (i:rest) = (getBlocksInstruction i) ++ (getBlocksInstructions rest)
getBlocksInstructions _ = []

getBlocksBlock :: IBlock -> [(Int,[Int])]
getBlocksBlock IBlock num i = [num,(getBlocksInstructions i)]

getBlocksBlocks :: [IBlock] -> [(Int,[Int])]
getBlocksBlocks (b:rest) = (getBlocksBlock b) ++ (getBlocksBlocks rest)
getBlocksBlocks _ = []

--Returns a list of all possible blocks
getBlocksFunction :: IFunction -> [(String,[Int])]
getBlocksFunction (IFunction name _ b) = getBlocksBlocks b

getBlocks :: [IFunction] -> [(String,[Int])]
getBlocks (f:rest) = (getBlocksFunction f) ++ (getBlocks rest)
getBlocks _ = []

removeDeadCode :: IProgram -> [(String,[Int])]
removeDeadCode (IProgram funcs) = getBlocks funcs