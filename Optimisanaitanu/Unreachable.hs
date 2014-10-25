module Unreachable where
import IParser
import Data.List

type Reg      = Int
type BlockNum = Int
type FuncName = String
type Val      = String
type Node     = Int
  
-- intercalate joins a list of strings with a separator (courtesy of StackOverflow)  
showIProg :: IProgram -> String
showIProg (IProgram funcs) = "(" ++ (intercalate "\n" (map showIFunc funcs)) ++ "  )\n"

--Turns a IFunction into a readable string
showIFunc :: IFunction -> String
showIFunc (IFunction id (IArguments args) iblocks) = "( " ++ id ++ " (" ++ (intercalate " " args) ++ ")\n" ++ (intercalate "\n" (map showBlock iblocks)) ++ "  )"
    
--Turns a IBlock into a readable string
showBlock :: IBlock -> String
showBlock (IBlock bnum [])    = "  (" ++ (show bnum) ++ "  )"
showBlock (IBlock bnum insts) = "  (" ++ (show bnum) ++ "  " ++ (intercalate "\n      " (map showInst insts)) ++ "  )"
    
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

getBlocksInstruction :: IInstruction -> [Node]
getBlocksInstruction (Ibr r b1 b2) = [b1,b2]
getBlocksInstruction _ = []

getBlocksInstructions :: [IInstruction] -> [Node]
getBlocksInstructions (i:rest) = (getBlocksInstruction i) ++ (getBlocksInstructions rest)
getBlocksInstructions _ = []

getBlocksBlock :: IBlock -> [(Node,[Node])]
getBlocksBlock (IBlock num i) = [(num,(getBlocksInstructions i))]

getBlocksBlocks :: [IBlock] -> [(Node,[Node])]
getBlocksBlocks (b:rest) = (getBlocksBlock b) ++ (getBlocksBlocks rest)
getBlocksBlocks _ = []

--Returns a list of all possible blocks
getBlocksFunction :: IFunction -> [(Node,[Node])]
getBlocksFunction (IFunction name _ b) = getBlocksBlocks b

removeBlocks :: [Node] -> IFunction -> [Node]
removeBlocks n _ = n

getNew :: [Node] -> [Node] -> Node
getNew (a:rest) stuff = if (fst a) in stuff then getNew rest stuff
    else fst a
getNew _ _ = -1

getChildren :: [(Node,[Node])] -> Node -> [Node]
getChildren (a:rest) n = if (fst a) == n then last a
    else getChildren rest
getChildren _ _ = []

bfs :: [(Node,[Node])] -> Node -> [Node] -> [Node] -> [Node]
bfs _ -1 _ stuff = stuff
bfs graph n searched current = bfs graph (getNew (searched ++ [n]) (current ++ (getChildren graph n))) (searched ++ [n]) (current ++ (getChildren graph n))
bfs _ = []

getBlocks :: [IFunction] -> [[Node]]
getBlocks (f:rest) = [removeBlocks (bfs (getBlocksFunction f) 0 [0])  f]-- ++ (getBlocks rest)
getBlocks _ = []

removeUnreachable :: IProgram -> [[Node]]
removeUnreachable (IProgram funcs) = getBlocks funcs

-- Removes unreachable code  
unreachableFile :: FilePath -> IO [[Node]]
unreachableFile file = do
   parsed_prog <- parseFile file
   return (removeUnreachable parsed_prog)
   
printUnreachableFile file = do
   removed <- unreachableFile file
   putStr ((show removed)++"\n")