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

getNew :: [Node] -> [Node] -> Node
getNew (a:rest) stuff = if (isin a stuff) then getNew rest stuff
    else a
getNew _ _ = -1

getChildren :: [(Node,[Node])] -> Node -> [Node]
getChildren (a:rest) n = if (fst a) == n then snd a
    else getChildren rest n
getChildren _ _ = []

isin :: Node -> [Node] -> Bool
isin n (a:rest) = if (n==a) then True
  else isin n rest
isin _ _ = False

removeDups :: [Node] -> [Node]
removeDups (a:rest) = if (isin a rest) then removeDups rest
  else [a] ++ (removeDups rest)
removeDups _ = []

bfs :: [(Node,[Node])] -> Node -> [Node] -> [Node] -> [Node]
bfs graph n searched current = if (n == -1) then current
  else bfs graph (getNew (searched ++ [n]) (removeDups (current ++ (getChildren graph n))))
  (searched ++ [n]) (removeDups(current ++ (getChildren graph n)))

removeBlocksBlocks :: [IBlock] -> [Node] -> [IBlock]
removeBlocksBlocks ((IBlock num i):rest) n = if (isin num n) then [(IBlock num i)] ++ (removeBlocksBlocks rest n)
  else removeBlocksBlocks rest n
removeBlocksBlocks _ _ = []

removeBlocks :: [Node] -> IFunction -> IFunction
removeBlocks n (IFunction name args blocks) = (IFunction name args (removeBlocksBlocks blocks n))

getBlocks :: [IFunction] -> [IFunction]
getBlocks (f:rest) = [removeBlocks (bfs (getBlocksFunction f) 0 [] [0]) f] ++ (getBlocks rest)
getBlocks _ = []

removeUnreachable :: IProgram -> IProgram
removeUnreachable (IProgram funcs) = (IProgram (getBlocks funcs))

-- Removes unreachable code  
unreachableFile :: FilePath -> IO IProgram
unreachableFile file = do
   parsed_prog <- parseFile file
   return (removeUnreachable parsed_prog)
   
printUnreachableFile file = do
   removed <- unreachableFile file
   putStrLn ((showIProg removed)++"\n")