module FlowGraph where
import IParser
import Data.List

type Node = Int
type BlockNum = Int
type FuncName = String
type Val      = String

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

--Turns an operatio into a readable string
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



--Random helper functions
isin :: Node -> [Node] -> Bool
isin n (a:rest) = if (n==a) then True
  else isin n rest
isin _ _ = False

getNew :: [Node] -> [Node] -> Node
getNew stuff (a:rest) = if (isin a stuff) then getNew stuff rest
    else a
getNew _ _ = -1

getChildren :: [(Node,[Node])] -> Node -> [Node]
getChildren (a:rest) n = if (fst a) == n then snd a
    else getChildren rest n
getChildren _ _ = []

removeDups :: [Node] -> [Node]
removeDups (a:rest) = if (isin a rest) then removeDups rest
  else [a] ++ (removeDups rest)
removeDups _ = []

bfs :: [(Node,[Node])] -> Node -> [Node] -> [Node] -> [Node]
bfs graph n searched current = if (n == -1) then current
  else bfs graph (getNew (searched ++ [n]) (removeDups (current ++ (getChildren graph n))))
  (searched ++ [n]) (removeDups (current ++ (getChildren graph n)))



--Get Block function
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

getBlocksFunction :: IFunction -> [(Node,[Node])]
getBlocksFunction (IFunction name _ b) = getBlocksBlocks b

getBlocksFuncs :: [IFunction] -> [[(Node,[Node])]]
getBlocksFuncs (a:rest) = [getBlocksFunction a] ++(getBlocksFuncs rest)
getBlocksFuncs _ = []

getBlocksProg :: IProgram -> [[(Node,[Node])]]
getBlocksProg (IProgram funcs) = getBlocksFuncs funcs

getCycles2 :: Node -> [(Node,[Node])] -> [Node]
getCycles2 n dat = if (isin n (bfs dat n [n] [])) then
                        [n]
                    else []

getCycles :: [(Node,[Node])] -> [(Node,[Node])] -> [Node]
getCycles (a:rest) b = (getCycles2 (fst a) b) ++ (getCycles rest b)
getCycles _ _ = []


getCycles1 :: [[(Node,[Node])]] -> [Node]
getCycles1 (a:rest) = getCycles a a


--Reverse graph functions
reverseNode2 :: Node -> [(Node,[Node])] -> [Node]
reverseNode2 n (a:rest) = if (isin n (snd a)) then [fst a] ++ (reverseNode2 n rest)
    else reverseNode2 n rest
reverseNode2 _ _ = []

reverseNode :: (Node,[Node]) -> [(Node,[Node])] -> (Node,[Node])
reverseNode n dat = (fst n,reverseNode2 (fst n) dat)

reverseGraphSingle :: [(Node,[Node])] -> [(Node,[Node])] -> [(Node,[Node])]
reverseGraphSingle (a:rest) dat = [reverseNode a dat] ++ (reverseGraphSingle rest dat)
reverseGraphSingle _ _ = []

reverseGraph :: [[(Node,[Node])]] -> [[(Node,[Node])]]
reverseGraph (a:rest) = [reverseGraphSingle a a] ++ (reverseGraph rest)
reverseGraph _ = []

--Code to run the thing
flowFile :: FilePath -> IO [Node]
flowFile file = do
   parsed_prog <- parseFile file
   return (getCycles1(getBlocksProg parsed_prog))
   
printFlowFile file = do
   removed <- flowFile file
   putStr ( show removed++"\n")
