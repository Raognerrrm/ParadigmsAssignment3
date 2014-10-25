module Unreachable where
import IParser
import FlowGraph

type Reg      = Int
type BlockNum = Int
type FuncName = String
type Val      = String

getNew :: [Node] -> [Node] -> Node
getNew stuff (a:rest) = if (isin a stuff) then getNew stuff rest
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
  (searched ++ [n]) (removeDups (current ++ (getChildren graph n)))

removeBlocksBlocks :: [IBlock] -> [Node] -> [IBlock]
removeBlocksBlocks ((IBlock num i):rest) n = if (isin num n)
  then [(IBlock num i)] ++ (removeBlocksBlocks rest n)
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
   putStrLn ( showIProg removed++"\n")