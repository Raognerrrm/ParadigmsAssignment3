module Unreachable where
import IParser
import FlowGraph




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