module FlowGraph where
import IParser

type Node = Int

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

reverseGraph :: [(Node,[Node])] -> [(Node,[Node])]
reverseGraph _ = []

flowFile :: FilePath -> IO [[(Node,[Node])]]
flowFile file = do
   parsed_prog <- parseFile file
   return (getBlocksProg parsed_prog)
   
printFlowFile file = do
   removed <- flowFile file
   putStr ( show removed++"\n")
