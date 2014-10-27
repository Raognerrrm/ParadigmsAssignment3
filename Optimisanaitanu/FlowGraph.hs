module FlowGraph where
import IParser

type Node = Int

--Random helper functions
isin :: Node -> [Node] -> Bool
isin n (a:rest) = if (n==a) then True
  else isin n rest
isin _ _ = False


--Get Block instructions
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
flowFile :: FilePath -> IO [[(Node,[Node])]]
flowFile file = do
   parsed_prog <- parseFile file
   return (reverseGraph (getBlocksProg parsed_prog))
   
printFlowFile file = do
   removed <- flowFile file
   putStr ( show removed++"\n")
