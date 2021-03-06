module FlowGraph where
import IParser
import Data.List

type Node = Int
type Val      = String

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
