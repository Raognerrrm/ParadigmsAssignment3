module Deadcode where
import IParser
import FlowGraph
import Data.List

type BlockFlowGraph = [(BlockNum,[BlockNum])]
type UsedRegMap = [(BlockNum, [RegNum])]
type RegNum = Int

-- We assume the flow graph is acyclic, because otherwise this problem is hard since
-- we can't perform a topological sort on the blocks

-- Helper function - returns block n from a list of blocks
get_block :: BlockNum -> [IBlock] -> IBlock
get_block _ [] = error "Error: block not found"
get_block n (x:rest) = if n == num then x
                       else get_block n rest
                       where IBlock num insts = x
  
-- Returns true if the node has no in vertices in the reverse flow graph
no_in_vertices :: (BlockNum, [BlockNum]) -> Bool
no_in_vertices (a,b) = (b == []) 

--- Remove a node from the flowgraph, deleting its node and associated edges
remove_node :: Node -> BlockFlowGraph -> BlockFlowGraph
remove_node node [] = []
remove_node node ((n,adj):rest) = if node == n then remove_node node rest
                                  else ((n,(delete node adj)):(remove_node node rest))

-- Returns the next block in the topological sort                                  
top_sort_next :: BlockFlowGraph -> BlockNum
top_sort_next [] = error "Error: empty or cyclic flow graph"
top_sort_next ((n,adj):rest) = if adj == [] then n
                               else top_sort_next rest

-- Return a list of blocks in topological sort order by no out vertices (assumes acyclic graph)
top_sort :: BlockFlowGraph -> [BlockNum]
top_sort [] = []
top_sort graph = (node:(top_sort (remove_node node graph)))
               where node = top_sort_next graph
               
deadcode_prog :: IProgram -> IProgram
deadcode_prog (IProgram funcs) = (IProgram (map deadcode_func funcs))

deadcode_func :: IFunction -> IFunction
deadcode_func (IFunction name args blocks) = (IFunction name args (deadcode_blocks blocks))

-- Run top_sort on the forward flow graph to get the order of blocks to travel
-- when doing the backwards flow analysis
deadcode_blocks :: [IBlock] -> [IBlock]
deadcode_blocks blocks = deadcode_b block_order [] forward_graph backward_graph blocks
                       where forward_graph = getBlocksBlocks blocks
                             block_order = top_sort forward_graph
                             backward_graph = reverseGraphSingle forward_graph forward_graph
                             
deadcode_b :: [BlockNum] -> UsedRegMap -> BlockFlowGraph -> BlockFlowGraph -> [IBlock] -> [IBlock]
deadcode_b _ _ _ _ [] = []
deadcode_b nums used_regs fwd bwd (block:rest) = (a:b)
                                               where a = (deadcode_block nums used_regs fwd bwd block)
                                                     b = (deadcode_b nums used_regs fwd bwd rest)
                                                     
s_lookup :: Eq a => a -> [(a,b)] -> b
s_lookup x [] = error "Error: lookup failed"
s_lookup x ((y,z):rest) = if x == y then z
                          else s_lookup x rest
                             
get_used_regs :: BlockNum -> UsedRegMap -> BlockFlowGraph -> [RegNum]
get_used_regs bnum used_regs fwd_graph = foldl union [] (map (\x -> s_lookup x used_regs) adj)
                                       where adj = s_lookup bnum fwd_graph
                                
deadcode_block :: [BlockNum] -> UsedRegMap -> BlockFlowGraph -> BlockFlowGraph -> IBlock -> IBlock
deadcode_block (b:rest) used_regs fwd bwd (IBlock n insts) = (IBlock n (deadcode_insts rev ur))
                                                 where ur = get_used_regs b used_regs fwd
                                                       rev = reverse insts

-- Clean up instructions within a given block                                                 
deadcode_insts :: [IInstruction] -> [RegNum] -> [IInstruction]
deadcode_insts [] _ = []
deadcode_insts (inst:rest) used_regs = case inst of
   Ilc r _        -> if elem r used_regs then (inst:(deadcode_insts rest (delete r used_regs)))
                     else deadcode_insts rest used_regs
   Ild r _        -> if elem r used_regs then (inst:(deadcode_insts rest (delete r used_regs)))
                     else deadcode_insts rest used_regs
   Ist _ r        -> (inst:(deadcode_insts rest (union [r] used_regs)))
   Iop _ r1 r2 r3 -> if elem r1 used_regs then (inst:(deadcode_insts rest (union [r2, r3] (delete r1 used_regs))))
                     else deadcode_insts rest used_regs
   Ibr r _ _      -> (inst:(deadcode_insts rest (union [r] used_regs)))
   Iret r         -> (inst:(deadcode_insts rest (union [r] used_regs)))
   Icall r _ _    -> if elem r used_regs then (inst:(deadcode_insts rest (delete r used_regs)))
                     else deadcode_insts rest used_regs
                     
deadcode_file :: FilePath -> IO IProgram
deadcode_file file = do
   parsed_prog <- parseFile file
   return (deadcode_prog parsed_prog)
   
main = do
   dc <- deadcode_file "deadcode.intermediate"
   putStr (showIProg dc ++ "\n")

-- Backward flow analysis: Update list of used register numbers as you go through the reverse flow graph
-- If a block contains a branch (i.e. more than one block points to it in the reverse flow graph), then
-- union the used registers