module Redundant where
import IParser
import FlowGraph


--The kill part of gen/kill
removeGenKill :: Reg -> [(Reg,Val)] -> [(Reg,Val)]
removeGenKill r (a:rest) = if (r == (fst a)) then removeGenKill r rest
                           else [a] ++ (removeGenKill r rest)
removeGenKill _ dat = dat

--Adds a reg val pair to the gen/kill list, or removes it if the reg has been modified
getGenKillInstruction :: IInstruction -> [(Reg,Val)] -> [(Reg,Val)]
getGenKillInstruction (Ild r var) dat = dat ++ [(r,var)]
getGenKillInstruction (Ilc r _) dat = removeGenKill r dat
getGenKillInstruction (Ist _ r) dat = removeGenKill r dat
getGenKillInstruction (Iop _ r _ _) dat = removeGenKill r dat
getGenKillInstruction (Icall r _ _) dat = removeGenKill r dat
getGenKillInstruction _ dat = dat

--Gets the gen/kill set for a block
getGenKill :: [IInstruction] -> [(Reg,Val)] -> [(Reg,Val)]
getGenKill (a:rest) dat = getGenKill rest (getGenKillInstruction a dat)
getGenKill _ dat = dat

--Gets the gen/kill graphs for a function
getGenKills :: [IBlock] -> [(Node,[(Reg,Val)])]
getGenKills ((IBlock num i):rest) = [(num,getGenKill i [])] ++ (getGenKills rest)
getGenKills _ = []

--Checks to see if a gen is in two sets of gen/kills
genIsIn :: (Reg,Val) -> [(Reg,Val)] -> Bool
genIsIn b (a:rest) = if ((fst a) == (fst b)) then
                        if ((snd a) == (snd b)) then
                            True
                        else genIsIn b rest
                     else genIsIn b rest
genIsIn _ _ = False

--Merges two gen/kill graphs. Because if a register does not have the same value for all nodes that it can come from, that register should be discarded
mergeGen1 :: [(Reg,Val)] -> [(Reg,Val)] -> [(Reg,Val)]
--mergeGen1 a b = a++b
mergeGen1 (a:rest) dat = if (genIsIn a dat) then [a] ++ (mergeGen1 rest dat)
                        else mergeGen1 rest dat
mergeGen1 _ _ = [] 

--Merges two gen/kill graphs. Because if a register does not have the same value for all nodes that it can come from, that register should be discarded
mergeGen :: [(Reg,Val)] -> [(Reg,Val)] -> [(Reg,Val)]
mergeGen a b = if ((length a) == 0) then b
                else if ((length b) == 0) then a 
                else (mergeGen1 a b) ++ (mergeGen1 b a)

--Gets the gen/kill for a block and all its parents
getBlockGenKill :: Node -> [(Node,[(Reg,Val)])] ->[(Node,[(Reg,Val)])] -> [(Node,[Node])] -> [Node] -> [Node] -> [(Reg,Val)]
getBlockGenKill num (a:rest) gen dat adds loops = if (isin (fst a) adds) then
                                                        (snd a) ++ (getBlockGenKill num rest gen dat adds loops)
                                                    else getBlockGenKill num rest gen dat adds loops
getBlockGenKill _ _ _ _ _ _ = []

getGenBlock :: Node -> [(Node,[(Reg,Val)])] -> [(Reg,Val)]
getGenBlock num (a:rest) = if ((fst a)==num) then snd a
                            else getGenBlock num rest
getGenBlock _ _ = []

--Gets the gen/kill for a block
getBlockGenKills :: Node -> [(Node,[(Reg,Val)])] -> [(Node,[Node])] -> [Node] -> [(Reg,Val)]
getBlockGenKills num gen (a:rest) loops = if (isin num loops) then  []
                                          else if (num == (fst a)) then
                                                    getBlockGenKill num gen gen ([a]++rest) (snd a) loops
                                                else getBlockGenKills num gen rest loops
getBlockGenKills _ _ _ _ = []

--Gets the earliest value of val
findVal :: Val -> [(Reg,Val)] -> Reg
findVal v (a:rest) = if (v == (snd a)) then fst a
                     else findVal v rest
findVal v _ = -1 --Something has gone wrong

--Replaces Reg with another register if it occurs first
replace :: Reg -> [(Reg,Val)] -> [(Reg,Val)] -> Reg
replace r (a:rest) dat = if (r == (fst a)) then findVal (snd a) dat
                     else replace r rest dat
replace r _ _ = r

--Replaces the registers in an instruction
replaceInstruction :: [(Reg,Val)] -> IInstruction -> IInstruction
replaceInstruction dat (Iop op r1 r2 r3) = (Iop op r1 (replace r2 dat dat) (replace r3 dat dat))
replaceInstruction dat (Ibr r1 b1 b2) = (Ibr (replace 1 dat dat) b1 b2)
replaceInstruction dat (Iret r1) =  (Iret (replace r1 dat dat))
replaceInstruction dat a = a

--Replaces the registers in a set of instructions
removeLoadsInstructions :: [IInstruction] -> Node -> [(Node,[Node])] -> [(Node,[(Reg,Val)])] -> [Node] -> [(Reg,Val)] -> [IInstruction]
removeLoadsInstructions (a:rest) bnum dat gen loops current = 
    [replaceInstruction (getGenKillInstruction a current) a] ++
                     (removeLoadsInstructions rest bnum dat gen loops (getGenKillInstruction a current)) 
removeLoadsInstructions _ _ _ _ _ _ = []

--Replaces the registers in a block
removeLoadsBlock :: IBlock -> [(Node,[Node])] -> [(Node,[(Reg,Val)])] -> [Node] ->IBlock
removeLoadsBlock (IBlock num i) dat gen loops = --(num,getBlockGenKills num gen dat loops)
         (IBlock num (removeLoadsInstructions i num dat gen loops (getBlockGenKills num gen dat loops)))


--Replaces the registers in a set of blocks
--                   Blocks    Control flow graph    Gen/kill for each block   Loopy    New blocks
removeLoadsBlocks :: [IBlock] -> [(Node,[Node])] -> [(Node,[(Node,Val)])] -> [Node] -> [IBlock]
removeLoadsBlocks (a:rest) dat gen loops = [removeLoadsBlock a dat gen loops] ++ (removeLoadsBlocks rest dat gen loops)
removeLoadsBlocks _ _ _ _ = []

--Replaces the registers in a function
removeLoads :: IFunction -> [(Node,[Node])] -> IFunction
removeLoads (IFunction name args blocks) a = (IFunction name args (removeLoadsBlocks blocks a (getGenKills blocks) (getCycles a a)))

--Replaces the registers in a set of functions
getLoads :: [IFunction] -> [[(Node,[Node])]] -> [IFunction]
getLoads (f:rest) (a:rest1) = [removeLoads f  a] ++ (getLoads rest rest1)
getLoads _ _ = []


--Replaces the registers in a program
removeRedundant :: IProgram -> IProgram
removeRedundant (IProgram funcs) = (IProgram (getLoads funcs (reverseGraph (getBlocksFuncs funcs))))

-- Removes unreachable code  
redundantFile :: FilePath ->IO IProgram
redundantFile file = do
   parsed_prog <- parseFile file
   return (removeRedundant parsed_prog)
   
printRedundantFile file = do
   removed <- redundantFile file
   putStrLn ( showIProg removed++"\n")
