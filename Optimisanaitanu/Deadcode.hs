module Deadcode where
import IParser

is_main :: IFunction -> Bool
is_main (IFunction func_name _ _) = if func_name == "main" then True
                                  else False
                                  
get_main :: [IFunction] -> IFunction
get_main [] = error "Error: No main function found."
get_main (f:rest) = if is_main f then f
                    else get_main rest
                    
is_block_zero :: IBlock -> Bool
is_block_zero (IBlock bnum _) = if bnum == 0 then True
                              else False
                    
get_block_zero :: [IBlock] -> IBlock
get_block_zero [] = error "Error: No block zero found."
get_block_zero (b:rest) = if is_block_zero b then b
                          else get_block_zero rest
                    
-- Perform a postoder traversal starting from the first instruction of the main function
-- deadcode_x converts x into a filtered version of x with dead code removed
deadcode_prog :: IProgram -> IProgram
deadcode_prog (IProgram funcs) = IProgram (deadcode_funcs funcs)

-----------------------------------

deadcode_inst :: [IInstruction] -> [IBlock] -> IFunction -> [IFunction] -> ([Reg], [IInstruction], [IBlock], IFunction, [IFunction])
deadcode_inst [] blocks func all_funcs = ([], [], blocks, func, all_funcs) 
deadcode_inst (inst:rest) blocks func all_funcs = case inst of
  -- lc/ld: Check if variable will be used, if so removed from used_regs, else delete instruction
  Ilc regnum _ = if elem regnum used_regs
                 then (delete regnum used_regs, [inst] ++ insts2, blocks2, func2, all_funcs2)
                 else (used_regs, insts2, blocks2, func2, all_funcs2)
                     
  Ild regnum _ = if elem regnum used_regs
                 then (delete regnum used_regs, [inst] ++ insts2, blocks2, func2, all_funcs2)
                 else (used_regs, insts2, blocks2, func2, all_funcs2)
                 
  Iop _ reg1 reg2 reg3 = if elem reg1 used_regs
                         then ([reg2, reg3] ++ (delete reg1 used_regs), [inst] ++ insts2, blocks2, func2, all_funcs2)
                         else (used_regs, insts2, blocks2, func2, all_funcs2)
  
  Ibr regnum _ _ = (union [regnum] used_regs, [inst] ++ insts2, blocks2, func2, all_funcs2)
  
  Ist _ regnum = (union [regnum] used_regs, [inst] ++ insts2, blocks2, func2, all_funcs2)
  
  Iret regnum = (union [regnum] used_regs, [inst] ++ insts2, blocks2, func2, all_funcs2)

  -- call: Check if output register will be used, if so remove it then union with function arg register list
  Icall regnum _ reglist = if elem regnum used_regs
                           then (union reglist (delete regnum used_regs), [inst] ++ insts2, blocks2, func2, all_funcs2)
                           else (used_regs, insts2, blocks2, func2, all_funcs2)
  
  where (used_regs, insts2, blocks2, func2, all_funcs2) = deadcode rest block all_funcs
    