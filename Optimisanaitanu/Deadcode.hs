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
                                   
deadcode_funcs :: [IFunction] -> [IFunction]
deadcode_funcs funcs = filtered_funcs
                     where (_, filtered_funcs) = deadcode_func (get_main funcs) funcs

deadcode_func :: IFunction -> [IFunction] -> (IFunction, [IFunction])
deadcode_func func all_funcs = (IFunction func_name args filtered_blocks, all_funcs)
                             where (IFunction func_name args blocks) = func
                                   (filtered_blocks, _, _) = deadcode_blocks blocks func all_funcs
                             
deadcode_blocks :: [IBlock] -> IFunction -> [IFunction] -> ([IBlock], IFunction, [IFunction])
deadcode_blocks blocks func all_funcs = de all_funcs


