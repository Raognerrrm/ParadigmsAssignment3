module Interpreter where
import IParser
import Data.Map
import Data.Maybe

type VarEnv = Map Var Int
type RegEnv = Map Reg Int

-- Tests if a function has the function name "main"
is_main :: IFunction -> Bool
is_main (IFunction f _ _) = (f == "main")

-- Gets the main function from the program, if present. Otherwise it raises an error.
get_main :: [IFunction] -> IFunction
get_main [] = error "Error: No main function defined."
get_main (f:rest) = if is_main f then f
                    else get_main rest
                    
interpretProgram :: IProgram -> [Int] -> Int
interpretProgram (IProgram funcs) args = interpretFunction (get_main funcs) args funcs

-- Check if a block is numbered 0
is_block_zero :: IBlock -> Bool
is_block_zero (IBlock bnum _) = (bnum == 0)

-- Takes a list of blocks and returns block 0, if preent. OTherwise it raises an error.
get_start_block :: [IBlock] -> IBlock
get_start_block [] = error "Error: No block 0 found."
get_start_block (b:rest) = if is_block_zero b then b
                           else get_start_block rest
                           
-- Returns the instruction list of a block
get_insts :: IBlock -> [IInstruction]
get_insts (IBlock _ insts) = insts
                           

interpretFunction :: IFunction -> [Int] -> [IFunction] -> Int
interpretFunction (IFunction f (IArguments f_args) blocks) args funcs = if length f_args /= length args
                                                                        then error ("Error: Incorrect number of arguments supplied to function " ++ f ++ ".")
                                                                        else interpretInst start_insts (fromList []) (fromList (zip f_args args)) blocks funcs
                                                                        where start_insts = get_insts (get_start_block blocks)

-- Returns the block number of a block
get_bnum :: IBlock -> Int
get_bnum (IBlock bnum _) = bnum

-- Returns the block with a given num
get_block :: Int -> [IBlock] -> IBlock
get_block num [] = error ("Error: No block number " ++ (show num) ++ " found.")
get_block num (b:rest) = if num == get_bnum b then b
                         else get_block num rest

-- Returns the name of a function
get_funcname :: IFunction -> FuncName
get_funcname (IFunction f _ _) = f

-- Returns the function with a given name
get_func :: FuncName -> [IFunction] -> IFunction
get_func name [] = error ("Error: No function " ++ name ++ " found.")
get_func name (f:rest) = if name == get_funcname f then f
                         else get_func name rest
                                                                         
bool_to_int :: Bool -> Int
bool_to_int True = 1
bool_to_int False = 0

renv_lookup :: Reg -> RegEnv -> Int
renv_lookup reg renv = if value == Nothing then error ("Error: Unassigned register " ++ (show reg))
                       else fromJust value
                       where value = Data.Map.lookup reg renv
                       
venv_lookup :: Var -> VarEnv -> Int
venv_lookup var venv = if value == Nothing then error ("Error: Unassigned variable " ++ var)
                       else fromJust value
                       where value = Data.Map.lookup var venv
                                                                         
-- Interprets an instruction given
-- A list of instructions - the instruction to execute being the first
-- A register and variable environment
-- All blocks within the current function
-- All functions in the program
interpretInst :: [IInstruction] -> RegEnv -> VarEnv -> [IBlock] -> [IFunction] -> Int
interpretInst [] _ _ _ _ = error "Error: Instructions exhausted (possibly due to a non-returning function)"
interpretInst (inst:rest) renv venv blocks funcs = case inst of
    Ilc reg c             -> interpretInst rest (insert reg c renv) venv blocks funcs
    Ild reg var           -> interpretInst rest (insert reg (venv_lookup var venv) renv) venv blocks funcs
    Ist var reg           -> interpretInst rest renv (insert var (renv_lookup reg renv) venv) blocks funcs
    Iop op rega regb regc -> case op of
                    Add         -> interpretInst rest (insert rega ((renv_lookup regb renv)+(renv_lookup regc renv)) renv) venv blocks funcs
                    Sub         -> interpretInst rest (insert rega ((renv_lookup regb renv)-(renv_lookup regc renv)) renv) venv blocks funcs
                    Mul         -> interpretInst rest (insert rega ((renv_lookup regb renv)*(renv_lookup regc renv)) renv) venv blocks funcs
                    Div         -> if num2 == 0 then error "Error: Division by zero"
                                   else interpretInst rest (insert rega (quot num1 num2) renv) venv blocks funcs
                                   where num1 = (renv_lookup regb renv)
                                         num2 = (renv_lookup regc renv)
                    LessThan    -> interpretInst rest (insert rega (bool_to_int ((renv_lookup regb renv) < (renv_lookup regc renv))) renv) venv blocks funcs
                    GreaterThan -> interpretInst rest (insert rega (bool_to_int ((renv_lookup regb renv) > (renv_lookup regc renv))) renv) venv blocks funcs
                    DEq         -> interpretInst rest (insert rega (bool_to_int ((renv_lookup regb renv) == (renv_lookup regc renv))) renv) venv blocks funcs
    Ibr reg b1 b2         -> if renv_lookup reg renv /= 0
                             then interpretInst ((get_insts (get_block b1 blocks)) ++ rest) renv venv blocks funcs
                             else interpretInst ((get_insts (get_block b2 blocks)) ++ rest) renv venv blocks funcs
    Iret reg              -> renv_lookup reg renv
    Icall reg f regs      -> interpretInst rest (insert reg (interpretFunction (get_func f funcs) (Prelude.map (\x -> renv_lookup x renv) regs) funcs) renv) venv blocks funcs

-- Interprets a program given its file path and a list of arguments    
interpretFile :: FilePath -> [Int] -> IO Int
interpretFile file args = do
   parsed_prog <- parseFile file
   return (interpretProgram parsed_prog args)
   
printInterpreted file args = do
   interpreted <- interpretFile file args
   putStr ((show interpreted)++"\n")
