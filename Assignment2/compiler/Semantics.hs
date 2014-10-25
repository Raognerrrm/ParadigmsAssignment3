module Semantics where
import Parser

type ErrorMessage = String

--Returns true if there is a function called 'main'
has_main :: [Function] -> Bool
has_main f = elem  "main" (get_funcs f)



--Returns the variable if it's not in the list of variables
var_in_id :: [Var] -> Var -> ErrorMessage
var_in_id (a:rest) b =
	if (a==b) then ""
	else var_in_id rest b
var_in_id _ a = a

--Checks that all the variables in an argument list are defined
var_in_args :: [Var] -> [Var] -> ErrorMessage
var_in_args vars (a:rest) = (var_in_id vars a) ++ (var_in_args vars rest)
var_in_args _ _  = ""

--Checks that all the variables in an expression are defined
var_in_exp :: [Var] -> Expression -> ErrorMessage
var_in_exp vars (ExpNum _) = ""
var_in_exp vars (ExpIdArg a (Arguments args)) = (var_in_args vars args)
var_in_exp vars (ExpId a) = var_in_id vars a
var_in_exp vars (ExpOp e _ f) = (var_in_exp vars e) ++ (var_in_exp vars f)

--Checks that all the variables in a statement are defined
var_in_stat :: [Var] -> Statement -> ErrorMessage
var_in_stat vars (StmtAssign a ex) = (var_in_id vars a) ++ (var_in_exp vars ex) 
var_in_stat vars (StmtIfThen a (Block b)) = (var_in_id vars a) ++ (var_in_block vars b)
var_in_stat vars (StmtIfThenElse a (Block b) (Block c)) = (var_in_id vars a) ++ (var_in_block vars b) ++ (var_in_block vars c)
var_in_stat vars (StmtReturn a) = var_in_id vars a

--Checks that all the variables in a block are defined
var_in_block :: [Var] -> [Statement] -> ErrorMessage
var_in_block vars (b:rest) = var_in_stat vars b ++ var_in_block vars rest
var_in_block _ _ = ""

--Checks that all the variables in a function are defined
has_defined_var :: Function -> ErrorMessage
has_defined_var (Function _ (Arguments a) (Variables v) (Block b)) = var_in_block (a++v) b

--Checks that all the variables in a function list are defined
defined_vars :: [Function] -> ErrorMessage
defined_vars (f:rest) = (has_defined_var f) ++ (defined_vars rest)
defined_vars _ = ""




--Checks that all the function calls in an expression are defined
func_in_exp :: [FuncName] -> Expression -> ErrorMessage
func_in_exp vars (ExpNum _) = ""
func_in_exp vars (ExpIdArg a (Arguments args)) = var_in_id vars a
func_in_exp vars (ExpId a) = ""
func_in_exp vars (ExpOp e _ f) = (func_in_exp vars e) ++ (func_in_exp vars f)

--Checks that all the function calls in a statement are defined
func_in_stat :: [FuncName] -> Statement -> ErrorMessage
func_in_stat vars (StmtAssign a ex) = (func_in_exp vars ex) 
func_in_stat vars (StmtIfThen a (Block b)) = (func_in_block vars b)
func_in_stat vars (StmtIfThenElse a (Block b) (Block c)) =  (func_in_block vars b) ++ (func_in_block vars c)
func_in_stat vars (StmtReturn a) =  ""

--Checks that all the function calls in a block are defined
func_in_block :: [FuncName] -> [Statement] -> ErrorMessage
func_in_block vars (b:rest) = func_in_stat vars b ++ func_in_block vars rest
func_in_block _ _ = ""

--Checks that all the function calls in a function are defined
has_defined_func :: [FuncName] -> Function -> ErrorMessage
has_defined_func vars (Function _ _ _ (Block b)) = func_in_block vars b

--Checks that all the function calls in a function list are defined
defined_funcs ::[FuncName] -> [Function] -> ErrorMessage
defined_funcs vars (f:rest) = (has_defined_func vars f) ++ (defined_funcs vars rest)
defined_funcs _ _= ""

--Returns the names of all the functions
get_funcs :: [Function] -> [FuncName]
get_funcs ((Function f _ _ _):rest) = [f] ++ (get_funcs rest)
get_funcs _ = [""]

--Checks that all the function calls in a program are defined
check_funcundef :: [Function] -> ErrorMessage
check_funcundef f = defined_funcs (get_funcs f) f



--Returns a name if it's in the list more than once
doubled_names :: [FuncName] -> ErrorMessage
doubled_names (f:rest) = if (elem f rest) then f
	else doubled_names rest
doubled_names _ = ""

check_fundouble :: [Function] -> ErrorMessage
check_fundouble f = doubled_names (get_funcs f)

--Checking duplicate variable names
check_vardouble :: [Function] -> ErrorMessage
check_vardouble ((Function _ (Arguments args) (Variables vars) _ ):rest) =  (doubled_names (args++vars)) ++ (check_vardouble rest)
check_vardouble _ = "" 

--Checks that a function call has the correct number of arguments
funcnum_in_args :: [(FuncName, Int)] -> FuncName -> Int -> ErrorMessage
funcnum_in_args ((f,a):rest) fun args = 
	if (f==fun && a/=args) then
		"'"++f++"' expects "++(show a)
	else funcnum_in_args rest fun args
funcnum_in_args _ _ _ = ""

--Checks that all the function calls in the expression have the correct number of arguments
funcnum_in_exp :: [(FuncName,Int)] -> Expression -> ErrorMessage
funcnum_in_exp vars (ExpNum _) = ""
funcnum_in_exp vars (ExpIdArg a (Arguments args)) = funcnum_in_args vars a (length args)
funcnum_in_exp vars (ExpId a) = ""
funcnum_in_exp vars (ExpOp e _ f) = (funcnum_in_exp vars e) ++ (funcnum_in_exp vars f)

--Checks that all the function calls in the statement have the correct number of arguments
funcnum_in_stat :: [(FuncName,Int)] -> Statement -> ErrorMessage
funcnum_in_stat vars (StmtAssign a ex) = (funcnum_in_exp vars ex) 
funcnum_in_stat vars (StmtIfThen a (Block b)) = (funcnum_in_block vars b)
funcnum_in_stat vars (StmtIfThenElse a (Block b) (Block c)) = (funcnum_in_block vars b) ++ (funcnum_in_block vars c)
funcnum_in_stat _ _ = ""

--Checks that all the function calls in the block have the correct number of arguments
funcnum_in_block :: [(FuncName,Int)] -> [Statement] -> ErrorMessage
funcnum_in_block vars (b:rest) = (funcnum_in_stat vars b) ++ (funcnum_in_block vars rest)
funcnum_in_block _ _ = ""

--Returns the number of arguments required for each function
get_funcnums :: [Function] -> [(FuncName,Int)]
get_funcnums ((Function f (Arguments args) _ _ ):rest) = [(f,length args)] ++ (get_funcnums rest)
get_funcnums _ = []

--Checks that all the function calls in the function have the correct number of arguments
func_varnum :: [Function] -> [(FuncName, Int)] -> ErrorMessage
func_varnum ((Function _ _ _ (Block b)):rest) t = (funcnum_in_block t b) ++ (func_varnum rest t)
func_varnum _ _ = ""

--Checks that all the function calls in the list of functions have the correct number of arguments
check_varnum :: [Function] -> ErrorMessage
check_varnum f = func_varnum f (get_funcnums f)

--Calls all the error checking functions
err_check (Program func_list)
 = if has_main func_list then
 		--let err_var = defined_vars func_list
 		--This isn't happy, and I can't work out what is wrong. Kevin help

 		if (defined_vars func_list) == "" then
 			if (check_funcundef func_list) == "" then 
 				if (check_fundouble func_list) == "" then 
 					if (check_vardouble func_list) == "" then 
 						if (check_varnum func_list) == "" then ""
 						else "Error: function "++(check_varnum func_list)++" variables."
 					else "Error: variable '"++(check_vardouble func_list)++"' redefined."
 				else "Error: function '"++(check_fundouble func_list)++"' redefined."
 			else "Error: function '"++(check_funcundef func_list)++"' undefined."
 		else "Error: variable '"++(defined_vars func_list)++"' undefined."
   else "Error: No main function defined."

parse_check :: FilePath -> IO Program
parse_check f = do
    prog <- parseFile f
    let err_msg = err_check prog
    
    if err_msg == "" then return prog
    else error err_msg

main = do
    prog <- parse_check "../Task 1 unit tests/factorial.code"
    print prog
