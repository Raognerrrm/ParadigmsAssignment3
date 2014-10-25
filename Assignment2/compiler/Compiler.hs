module Compiler where
import Parser
import Semantics
import Data.List

type Reg      = Int
type BlockNum = Int

data IProgram 
	= IProgram [IFunction]
    deriving Show
  
-- intercalate joins a list of strings with a separator (courtesy of StackOverflow)  
showIProg :: IProgram -> String
showIProg (IProgram funcs) = "(" ++ (intercalate "\n" (map showIFunc funcs)) ++ "  )\n"

data IFunction
	= IFunction FuncName IArguments [IBlock]
	deriving Show

--Turns a IFunction into a readable string
showIFunc :: IFunction -> String
showIFunc (IFunction id (IArguments args) iblocks) = "( " ++ id ++ " (" ++ (intercalate " " args) ++ ")\n" ++ (intercalate "\n" (map showBlock iblocks)) ++ "  )"

data IArguments
	= IArguments [Var]
	deriving Show

data IBlock
	= IBlock BlockNum [IInstruction]
	deriving Show
    
--Turns a IBlock into a readable string
showBlock :: IBlock -> String
showBlock (IBlock bnum [])    = "  (" ++ (show bnum) ++ "  )"
showBlock (IBlock bnum insts) = "  (" ++ (show bnum) ++ "  " ++ (intercalate "\n      " (map showInst insts)) ++ "  )"

data IInstruction
	= Ilc Reg Int
	| Ild Reg Var
	| Ist Var Reg
	| Iop Op Reg Reg Reg
	| Ibr Reg BlockNum BlockNum
	| Iret Reg
	| Icall Reg FuncName [Reg]
	deriving Show
    
--Turns an operatio into a readable string
toIop :: Op -> String
toIop op = case op of
    Add         -> "add"
    Sub         -> "sub"
    Mul         -> "mul"
    Div         -> "div"
    LessThan    -> "lt"
    GreaterThan -> "gt"
    DEq         -> "cmp"

--Turns a IInstruction into a readable string
showInst :: IInstruction -> String
showInst inst = case inst of
    Ilc reg const    -> "(lc r" ++ (show reg) ++ " " ++ (show const) ++ ")"
    Ild reg var      -> "(ld r" ++ (show reg) ++ " " ++ var ++ ")"
    Ist var reg      -> "(st " ++ var ++ " r" ++ (show reg) ++ ")"
    Iop op r1 r2 r3  -> "(" ++ (toIop op) ++ " r" ++ (show r1) ++ " r" ++ (show r2) ++ " r" ++ (show r3) ++ ")"
    Ibr cond b1 b2   -> "(br r" ++ (show cond) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ")"
    Iret reg         -> "(ret r" ++ (show reg) ++ ")"
    Icall reg f args -> "(call r" ++ (show reg) ++ " " ++ f ++ " r" ++ (intercalate " r" (map show args)) ++ ")"  

-- Converts a list of variables to a list of ld statements giving a starting register num        
loadVars :: [Var] -> Reg -> [IInstruction]
loadVars [] _           = []
loadVars (var:rest) reg = (Ild reg var) : (loadVars rest (reg + 1))
                          
--Turns an expression into a list of instructions, and places the value in the supplied register            
iexp :: Expression -> Reg -> [IInstruction]
iexp expression reg = case expression of
        (ExpNum num)                  -> [Ilc reg num]
        (ExpId id)                    -> [Ild reg id]
        (ExpIdArg f (Arguments args)) -> (loadVars args (reg + 1)) ++ [Icall reg f [(reg+1) .. (reg + (length args))]]
        (ExpOp exp1 op exp2)          -> (instlist1 ++ instlist2) ++ [(Iop op reg (reg + 1) (reg + 2))]
                                          where instlist1 = iexp exp1 (reg + 1)
                                                instlist2 = iexp exp2 (reg + 2)
                                                
--Turns a statement int a list of instructions
istmt :: Statement -> BlockNum -> ([IInstruction], [IBlock], BlockNum)
istmt statement bnum = case statement of
        (StmtAssign var exp)       -> (instlist ++ [(Ist var 1)], [], bnum)
                                         where instlist = iexp exp 1
        (StmtReturn var)           -> ([Ild 1 var, Iret 1], [], bnum)
        (StmtIfThen var b1)        -> ([Ild 1 var, Ibr 1 (bnum + 1) (bnum1 + 1)], ib1 ++ [IBlock (bnum1 + 1) [Ilc 1 0]], bnum1 + 1) -- Dummy instruction for the other branch
                                         where (ib1, bnum1) = iblock b1 (bnum + 1) []
        (StmtIfThenElse var b1 b2) -> ([Ild 1 var, Ibr 1 (bnum + 1) (bnum1 + 1)], ib1 ++ ib2, bnum2)
                                         where (ib1, bnum1) = iblock b1 (bnum + 1) []
                                               (ib2, bnum2) = iblock b2 (bnum1 + 1) []

--Turns a list of statements into a list of instructions
istmts :: [Statement] -> BlockNum -> ([IInstruction], [IBlock], BlockNum)
istmts [] bnum          = ([], [], bnum)
istmts (stmt:rest) bnum = (insts1 ++ insts2, blocks1 ++ blocks2, bnum3)
                            where (insts1, blocks1, bnum2) = istmt stmt bnum
                                  (insts2, blocks2, bnum3) = istmts rest bnum2
                             
--Turns a block into a list of Iblocks, returning the last used block number
iblock :: Block -> BlockNum -> [Var] -> ([IBlock], BlockNum)
iblock (Block b) bnum vars = if bnum == 0 then ([IBlock bnum ([Ilc 1 0] ++ (map (\x -> Ist x 1) vars) ++ insts)] ++ blocks, bnum2) -- Initialise VARS variables to 0
                             else ([IBlock bnum insts] ++ blocks, bnum2)
                             where (insts, blocks, bnum2) = istmts b bnum

--Turns a function into an IFunction
ifunc :: Function -> IFunction
ifunc (Function f (Arguments args) (Variables vars) b) = (IFunction f (IArguments args) block)
                                                       where (block, bnum) = (iblock b 0 vars)

--Turns a program into an IProgram
iprog :: Program -> IProgram
iprog (Program funcs) = IProgram (map ifunc funcs)

compileFile :: FilePath -> IO String
compileFile f = do
    checked_prog <- parse_check f
    return (showIProg (iprog checked_prog))
    
printCompiled f = do
    compiled <- compileFile f
    putStr compiled
