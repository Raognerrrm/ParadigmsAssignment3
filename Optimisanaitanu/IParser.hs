{-# OPTIONS_GHC -w #-}
module IParser where
import Data.Char
import System.IO
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

action_0 (24) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (24) = happyShift action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_6
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 _ = happyReduce_2

action_3 (26) = happyAccept
action_3 _ = happyFail

action_4 (25) = happyShift action_9
action_4 _ = happyFail

action_5 (24) = happyShift action_6
action_5 (5) = happyGoto action_8
action_5 (6) = happyGoto action_5
action_5 _ = happyReduce_2

action_6 (20) = happyShift action_7
action_6 _ = happyFail

action_7 (24) = happyShift action_11
action_7 (7) = happyGoto action_10
action_7 _ = happyFail

action_8 _ = happyReduce_3

action_9 _ = happyReduce_1

action_10 (24) = happyShift action_16
action_10 (9) = happyGoto action_14
action_10 (10) = happyGoto action_15
action_10 _ = happyFail

action_11 (20) = happyShift action_13
action_11 (8) = happyGoto action_12
action_11 _ = happyReduce_6

action_12 (25) = happyShift action_21
action_12 _ = happyFail

action_13 (20) = happyShift action_13
action_13 (8) = happyGoto action_20
action_13 _ = happyReduce_6

action_14 (25) = happyShift action_19
action_14 _ = happyFail

action_15 (24) = happyShift action_16
action_15 (9) = happyGoto action_18
action_15 (10) = happyGoto action_15
action_15 _ = happyReduce_8

action_16 (21) = happyShift action_17
action_16 _ = happyFail

action_17 (24) = happyShift action_24
action_17 (11) = happyGoto action_22
action_17 (12) = happyGoto action_23
action_17 _ = happyFail

action_18 _ = happyReduce_9

action_19 _ = happyReduce_4

action_20 _ = happyReduce_7

action_21 _ = happyReduce_5

action_22 (25) = happyShift action_33
action_22 _ = happyFail

action_23 (24) = happyShift action_24
action_23 (11) = happyGoto action_32
action_23 (12) = happyGoto action_23
action_23 _ = happyReduce_11

action_24 (14) = happyShift action_25
action_24 (15) = happyShift action_26
action_24 (16) = happyShift action_27
action_24 (17) = happyShift action_28
action_24 (18) = happyShift action_29
action_24 (19) = happyShift action_30
action_24 (22) = happyShift action_31
action_24 _ = happyFail

action_25 (23) = happyShift action_40
action_25 _ = happyFail

action_26 (23) = happyShift action_39
action_26 _ = happyFail

action_27 (20) = happyShift action_38
action_27 _ = happyFail

action_28 (23) = happyShift action_37
action_28 _ = happyFail

action_29 (23) = happyShift action_36
action_29 _ = happyFail

action_30 (23) = happyShift action_35
action_30 _ = happyFail

action_31 (23) = happyShift action_34
action_31 _ = happyFail

action_32 _ = happyReduce_12

action_33 _ = happyReduce_10

action_34 (23) = happyShift action_47
action_34 _ = happyFail

action_35 (20) = happyShift action_46
action_35 _ = happyFail

action_36 (25) = happyShift action_45
action_36 _ = happyFail

action_37 (21) = happyShift action_44
action_37 _ = happyFail

action_38 (23) = happyShift action_43
action_38 _ = happyFail

action_39 (20) = happyShift action_42
action_39 _ = happyFail

action_40 (21) = happyShift action_41
action_40 _ = happyFail

action_41 (25) = happyShift action_54
action_41 _ = happyFail

action_42 (25) = happyShift action_53
action_42 _ = happyFail

action_43 (25) = happyShift action_52
action_43 _ = happyFail

action_44 (21) = happyShift action_51
action_44 _ = happyFail

action_45 _ = happyReduce_18

action_46 (23) = happyShift action_50
action_46 (13) = happyGoto action_49
action_46 _ = happyReduce_20

action_47 (23) = happyShift action_48
action_47 _ = happyFail

action_48 (25) = happyShift action_58
action_48 _ = happyFail

action_49 (25) = happyShift action_57
action_49 _ = happyFail

action_50 (23) = happyShift action_50
action_50 (13) = happyGoto action_56
action_50 _ = happyReduce_20

action_51 (25) = happyShift action_55
action_51 _ = happyFail

action_52 _ = happyReduce_15

action_53 _ = happyReduce_14

action_54 _ = happyReduce_13

action_55 _ = happyReduce_17

action_56 _ = happyReduce_21

action_57 _ = happyReduce_19

action_58 _ = happyReduce_16

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (IProgram happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (IFunction happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (IArguments happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  8 happyReduction_6
happyReduction_6  =  HappyAbsSyn8
		 ([]
	)

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 10 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyTerminal (TokenNum happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (IBlock happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 12 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_4)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Ilc happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 12 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Ild happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 12 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyTerminal (TokenReg happy_var_4)) `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Ist happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 12 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyTerminal (TokenReg happy_var_5)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_4)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	(HappyTerminal (TokenOp happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Iop happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 6 12 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_5)) `HappyStk`
	(HappyTerminal (TokenNum happy_var_4)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Ibr happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Iret happy_var_3
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 12 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	(HappyTerminal (TokenReg happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Icall happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0  13 happyReduction_20
happyReduction_20  =  HappyAbsSyn13
		 ([]
	)

happyReduce_21 = happySpecReduce_2  13 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_2)
	(HappyTerminal (TokenReg happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 26 26 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLc -> cont 14;
	TokenLd -> cont 15;
	TokenSt -> cont 16;
	TokenBr -> cont 17;
	TokenRet -> cont 18;
	TokenCall -> cont 19;
	TokenId happy_dollar_dollar -> cont 20;
	TokenNum happy_dollar_dollar -> cont 21;
	TokenOp happy_dollar_dollar -> cont 22;
	TokenReg happy_dollar_dollar -> cont 23;
	TokenOB -> cont 24;
	TokenCB -> cont 25;
	_ -> happyError' (tk:tks)
	}

happyError_ 26 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

calc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Syntax Error."

type Reg      = Int
type Var      = String
type FuncName = String
type BlockNum = Int

data IProgram 
	= IProgram [IFunction]
    deriving (Show, Eq)
    
data IFunction
	= IFunction FuncName IArguments [IBlock]
	deriving (Show, Eq)
    
data IArguments
	= IArguments [Var]
	deriving (Show, Eq)

data IBlock
	= IBlock BlockNum [IInstruction]
	deriving (Show, Eq)
    
data IInstruction
	= Ilc Reg Int
	| Ild Reg Var
	| Ist Var Reg
	| Iop Op Reg Reg Reg
	| Ibr Reg BlockNum BlockNum
	| Iret Reg
	| Icall Reg FuncName [Reg]
	deriving (Show, Eq)
  
data Op
  = Add
  | Sub
  | Mul
  | Div
  | LessThan
  | GreaterThan
  | DEq
  deriving (Show, Eq)

data Token
  = TokenLc
  | TokenLd
  | TokenSt
  | TokenOp Op
  | TokenBr
  | TokenRet
  | TokenCall
  | TokenReg Int
  | TokenId String
  | TokenNum Int
  | TokenOB
  | TokenCB
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('-':cs) = lexMinus ('-':cs)
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (_:cs) = lexer cs

lexMinus ('-':cs) =
   case span isDigit cs of
      (num,rest) -> TokenNum (read ('-':num)) : lexer rest -- Negative numbers

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span (\x -> (isDigit x) || (isAlpha x)) cs of
      ("lc",rest)   -> TokenLc : lexer rest
      ("ld",rest)   -> TokenLd : lexer rest
      ("st",rest)   -> TokenSt : lexer rest
      ("add",rest)  -> TokenOp Add : lexer rest
      ("sub",rest)  -> TokenOp Sub : lexer rest
      ("mul",rest)  -> TokenOp Mul : lexer rest
      ("div",rest)  -> TokenOp Div : lexer rest
      ("lt",rest)   -> TokenOp LessThan : lexer rest
      ("gt",rest)   -> TokenOp GreaterThan : lexer rest
      ("cmp",rest)  -> TokenOp DEq : lexer rest
      ("br",rest)   -> TokenBr : lexer rest
      ("ret",rest)  -> TokenRet : lexer rest
      ("call",rest) -> TokenCall : lexer rest
      (var,rest)    -> lexReg var rest
      
lexReg cs csrest =
   case span isAlpha cs of
      ("r",rest)   -> if x1 == y1 then TokenReg (read x1) : lexer csrest
                      else TokenId cs : lexer csrest
                      where (x1,x2) = span isDigit rest
                            (y1,y2) = span (\x -> (isDigit x) || (isAlpha x)) rest
      (_,rest)     -> TokenId cs : lexer csrest
      
parseFile :: FilePath -> IO IProgram -- Once an IO, always an IO
parseFile f = do
    contents <- readFile f
    return (calc (lexer contents))
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
