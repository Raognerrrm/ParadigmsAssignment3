{-# OPTIONS_GHC -w #-}
module Parser where
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

action_0 (14) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (14) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (14) = happyShift action_4
action_3 (5) = happyGoto action_7
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (22) = happyShift action_6
action_4 _ = happyFail

action_5 (30) = happyAccept
action_5 _ = happyFail

action_6 (28) = happyShift action_9
action_6 (7) = happyGoto action_8
action_6 _ = happyFail

action_7 _ = happyReduce_3

action_8 (17) = happyShift action_14
action_8 (8) = happyGoto action_13
action_8 _ = happyReduce_7

action_9 (22) = happyShift action_11
action_9 (29) = happyShift action_12
action_9 (9) = happyGoto action_10
action_9 _ = happyFail

action_10 (29) = happyShift action_19
action_10 _ = happyFail

action_11 (26) = happyShift action_18
action_11 _ = happyReduce_9

action_12 _ = happyReduce_5

action_13 (15) = happyShift action_17
action_13 (10) = happyGoto action_16
action_13 _ = happyFail

action_14 (22) = happyShift action_11
action_14 (9) = happyGoto action_15
action_14 _ = happyFail

action_15 (25) = happyShift action_26
action_15 _ = happyFail

action_16 _ = happyReduce_4

action_17 (18) = happyShift action_23
action_17 (21) = happyShift action_24
action_17 (22) = happyShift action_25
action_17 (11) = happyGoto action_21
action_17 (12) = happyGoto action_22
action_17 _ = happyReduce_12

action_18 (22) = happyShift action_11
action_18 (9) = happyGoto action_20
action_18 _ = happyFail

action_19 _ = happyReduce_6

action_20 _ = happyReduce_10

action_21 (16) = happyShift action_31
action_21 _ = happyFail

action_22 (25) = happyShift action_30
action_22 _ = happyFail

action_23 (22) = happyShift action_29
action_23 _ = happyFail

action_24 (22) = happyShift action_28
action_24 _ = happyFail

action_25 (27) = happyShift action_27
action_25 _ = happyFail

action_26 _ = happyReduce_8

action_27 (22) = happyShift action_35
action_27 (23) = happyShift action_36
action_27 (28) = happyShift action_37
action_27 (13) = happyGoto action_34
action_27 _ = happyFail

action_28 _ = happyReduce_17

action_29 (19) = happyShift action_33
action_29 _ = happyFail

action_30 (18) = happyShift action_23
action_30 (21) = happyShift action_24
action_30 (22) = happyShift action_25
action_30 (11) = happyGoto action_32
action_30 (12) = happyGoto action_22
action_30 _ = happyReduce_12

action_31 _ = happyReduce_11

action_32 _ = happyReduce_13

action_33 (15) = happyShift action_17
action_33 (10) = happyGoto action_40
action_33 _ = happyFail

action_34 _ = happyReduce_14

action_35 (28) = happyShift action_9
action_35 (7) = happyGoto action_39
action_35 _ = happyReduce_20

action_36 _ = happyReduce_18

action_37 (22) = happyShift action_35
action_37 (23) = happyShift action_36
action_37 (28) = happyShift action_37
action_37 (13) = happyGoto action_38
action_37 _ = happyFail

action_38 (24) = happyShift action_42
action_38 _ = happyFail

action_39 _ = happyReduce_19

action_40 (20) = happyShift action_41
action_40 _ = happyReduce_16

action_41 (15) = happyShift action_17
action_41 (10) = happyGoto action_44
action_41 _ = happyFail

action_42 (22) = happyShift action_35
action_42 (23) = happyShift action_36
action_42 (28) = happyShift action_37
action_42 (13) = happyGoto action_43
action_42 _ = happyFail

action_43 (29) = happyShift action_45
action_43 _ = happyFail

action_44 _ = happyReduce_15

action_45 _ = happyReduce_21

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

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
happyReduction_4 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn7
		 (Arguments []
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Arguments happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 (Variables []
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Variables happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Block happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  11 happyReduction_12
happyReduction_12  =  HappyAbsSyn11
		 ([]
	)

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 (StmtAssign happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 12 happyReduction_15
happyReduction_15 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (StmtIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 12 happyReduction_16
happyReduction_16 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (StmtIfThen happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn12
		 (StmtReturn happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn13
		 (ExpNum happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn13
		 (ExpIdArg happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn13
		 (ExpId happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 13 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyTerminal (TokenOp happy_var_3)) `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (ExpOp happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenFunction -> cont 14;
	TokenBegin -> cont 15;
	TokenEnd -> cont 16;
	TokenVars -> cont 17;
	TokenIf -> cont 18;
	TokenThen -> cont 19;
	TokenElse -> cont 20;
	TokenReturn -> cont 21;
	TokenId happy_dollar_dollar -> cont 22;
	TokenNum happy_dollar_dollar -> cont 23;
	TokenOp happy_dollar_dollar -> cont 24;
	TokenSemi -> cont 25;
	TokenComma -> cont 26;
	TokenEq -> cont 27;
	TokenOB -> cont 28;
	TokenCB -> cont 29;
	_ -> happyError' (tk:tks)
	}

happyError_ 30 tk tks = happyError' tks
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

type Var      = String
type FuncName = String

data Program
  = Program [Function]
  deriving Show
  
data Function
  = Function FuncName Arguments Variables Block
  deriving Show

data Arguments
  = Arguments [Var]
  deriving Show

data Variables
  = Variables [Var]
  deriving Show

data Block
  = Block [Statement]
  deriving Show

data Statement
  = StmtAssign Var Expression
  | StmtIfThen Var Block
  | StmtIfThenElse Var Block Block
  | StmtReturn Var
  deriving Show

data Expression
  = ExpNum Int
  | ExpId Var
  | ExpIdArg FuncName Arguments
  | ExpOp Expression Op Expression
  deriving Show
  
data Op
  = Add
  | Sub
  | Mul
  | Div
  | LessThan
  | GreaterThan
  | DEq
  deriving Show

data Token
  = TokenFunction
  | TokenBegin
  | TokenEnd
  | TokenVars
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenReturn
  | TokenId Var
  | TokenNum Int
  | TokenOp Op
  | TokenSemi
  | TokenComma
  | TokenEq
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
lexer ('=':cs) = lexEq ('=':cs)
lexer (';':cs) = TokenSemi : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('+':cs) = TokenOp Add : lexer cs
lexer ('*':cs) = TokenOp Mul : lexer cs
lexer ('/':cs) = TokenOp Div : lexer cs
lexer ('<':cs) = TokenOp LessThan : lexer cs
lexer ('>':cs) = TokenOp GreaterThan : lexer cs

lexMinus ('-':cs) =
   case span isDigit cs of
      ("",rest)  -> TokenOp Sub : lexer rest
      (num,rest) -> TokenNum (read ('-':num)) : lexer rest -- Negative numbers
      
lexEq ('=':cs) =
   case span (\x -> x == '=') cs of
      ("",rest)  -> TokenEq : lexer rest
      ("=",rest) -> TokenOp DEq : lexer rest -- Double equals ==

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span (\x -> (isDigit x) || (isAlpha x)) cs of
      ("FUNCTION",rest) -> TokenFunction : lexer rest
      ("BEGIN",rest)    -> TokenBegin : lexer rest
      ("END",rest)      -> TokenEnd : lexer rest
      ("IF",rest)       -> TokenIf : lexer rest
      ("THEN",rest)     -> TokenThen : lexer rest
      ("ELSE",rest)     -> TokenElse : lexer rest
      ("RETURN",rest)   -> TokenReturn : lexer rest
      ("VARS",rest)     -> TokenVars : lexer rest
      (id,rest)         -> TokenId id : lexer rest
      
parseFile :: FilePath -> IO Program -- Once an IO, always an IO
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
