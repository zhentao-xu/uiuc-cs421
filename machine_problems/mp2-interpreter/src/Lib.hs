module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y 
liftCompOp _ _ _ = ExnVal "Cannot lift"


--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = case H.lookup s env of 
    Just (IntVal i) -> IntVal i
    Just (BoolVal i) -> BoolVal i
    Just (CloVal xs body clenv) -> CloVal xs body clenv
    Just (ExnVal s) -> ExnVal s
    Nothing -> ExnVal "No match in env"

--- ### Arithmetic
-- special operation for divide, which will cause divide by zero error
eval (IntOpExp op e1 e2) env = 
    case (op, eval e1 env, eval e2 env) of 
        ("/", _, IntVal 0) -> ExnVal "Division by 0"
        (_, v1, v2) -> case (H.lookup op intOps) of
            Just f -> liftIntOp f v1 v2
            Nothing -> ExnVal  "No matching operator"

                                    
    

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = case (H.lookup op boolOps) of 
    Just f ->  liftBoolOp f (eval e1 env) (eval e2 env)
    Nothing -> ExnVal  "No matching operator"

eval (CompOpExp op e1 e2) env = case (H.lookup op compOps) of 
    Just f ->  liftCompOp f (eval e1 env) (eval e2 env)
    Nothing -> ExnVal  "No matching operator"

--- ### If Expressions

eval (IfExp e1 e2 e3) env = case (eval e1 env) of
    BoolVal True -> eval e2 env
    BoolVal False -> eval e3 env
    _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    case (eval e1 env) of
        CloVal params body clenv -> case (params , args) of 
            ([], []) -> eval body clenv
            (k:ks, a:as) -> let newenv = H.insert k (eval a env) clenv
                            in eval (AppExp (FunExp ks body) as) newenv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = case pairs of 
    [] -> eval body env
    ((k,v):kvs) -> eval (LetExp kvs body) (H.insert k (eval v env) env)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, H.insert var (eval e env) env)

--- ### Sequencing

exec (SeqStmt stmts) penv env = case stmts of
    [] -> ("", penv, env)
    (s:ss) ->   let (vals, penvs, envs) = exec s penv env
                    (valss, penvss, envss) = exec (SeqStmt ss) penvs envs
                in (vals ++ valss, penvss, envss)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = case (eval e1 env) of
    BoolVal True -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements


exec p@(ProcedureStmt name params body) penv env = ("", H.insert name p penv, env)


exec (CallStmt name args) penv env = case (H.lookup name penv) of
    Just (ProcedureStmt _ params body) ->  let newenv = fromList( zip params (map (\x -> eval x env) args) ) 
                                            in exec body penv (H.union newenv env) 
    Nothing -> ("Procedure not found", penv, env)
