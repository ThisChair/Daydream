{-|
Module : SymTable
Authors : Carlos Infante
          Daniel Varela

Functions relevant to the construction of the symbol table
and type-checking.
-}
module SymTable where

import Data.Map as M
import Data.List as L
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Monad(zipWithM_,zipWithM,when)

import Lexer
import SyntaxTree
import Utils

-- | Symbol table, a Map indexing another Map of scopes by name.
type SymTable = Map String (Map Integer SymScope)

-- | Data of a variable.
data SymScope
    = SymScope { scope    :: Integer        -- ^ Scope number of the variable.
               , typeS    :: (Type,Integer) -- ^ Type of the variable and its scope.
               , otherS   :: [Instruction]  -- ^ List of instructions of a function scope.
               , pos      :: AlexPosn       -- ^ Position of the variable.
               }
    | VarScope { scope   :: Integer         -- ^ Scope of the variable.
               , decl    :: Bool            -- ^ True if declared.
               , typeS   :: (Type,Integer)  -- ^ Type and its scope.
               , pos     :: AlexPosn        -- ^ Position of the variable.
               , width   :: Integer         -- ^ Size of the variable.
               }
    | FunScope { scope   :: Integer         -- ^ Scope of the function.
               , typeS   :: (Type,Integer)  -- ^ Type and its scope.
               , parS    :: [(String,Integer)]
               , otherS  :: [Instruction]   -- ^ List of instructions of function.
               , pos     :: AlexPosn        -- ^ Position of the variable.
               } 
    deriving (Show, Eq)

-- | Translation from string to Type.
readType :: String -> Type
readType "Int" = TypeInt
readType "Float" = TypeFloat
readType "Bool" = TypeBool
readType "Char" = TypeChar
readType "String" = TypeString
readType s = TypeData s

-- | SymTable, scope stack and actual scope.
type ScopeStack = (SymTable,Stack,Integer)

-- | Stack structure for scopes.
type Stack = [StackEntry]

data ScopeCategory 
    -- | Block belonging to a function.
    = SFunc
    -- | Block belonging to a type creation.
    | SType
    -- | Block belonging to a type constructor.
    | SCons
    -- | Block belonging to a For cycle variables.
    | SFor
    -- | Normal block.
    | SBlock
    -- | Global scope.
    | SGlobal
    deriving (Show, Eq)

data StackEntry 
    = StackEntry { actualScope :: Integer                -- ^ Scope of the entry.
                 , sCategory   :: ScopeCategory          -- ^ As described above.
                 , belongs     :: Maybe (String,Integer) -- ^ A scope may belong to a function/type.
                 , other       :: [(String,Integer)]     -- ^ Saves certain variables that may be needed.
                 }
                 deriving (Show, Eq)

-- | Monad to keep the state during parsing.
type ParseMonad = ExceptT String (StateT ScopeStack (WriterT [String] IO))

-- | Stack pop operation.
pop :: Stack -> Stack
pop (x:xs) = xs

-- | Stack push operation.
push :: StackEntry -> Stack -> Stack
push x xs = x:xs 

-- | Insert or modify a SymScope indexed by an String.
insertSym :: String -> SymScope -> SymTable -> SymTable
insertSym elem elemScope = 
        M.insertWith M.union elem newSym
    where
        newSym = M.fromList [(scope elemScope,elemScope)]

-- | Similar to insertSym, but receives the String and SymScope as a tuple.
insertSym' :: (String,SymScope) -> SymTable -> SymTable
insertSym' (elem,elemScope) = insertSym elem elemScope

-- | Updates the instruction list of a function.
updateIns :: String -> Integer -> [Instruction] -> SymTable -> SymTable
updateIns id s newins sym = case M.lookup id sym >>= M.lookup s of
    Just (SymScope i (TypeFunc intype outype,_) ins pos) -> 
        insertSym id (SymScope i (TypeFunc intype outype,0) (ins++newins) pos) sym

-- | Updates the instruction for the correct type.
updateIns' :: String -> Integer -> [Instruction] -> SymTable -> SymTable
updateIns' id s newins sym = case M.lookup id sym >>= M.lookup s of
    Just (FunScope i t pars ins pos) -> 
        insertSym id (FunScope i t pars (ins++newins) pos) sym

-- | Updates paramaters on a function.
updatePars :: String -> Integer -> [(String,Integer)] -> SymTable -> SymTable
updatePars id s newpars sym = case M.lookup id sym >>= M.lookup s of
    Just (FunScope i t pars ins pos) ->
        insertSym id (FunScope i t (pars++newpars) ins pos) sym

-- | Looks for a SymScope in a SymTable checking the scope in a Stack.
lookupTable :: String -> SymTable -> Stack -> Maybe SymScope
lookupTable elem table stack = checkChain stack chain
    where 
        chain = M.lookup elem table

-- | Extracts the SymTable and Stack from ScopeStack and uses them to search for a SymScope.
lookupTable' :: String -> ScopeStack -> Maybe SymScope
lookupTable' elem (table,stack,_) = lookupTable elem table stack

-- | Looks for the correct Scope in the chain.
checkChain :: Stack -> Maybe (Map Integer SymScope) -> Maybe SymScope
checkChain stack Nothing = Nothing
checkChain stack (Just chain) = case M.lookup 0 chain of 
    (Just x) -> Just x
    Nothing -> search filtered_stack
        where
            filtered_stack = L.filter (\x -> actualScope x  `elem` keys chain) stack 
            search [] = Nothing
            search l  = M.lookup ((actualScope . head) filtered_stack) chain

-- | Initial state of the stack and SymTable.
initialState :: ScopeStack
initialState = (initialSymTable, [StackEntry 2 SGlobal Nothing [], StackEntry 1 SGlobal Nothing []],5)

-- |  Placeholder for elements with no position. Should be deleted later.
noPos :: AlexPosn
noPos = AlexPn 0 0 0

-- | Scope 0 (pervasive).
scopeZero :: [(String,SymScope)]
scopeZero = [
    ("Type",   SymScope 0 (TypeType,0) [] noPos),
    ("Unit",   SymScope 0 (TypeType,0) [] noPos),
    ("Int",    SymScope 0 (TypeType,0) [] noPos),
    ("Float",  SymScope 0 (TypeType,0) [] noPos),
    ("Char",   SymScope 0 (TypeType,0) [] noPos),
    ("String", SymScope 0 (TypeType,0) [] noPos),
    ("Bool",   SymScope 0 (TypeType,0) [] noPos),
    ("_list",  SymScope 0 (TypeType,0) [] noPos),
    ("_tuple", SymScope 0 (TypeType,0) [] noPos),
    ("_array", SymScope 0 (TypeType,0) [] noPos),
    ("_dict",  SymScope 0 (TypeType,0) [] noPos),
    ("_ptr",   SymScope 0 (TypeType,0) [] noPos)
    ]

-- | Scope 1 for language constants. Still unused.
scopeOne :: [(String,SymScope)]
scopeOne = []

-- | Initial state of the SymTable.
initialSymTable :: SymTable
initialSymTable = P.foldr insertSym' M.empty (scopeZero ++ scopeOne)

-- | Applies a transformation to a SymTable in a ScopeStack.
modifySymTable :: (SymTable -> SymTable) -> ScopeStack -> ScopeStack
modifySymTable f (sym,st,i) = (f sym,st,i)

-- | Applies a transformation to a Stack in a ScopeStack.
modifyStack :: (Stack -> Stack) -> ScopeStack -> ScopeStack
modifyStack f (sym,st,i) = (sym,f st,i)

-- | Increases the next scope counter.
addScopeNumber :: ScopeStack -> ScopeStack
addScopeNumber (sym,st,i) = (sym,st,i+1)

-- | Gets the next scope.
getScopeNumber :: Monad m => StateT ScopeStack m Integer
getScopeNumber = do
    (_,_,i) <- get
    return i

-- | Gets the actual scope number from the Stack.
getActualScope :: Monad m => StateT ScopeStack m Integer
getActualScope = do
    (_,s:_,_) <- get
    return (actualScope s)

-- | Gets scope number for the scope at depth n.
peekScope :: Monad m => Int -> StateT ScopeStack m Integer
peekScope n = do
    (_,s,_) <- get
    return $ actualScope (s !! n)

-- | Increases scope counter in the state.
addNumber :: Monad m => StateT ScopeStack m ()
addNumber = modify addScopeNumber

-- | Stack push operation in the state.
pushS :: Monad m => StackEntry -> StateT ScopeStack m ()
pushS = modify . modifyStack . push

-- | Stack pop operation in the state.
popS :: Monad m => StateT ScopeStack m ()
popS = modify $ modifyStack pop

-- | Insert symbol into SymTable from state.
insertSymS :: Monad m => String -> SymScope -> StateT ScopeStack m ()
insertSymS elem elemScope = modify . modifySymTable $ insertSym elem elemScope

-- | Insert Instructions into a function symbol.
insertIns :: String -> Integer -> [Instruction] -> ParseMonad ()
insertIns id s ins = lift $ modify . modifySymTable $ updateIns id s ins

-- | Insert Instructions into a function symbol.
insertIns' :: String -> Integer -> [Instruction] -> ParseMonad ()
insertIns' id s ins = lift $ modify . modifySymTable $ updateIns' id s ins

-- | Insert Params into a function symbol.
insertPars :: String -> Integer -> [(String,Integer)] -> ParseMonad ()
insertPars id s pars = lift $ modify . modifySymTable $ updatePars id s pars

-- | Search for symbol in table.
searchTable :: String -> ParseMonad (String,Integer,AlexPosn)
searchTable id = do
    ss <- lift get
    case lookupTable' id ss of
        Just sym -> return (id,scope sym, pos sym)
        Nothing -> do
            lift $ lift $ tell ["Undeclared symbol: " ++ id]
            return (id,-1, noPos)

-- | Search for symbol with an specific type in the table.
searchTable' :: String -> ParseMonad ((String,Integer,AlexPosn),Type)
searchTable' id = do
    ss <- lift get
    case lookupTable' id ss of
        Just sym -> return ((id,scope sym, pos sym), fst $ typeS sym)
        Nothing -> do
            lift $ lift $ tell ["Undeclared symbol: " ++ id]
            return ((id,-1, noPos),TypeError)

-- | Search or a type in the table.
searchType :: String -> ParseMonad (String,Integer)
searchType id = do
    ss <- lift get
    case lookupTable' id ss of
        Just sym -> return (id,scope sym)
        Nothing -> do
            lift $ lift $ tell ["Unexistent type: " ++ id]
            return (id,-1)

-- | Get the type specific entry in the table.
getType :: TypeName -> ParseMonad (Type,Integer)
getType (Name _ s) = do
    (t,scope) <- searchType s
    return (readType t,scope)
getType (List _ tn) = do
    (t,_) <- getType tn
    return (TypeList t,0)
getType (Array _ tn n) = do
    (t,_) <- getType tn
    return (TypeArray t (tokenVal n),0)
getType (Tuple _ tns) = do
    ts <- mapM getType tns
    return (TypeTuple (P.map fst ts),0)
getType (Dict _ (k,v)) = do
    (tk,_) <- getType k
    (tv,_) <- getType v
    return (TypeDict tk tv,0)
getType (Pointer _ t) = do
    (tp,_) <- getType t
    return (TypePointer tp, 0)

-- | True if the name is in scope 0.
isPervasive :: String -> ScopeStack -> Bool
isPervasive id (sym,_,_) = case search of
    Just _ -> True
    Nothing -> False
  where
    search = M.lookup id sym >>= M.lookup 0

-- | Error if a name is in scope 0.
pervasiveCheck :: String -> AlexPosn -> ParseMonad ()
pervasiveCheck id (AlexPn _ i j) = do
    ss <- lift get
    when (isPervasive id ss) $ lift $ lift $ tell errMsg
  where
    errMsg = [showPos i j ++ ": " ++ id ++ " is a reserved name and can not be used." ]

-- | Checks if a name has already been declared in an scope.
redeclaredCheck :: String -> Integer -> AlexPosn -> ParseMonad ()
redeclaredCheck id scope (AlexPn _ i j) = do
        (sym,_,_) <- lift get
        let search = M.lookup id sym >>= M.lookup scope
        case search of
            Just ss -> lift $ lift $ redeclaredError id i j (pos ss)
            Nothing -> return ()

-- | Error for repeated declaration.
redeclaredError :: Monad m => String -> Int -> Int -> AlexPosn -> WriterT [String] m ()
redeclaredError id i j (AlexPn _ i' j') = tell errMsg
  where
    errMsg = [showPos i j ++ ": " ++ id ++ " already declared at: " ++ showPos i' j']

-- | Checks if a number is an integer or a float.
getNumType :: String -> Type
getNumType n = if '.' `elem` n then TypeFloat else TypeInt

-- | Checks the homogeneity of a list and returns its type.
checkListType :: [Exp] -> AlexPosn -> ParseMonad Type
checkListType es (AlexPn _ i j) = case expsType es of
    TypeError -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ " Elements of a list must be of the same type."]
        return TypeError
    t -> return $ TypeList t

-- | Checks the homogeneity of an array and returns its type.
checkArrType :: [Exp] -> AlexPosn -> ParseMonad Type
checkArrType es (AlexPn _ i j) = case expsType es of
    TypeError -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ " Elements of an array must be of the same type."]
        return TypeError
    t -> return $ TypeArray t (show $ length es)

-- | Checks the homogeneity of keys and elements of a dictionary and returns its type.
checkDictType :: [(Exp,Exp)] -> AlexPosn -> ParseMonad Type
checkDictType es (AlexPn _ i j) = case expsType $ P.map fst es of
    TypeError -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Keys of a dictionary must be of the same type."]
        return TypeError
    tk -> case expsType $ P.map snd es of
        TypeError -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Elements of a dictionary must be of the same type."]
            return TypeError
        tv -> return $ TypeDict tk tv

-- | Returns the type of a tuple.
checkTupType :: [Exp] -> AlexPosn -> ParseMonad Type
checkTupType es (AlexPn _ i j) = if TypeError `elem` P.map returnType es
    then return TypeError
    else return $ TypeTuple (P.map returnType es)

-- | Check numeric binary operations.
checkNumBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkNumBin l r (AlexPn _ i j) = case returnType l of
    TypeInt -> case returnType r of
        TypeInt -> return TypeInt
        TypeFloat -> return TypeFloat
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell (rightErr t)
            return TypeError
    TypeFloat -> case returnType r of
        TypeInt -> return TypeFloat
        TypeFloat -> return TypeFloat
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell (rightErr t)
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell (leftErr t)
        return TypeError
  where
    rightErr t = [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as right operand."]
    leftErr t = [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as left operand."]

-- | Check int-only operations.
checkIntBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkIntBin l r (AlexPn _ i j) = case returnType l of
    TypeInt -> case returnType r of
        TypeInt -> return TypeInt
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Int, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Int, but got " ++ show t ++ " as left operand."]
        return TypeError

-- | Check unary numeric operations.
checkNumUn :: Exp -> AlexPosn -> ParseMonad Type
checkNumUn e (AlexPn _ i j) = case returnType e of
    TypeInt -> return TypeInt
    TypeFloat -> return TypeFloat
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t]
        return TypeError

-- | Check unary boolean operations.
checkBoolUn :: Exp -> AlexPosn -> ParseMonad Type
checkBoolUn e (AlexPn _ i j) = case returnType e of
    TypeBool -> return TypeBool
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Bool, but got " ++ show t]
        return TypeError

-- | Check binary boolean operations.
checkBoolBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkBoolBin l r (AlexPn _ i j) = case returnType l of
    TypeBool -> case returnType r of
        TypeBool -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Bool, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Bool, but got " ++ show t ++ " as left operand."]
        return TypeError

-- | Checks numeric comparisons.
checkNumComp :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkNumComp l r (AlexPn _ i j) = case returnType l of
    TypeInt -> case returnType r of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeFloat -> case returnType r of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as left operand."]
        return TypeError

-- | Checks valid comparable types.
checkComp :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkComp l r (AlexPn _ i j) = case returnType l of
    TypeInt -> case returnType r of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeBool -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected equal types, but got Int as left operand and Bool as right operand."]
            return TypeError
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeFloat -> case returnType r of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeBool -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected equal types, but got Int as left operand and Bool as right operand."]
            return TypeError
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected numeric type, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeBool -> case returnType r of
        TypeBool -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expected Bool, but got " ++ show t ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected comparable type, but got " ++ show t ++ " as left operand."]
        return TypeError

-- | If-Then instruction type check.
checkIf :: Exp -> Instruction -> AlexPosn -> ParseMonad Type
checkIf cond ins (AlexPn _ i j) = case (returnType cond, returnType ins) of
    (TypeBool, TypeVoid) -> return TypeVoid
    (TypeError, _) -> return TypeError
    (_, TypeError) -> return TypeError
    (t, TypeVoid) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected bool in if condition, but got " ++ show t]
        return TypeError
    (TypeBool, t) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expression of type " ++ show t ++ "when expecting instruction."]
        return TypeError

-- | If-Then-Else instruction type check.
checkIfE :: Exp -> Instruction -> Instruction -> AlexPosn -> ParseMonad Type
checkIfE cond ins inE (AlexPn _ i j) = case (returnType cond, returnType ins, returnType inE) of
    (TypeBool, TypeVoid, TypeVoid) -> return TypeVoid
    (TypeError, _, _) -> return TypeError
    (_, TypeError, _) -> return TypeError
    (_, _, TypeError) -> return TypeError
    (t, TypeVoid, TypeVoid) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected bool in if condition, but got " ++ show t]
        return TypeError
    (TypeBool, t, TypeVoid) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expression of type " ++ show t ++ "when expecting instruction."]
        return TypeError
    (TypeBool, TypeVoid, t) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expression of type " ++ show t ++ "when expecting instruction."]
        return TypeError

-- | Checks type compatibility of the RValue with the LValue.
checkDAssign :: Type -> RightValue -> AlexPosn -> ParseMonad Type
checkDAssign TypeError _ _ = return TypeError
checkDAssign t (ValueExp e) (AlexPn _ i j) = case returnType e of
    TypeError -> return TypeError
    x -> if t == x
        then return TypeVoid
        else do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Can't assign " ++ show x ++ " to " ++ show t]
            return TypeError
checkDAssign t (ValueCons c) (AlexPn _ i j) = return TypeError

-- | Returns type of assign instruction.
assignType :: ([Identifier],[RightValue]) -> Type
assignType (ids,_) = if hasError then TypeError else TypeVoid
    where
        hasError = TypeError `elem` P.map returnType ids

-- | Check that, when indexing, the type is indexable.
checkIndex :: String -> Exp -> AlexPosn -> ParseMonad Type
checkIndex id e (AlexPn _ i j) = do
    (_,t) <- searchTable' id
    case returnType e of
        TypeError -> return TypeError
        TypeInt -> case t of
            TypeArray at _ -> return at
            TypeList lt -> return lt
            TypeError -> return TypeError
            te -> do
                lift $ lift $ tell [showPos i j ++ ": " ++ "Not indexable type: " ++ show te]
                return TypeError
        te -> do
            lift $ lift $ tell [showPos i j ++ ": " ++ "Expecting index by integers, but instead got " ++ show te]
            return TypeError

-- | Checks type of multiple assigns.
checkAssign :: ([Identifier],[RightValue]) -> ParseMonad Type
checkAssign (ids,rvs) = do
    ts <- zipWithM checkSingle ids rvs
    if TypeError `elem` ts then return TypeError else return TypeVoid

-- | Checks type of single assign.
checkSingle :: Identifier -> RightValue -> ParseMonad Type
checkSingle (Variable t (id,_,AlexPn _ i j)) (ValueExp e) = case (t,returnType e) of
    (TypeError,_) -> return TypeError
    (_, TypeError) -> return TypeError
    (TypeFloat, TypeInt) -> return TypeVoid  
    (t,te) -> if t =+ te 
        then return TypeVoid
        else do 
            lift $ lift $ tell [showPos i j ++ ": " ++ "Variable " ++ id ++ " of type " ++ show t ++ " can't be assigned with type " ++ show te]
            return TypeError
checkSingle _ _ = return TypeError

-- | Check the function call type.
checkFunCall :: String -> [Exp] -> AlexPosn -> ParseMonad Type
checkFunCall id pars (AlexPn _ i j) = do
    (_,t) <- searchTable' id
    case t of 
        TypeError -> return TypeError
        TypeFunc args ret -> if and $ zipWith (=+) (P.map returnType pars) args
            then case ret of
                [] -> return TypeVoid
                [x] -> return x
                _ -> return $ TypeTuple ret
            else do
                lift $ lift $ tell $ errorMsg args
                return TypeError
  where
    errorMsg args = [showPos i j ++ ": " ++ "Calling function " ++ id ++ " with parameters of types " ++ show (P.map returnType pars) ++ " while expecting types " ++ show args ]

-- | Check that a function called as an instruction has no return type.
checkIFunCall :: FCall -> ParseMonad Type
checkIFunCall (FCall t id _)  = case t of 
    TypeError -> return TypeError
    TypeVoid -> return TypeVoid
    _ -> do            
        let (AlexPn _ i j) = tokenPos id
        lift $ lift $ tell [showPos i j ++ ": " ++ "Function " ++ tokenVal id ++ " with return type " ++ show t ++ " where none expected." ]
        return TypeError

-- | Check that the return instruction isn't used outside a function.
checkInFun :: AlexPosn -> ParseMonad ()
checkInFun (AlexPn _ i j) = do
    (_,s,_) <- lift get
    case P.filter (\x -> sCategory x == SFunc) s of
        [] -> lift $ lift $ tell [showPos i j ++ ": Return instruction outside function." ]
        _ -> return ()

-- | Check every return instruction in a function.
checkRetT :: Token -> [Instruction] -> [Type] -> ParseMonad ()
checkRetT id ins ts = zipWith3M_ checkTs (repeat id) (P.filter isRet ins) (repeat ts)

-- | Check the correct return of a function.
checkTs :: Token -> Instruction -> [Type] -> ParseMonad ()
checkTs id (Ret _ (ETup _ es)) ts = if P.map returnType es == ts
    then return ()
    else do
        let (AlexPn _ i j) = tokenPos id
        lift $ lift $ tell [showPos i j ++ ": Function " ++ tokenVal id ++ " expected return type " ++ show ts ++ " but got " ++ show (P.map returnType es)]
checkTs id (Ret _ e) ts = if returnType e == head ts
    then return ()
    else do
        let (AlexPn _ i j) = tokenPos id
        lift $ lift $ tell [showPos i j ++ ": Function " ++ tokenVal id ++ " expected return type " ++ show (head ts) ++ " but got " ++ show (returnType e)]

-- | Check if a instruction is a return.
isRet :: Instruction -> Bool
isRet (Ret _ _) = True
isRet _ = False 

-- | While instruction type check.
checkWhile :: Exp -> Instruction -> AlexPosn -> ParseMonad Type
checkWhile cond ins (AlexPn _ i j) = case (returnType cond, returnType ins) of
    (TypeBool, TypeVoid) -> return TypeVoid
    (TypeError, _) -> return TypeError
    (_, TypeError) -> return TypeError
    (t, TypeVoid) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expected bool in where condition, but got " ++ show t]
        return TypeError
    (TypeBool, t) -> do
        lift $ lift $ tell [showPos i j ++ ": " ++ "Expression of type " ++ show t ++ "when expecting instruction."]
        return TypeError