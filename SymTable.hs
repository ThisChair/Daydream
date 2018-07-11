module SymTable where
import Data.Map as M
import Data.List as L
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Monad(zipWithM_,zipWithM)
import Lexer
import SyntaxTree

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { scope    :: Integer
                         , typeS    :: (Type,Integer)
                         , otherS   :: [Instruction]
                         , pos      :: AlexPosn  } deriving (Show, Eq)

readType :: String -> Type
readType "Int" = TypeInt
readType "Float" = TypeFloat
readType "Bool" = TypeBool
readType "Char" = TypeChar
readType "String" = TypeString
readType s = TypeData s

insertSym :: String -> SymScope -> SymTable -> SymTable
insertSym elem elemScope table = 
        M.insertWith (M.union) elem newSym table
    where
        newSym = M.fromList [(scope elemScope,elemScope)]

insertSym' :: (String,SymScope) -> SymTable -> SymTable
insertSym' (elem,elemScope) table = insertSym elem elemScope table

lookupTable' :: String -> ScopeStack -> Maybe SymScope
lookupTable' elem (table,stack,_) = lookupTable elem table stack

lookupTable :: String -> SymTable -> Stack -> Maybe SymScope
lookupTable elem table stack = checkChain stack chain
    where 
        chain = M.lookup elem table

checkChain :: Stack -> Maybe (Map Integer SymScope) -> Maybe SymScope
checkChain stack Nothing = Nothing
checkChain stack (Just chain) = case (M.lookup 0 chain) of 
    (Just x) -> Just x
    Nothing -> search filtered_stack
        where
            filtered_stack = L.filter (\x -> elem (actualScope x) (keys chain)) stack 
            search [] = Nothing
            search l  = M.lookup ((actualScope . head) filtered_stack) chain

type ScopeStack = (SymTable,Stack,Integer)

type ParseMonad = ExceptT String (StateT ScopeStack (WriterT [String] IO))

type Stack = [StackEntry]

data ScopeCategory = SFunc | SType | SCons | SFor | SBlock | SGlobal deriving (Show, Eq)

data StackEntry = StackEntry { actualScope :: Integer
                             , sCategory   :: ScopeCategory
                             , belongs     :: Maybe (String,Integer)
                             , other       :: [(String,Integer)]  } deriving (Show, Eq)

pop :: Stack -> Stack
pop (x:xs) = xs

push :: StackEntry -> Stack -> Stack
push x xs = x:xs 


initialState :: ScopeStack
initialState = (initialSymTable, [StackEntry 2 SGlobal Nothing [], StackEntry 1 SGlobal Nothing []],5)

noPos :: AlexPosn
noPos = AlexPn 0 0 0

scopeZero :: [(String,SymScope)]
scopeZero = [
    ("Type",  (SymScope 0 (TypeType,0) [] noPos)),
    ("Unit",  (SymScope 0 (TypeType,0) [] noPos)),
    ("Int",   (SymScope 0 (TypeType,0) [] noPos)),
    ("Float", (SymScope 0 (TypeType,0) [] noPos)),
    ("Char",  (SymScope 0 (TypeType,0) [] noPos)),
    ("String",(SymScope 0 (TypeType,0) [] noPos)),
    ("Bool",  (SymScope 0 (TypeType,0) [] noPos)),
    ("_list", (SymScope 0 (TypeType,0) [] noPos)),
    ("_tuple",(SymScope 0 (TypeType,0) [] noPos)),
    ("_array",(SymScope 0 (TypeType,0) [] noPos)),
    ("_dict", (SymScope 0 (TypeType,0) [] noPos)),
    ("_ptr",  (SymScope 0 (TypeType,0) [] noPos))
    ]
scopeOne :: [(String,SymScope)]
scopeOne = []

initialSymTable :: SymTable
initialSymTable = P.foldr insertSym' M.empty (scopeZero ++ scopeOne)

modifySymTable :: (SymTable -> SymTable) -> ScopeStack -> ScopeStack
modifySymTable f (sym,st,i) = (f sym,st,i)

modifyStack :: (Stack -> Stack) -> ScopeStack -> ScopeStack
modifyStack f (sym,st,i) = (sym,f st,i)

addScopeNumber :: ScopeStack -> ScopeStack
addScopeNumber (sym,st,i) = (sym,st,i+1)

getScopeNumber :: Monad m => StateT ScopeStack m Integer
getScopeNumber = do
    (_,_,i) <- get
    return i

getActualScope :: Monad m => StateT ScopeStack m Integer
getActualScope = do
    (_,(s:_),_) <- get
    return (actualScope s)

peekScope :: Monad m => Int -> StateT ScopeStack m Integer
peekScope n = do
    (_,s,_) <- get
    return $ actualScope (s !! n)

addNumber :: Monad m => StateT ScopeStack m ()
addNumber = modify addScopeNumber

pushS :: Monad m => StackEntry -> StateT ScopeStack m ()
pushS = modify . modifyStack . push

popS :: Monad m => StateT ScopeStack m ()
popS = modify $ modifyStack $ pop

insertSymS :: Monad m => String -> SymScope -> StateT ScopeStack m ()
insertSymS elem elemScope = modify . modifySymTable $ insertSym elem elemScope


insertIns :: String -> Integer -> [Instruction] -> ParseMonad ()
insertIns id s ins = lift $ modify . modifySymTable $ updateIns id s ins

updateIns :: String -> Integer -> [Instruction] -> SymTable -> SymTable
updateIns id s newins sym = case (M.lookup id sym >>= (\x -> M.lookup s x)) of
    Just (SymScope i (TypeFunc intype outype,_) ins pos) -> insertSym id (SymScope i (TypeFunc intype outype,0) (ins++newins) pos) sym


searchTable :: String -> ParseMonad (String,Integer,AlexPosn)
searchTable id = do
    ss <- lift get
    case (lookupTable' id ss) of
        Just sym -> return (id,scope sym, pos sym)
        Nothing -> do
            lift $ lift $ tell $ ["Undeclared symbol: " ++ id]
            return (id,-1, noPos)

searchTable' :: String -> ParseMonad ((String,Integer,AlexPosn),Type)
searchTable' id = do
    ss <- lift get
    case (lookupTable' id ss) of
        Just sym -> return ((id,scope sym, pos sym), fst $ typeS sym)
        Nothing -> do
            lift $ lift $ tell $ ["Undeclared symbol: " ++ id]
            return ((id,-1, noPos),TypeError)

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

searchType :: String -> ParseMonad (String,Integer)
searchType id = do
    ss <- lift get
    case (lookupTable' id ss) of
        Just sym -> return (id,scope sym)
        Nothing -> do
            lift $ lift $ tell $ ["Unexistent type: " ++ id]
            return (id,-1)

isPervasive :: String -> ScopeStack -> Bool
isPervasive id (sym,_,_) = case search of
            Just _ -> True
            Nothing -> False
    where
        search = M.lookup id sym >>= (\x -> M.lookup 0 x)

pervasiveCheck :: String -> AlexPosn -> ParseMonad ()
pervasiveCheck id (AlexPn _ i j) = do
    ss <- lift get
    if (isPervasive id ss) 
        then (lift $ lift $ tell $ [(showPos i j) ++ ": " ++ id ++ " is a reserved name and can not be used." ]) 
        else return ()

redeclaredCheck :: String -> Integer -> AlexPosn -> ParseMonad ()
redeclaredCheck id scope (AlexPn _ i j) = do
        (sym,_,_) <- lift get
        let search = M.lookup id sym >>= (\x -> M.lookup scope x)
        case search of
            Just ss -> lift $ lift $ redeclaredError id i j (pos ss)
            Nothing -> return ()
        

redeclaredError :: Monad m => String -> Int -> Int -> AlexPosn -> WriterT [String] m ()
redeclaredError id i j (AlexPn _ i' j') = tell [(showPos i j) ++ ": " ++ id ++ " already declared at: " ++ (showPos i' j')] 

zipWith3M_ :: Applicative m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f x y z = zipWithM_ (\x' (y',z') -> f x' y' z') x (zip y z)

getNumType :: String -> Type
getNumType n = if (elem '.' n) then TypeFloat else TypeInt

checkListType :: [Exp] -> AlexPosn -> ParseMonad Type
checkListType es (AlexPn _ i j) = case (expsType es) of
    TypeError -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ " Elements of a list must be of the same type."]
        return TypeError
    t -> return $ TypeList t

checkArrType :: [Exp] -> AlexPosn -> ParseMonad Type
checkArrType es (AlexPn _ i j) = case (expsType es) of
    TypeError -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ " Elements of an array must be of the same type."]
        return TypeError
    t -> return $ TypeArray t (show $ length es)

checkDictType :: [(Exp,Exp)] -> AlexPosn -> ParseMonad Type
checkDictType es (AlexPn _ i j) = case (expsType $ P.map fst es) of
    TypeError -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Keys of a dictionary must be of the same type."]
        return TypeError
    tk -> case (expsType $ P.map snd es) of
        TypeError -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Elements of a dictionary must be of the same type."]
            return TypeError
        tv -> return $ TypeDict tk tv

checkTupType :: [Exp] -> AlexPosn -> ParseMonad Type
checkTupType es (AlexPn _ i j) = if (elem TypeError (P.map returnType es)) 
    then return TypeError
    else return $ TypeTuple (P.map returnType es)

checkNumBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkNumBin l r (AlexPn _ i j) = case (returnType l) of
    TypeInt -> case (returnType r) of
        TypeInt -> return TypeInt
        TypeFloat -> return TypeFloat
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeFloat -> case (returnType r) of
        TypeInt -> return TypeFloat
        TypeFloat -> return TypeFloat
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as left operand."]
        return TypeError

checkIntBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkIntBin l r (AlexPn _ i j) = case (returnType l) of
    TypeInt -> case (returnType r) of
        TypeInt -> return TypeInt
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Int, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Int, but got " ++ (show t) ++ " as left operand."]
        return TypeError

checkNumUn :: Exp -> AlexPosn -> ParseMonad Type
checkNumUn e (AlexPn _ i j) = case (returnType e) of
    TypeInt -> return TypeInt
    TypeFloat -> return TypeFloat
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t)]
        return TypeError

checkBoolUn :: Exp -> AlexPosn -> ParseMonad Type
checkBoolUn e (AlexPn _ i j) = case (returnType e) of
    TypeBool -> return TypeBool
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Bool, but got " ++ (show t)]
        return TypeError

checkBoolBin :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkBoolBin l r (AlexPn _ i j) = case (returnType l) of
    TypeBool -> case (returnType r) of
        TypeBool -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Bool, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Bool, but got " ++ (show t) ++ " as left operand."]
        return TypeError

checkNumComp :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkNumComp l r (AlexPn _ i j) = case (returnType l) of
    TypeInt -> case (returnType r) of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeFloat -> case (returnType r) of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as left operand."]
        return TypeError

checkComp :: Exp -> Exp -> AlexPosn -> ParseMonad Type
checkComp l r (AlexPn _ i j) = case (returnType l) of
    TypeInt -> case (returnType r) of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeBool -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected equal types, but got Int as left operand and Bool as right operand."]
            return TypeError
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeFloat -> case (returnType r) of
        TypeInt -> return TypeBool
        TypeFloat -> return TypeBool
        TypeBool -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected equal types, but got Int as left operand and Bool as right operand."]
            return TypeError
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected numeric type, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeBool -> case (returnType r) of
        TypeBool -> return TypeBool
        TypeError -> return TypeError
        t -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected Bool, but got " ++ (show t) ++ " as right operand."]
            return TypeError
    TypeError -> return TypeError
    t -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected comparable type, but got " ++ (show t) ++ " as left operand."]
        return TypeError

checkIf :: Exp -> Instruction -> AlexPosn -> ParseMonad Type
checkIf cond ins (AlexPn _ i j) = case (returnType cond, returnType ins) of
    (TypeBool, TypeVoid) -> return TypeVoid
    (TypeError, _) -> return TypeError
    (_, TypeError) -> return TypeError
    (t, TypeVoid) -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected bool in if condition, but got " ++ (show t)]
        return TypeError
    (TypeBool, t) -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expression of type " ++ (show t) ++ "when expecting instruction."]
        return TypeError

checkIfE :: Exp -> Instruction -> Instruction -> AlexPosn -> ParseMonad Type
checkIfE cond ins inE (AlexPn _ i j) = case (returnType cond, returnType ins, returnType inE) of
    (TypeBool, TypeVoid, TypeVoid) -> return TypeVoid
    (TypeError, _, _) -> return TypeError
    (_, TypeError, _) -> return TypeError
    (_, _, TypeError) -> return TypeError
    (t, TypeVoid, TypeVoid) -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expected bool in if condition, but got " ++ (show t)]
        return TypeError
    (TypeBool, t, TypeVoid) -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expression of type " ++ (show t) ++ "when expecting instruction."]
        return TypeError
    (TypeBool, TypeVoid, t) -> do
        lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expression of type " ++ (show t) ++ "when expecting instruction."]
        return TypeError

checkDAssign :: Type -> RightValue -> AlexPosn -> ParseMonad Type
checkDAssign TypeError _ _ = return TypeError
checkDAssign t (ValueExp e) (AlexPn _ i j) = case (returnType e) of
    TypeError -> return TypeError
    x -> if t == x
        then return TypeVoid
        else do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Can't assign " ++ (show x) ++ " to " ++ (show t)]
            return TypeError
checkDAssign t (ValueCons c) (AlexPn _ i j) = return TypeError

assignType :: ([Identifier],[RightValue]) -> Type
assignType (ids,_) = if hasError then TypeError else TypeVoid
    where
        hasError = elem TypeError (P.map returnType ids)

checkIndex :: String -> Exp -> AlexPosn -> ParseMonad Type
checkIndex id e (AlexPn _ i j) = do
    (_,t) <- searchTable' id
    case (returnType e) of
        TypeError -> return TypeError
        TypeInt -> case t of
            TypeArray at _ -> return at
            TypeList lt -> return lt
            TypeError -> return TypeError
            te -> do
                lift $ lift $ tell [(showPos i j) ++ ": " ++ "Not indexable type: " ++ (show te)]
                return TypeError
        te -> do
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Expecting index by integers, but instead got " ++ (show te)]
            return TypeError

checkAssign :: ([Identifier],[RightValue]) -> ParseMonad Type
checkAssign (ids,rvs) = do
    ts <- zipWithM checkSingle ids rvs
    if (elem TypeError ts) then (return TypeError) else (return TypeVoid)

checkSingle :: Identifier -> RightValue -> ParseMonad Type
checkSingle (Variable t (id,_,(AlexPn _ i j))) (ValueExp e) = case (t,returnType e) of
    (TypeError,_) -> return TypeError
    (_, TypeError) -> return TypeError
    (TypeFloat, TypeInt) -> return TypeVoid  
    (t,te) -> if (t == te) 
        then return TypeVoid
        else do 
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Variable " ++ id ++ " of type " ++ (show t) ++ " can't be assigned with type " ++ (show te)]
            return TypeError
checkSingle _ _ = return TypeError

checkFunCall :: String -> [Exp] -> AlexPosn -> ParseMonad Type
checkFunCall id pars (AlexPn _ i j) = do
    (_,t) <- (searchTable' id)    
    case t of 
        TypeError -> return TypeError
        TypeFunc args ret -> if ((P.map returnType pars) == args)
            then case ret of
                [] -> return TypeVoid
                [x] -> return x
                _ -> return $ TypeTuple ret
            else do
                lift $ lift $ tell [(showPos i j) ++ ": " ++ "Calling function " ++ id ++ " with pars of types " ++ (show (P.map returnType pars)) ++ " while expecting types " ++ (show args) ]
                return TypeError

checkIFunCall :: FCall -> ParseMonad Type
checkIFunCall (FCall t id _)  = do   
    case t of 
        TypeError -> return TypeError
        TypeVoid -> return TypeVoid
        _ -> do            
            let (AlexPn _ i j) = tokenPos id
            lift $ lift $ tell [(showPos i j) ++ ": " ++ "Function " ++ (tokenVal id) ++ " with return type " ++ (show t) ++ " where none expected." ]
            return TypeError

checkInFun :: AlexPosn -> ParseMonad ()
checkInFun (AlexPn _ i j) = do
    (_,s,_) <- lift get
    case (P.filter (\x -> sCategory x == SFunc) s) of
        [] -> lift $ lift $ tell [(showPos i j) ++ ": Return instruction outside function." ]
        _ -> return ()

checkRetT :: Token -> [Instruction] -> [Type] -> ParseMonad ()
checkRetT id ins ts = zipWith3M_ checkTs (repeat id) (P.filter isRet ins) (repeat ts)

checkTs :: Token -> Instruction -> [Type] -> ParseMonad ()
checkTs id (Ret _ (ETup _ es)) ts = if ((P.map returnType es) == ts)
    then return ()
    else do
        let (AlexPn _ i j) = tokenPos id
        lift $ lift $ tell [(showPos i j) ++ ": Function " ++ (tokenVal id) ++ " expected return type " ++ (show ts) ++ " but got " ++ (show (P.map returnType es))]
checkTs id (Ret _ e) ts = if ((returnType e) == (head ts))
    then return ()
    else do
        let (AlexPn _ i j) = tokenPos id
        lift $ lift $ tell [(showPos i j) ++ ": Function " ++ (tokenVal id) ++ " expected return type " ++ (show $ head ts) ++ " but got " ++ (show (returnType e))]
        

isRet :: Instruction -> Bool
isRet (Ret _ _) = True
isRet _ = False 
