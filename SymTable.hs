module SymTable where
import Data.Map as M
import Data.List as L
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Class(lift)
import Control.Monad(zipWithM_)
import Lexer
import SyntaxTree

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { scope    :: Integer
                         , typeS    :: (Type,Integer)
                         , otherS   :: [(String,Integer)]
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
scopeOne = [
    ("malloc", (SymScope 1 (TypeFunc [TypeInt] [TypePointer TypeInt],0) [("size",2)] noPos)),
    ("size",   (SymScope 3 (TypeInt,0) [] noPos)),
    ("free",   (SymScope 1 (TypeFunc [TypePointer TypeInt] [],0) [("ptr",3)] noPos)),
    ("ptr",    (SymScope 4 (TypePointer TypeInt,0) [] noPos))
    ]

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

addNumber :: Monad m => StateT ScopeStack m ()
addNumber = modify addScopeNumber

pushS :: Monad m => StackEntry -> StateT ScopeStack m ()
pushS = modify . modifyStack . push

popS :: Monad m => StateT ScopeStack m ()
popS = modify $ modifyStack $ pop

insertSymS :: Monad m => String -> SymScope -> StateT ScopeStack m ()
insertSymS elem elemScope = modify . modifySymTable $ insertSym elem elemScope

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
getNumType n = if (elem '.' n) then TypeInt else TypeFloat


