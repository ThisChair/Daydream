module SymTable where
import Data.Map as M
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class(lift)

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { category :: Category    -- Representacion de un alcance particular.
                         , scope    :: Integer
                         , typeS    :: (String,Integer)
                         , otherS   :: [(String,Integer)]  } deriving (Show, Eq)

data Category = CFunc | CVar | CType | CParam | CField | CCons | CFor deriving (Show, Eq)

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
    Nothing -> (\x -> if (x /= []) then (snd . head) x else Nothing) $ M.toList $ M.filter (/=Nothing) $ M.map (checkScopeInStack stack ) chain

checkScopeInStack :: Stack -> SymScope -> Maybe SymScope
checkScopeInStack [] _ = Nothing
checkScopeInStack stack entry 
 | scope entry == ((actualScope . head)  stack) = Just entry
 | otherwise = checkScopeInStack (pop stack) entry

type ScopeStack = (SymTable,Stack,Integer)

type ParseMonad = ExceptT String (StateT ScopeStack IO)

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

scopeZero :: [(String,SymScope)]
scopeZero = [
    ("Type",  (SymScope CType 0 ("Type",0) [])),
    ("Unit",  (SymScope CType 0 ("Type",0) [])),
    ("Int",   (SymScope CType 0 ("Type",0) [])),
    ("Float", (SymScope CType 0 ("Type",0) [])),
    ("Char",  (SymScope CType 0 ("Type",0) [])),
    ("String",(SymScope CType 0 ("Type",0) [])),
    ("Bool",  (SymScope CType 0 ("Type",0) [])),
    ("_list", (SymScope CType 0 ("Type",0) [])),
    ("_tuple",(SymScope CType 0 ("Type",0) [])),
    ("_array",(SymScope CType 0 ("Type",0) [])),
    ("_dict", (SymScope CType 0 ("Type",0) [])),
    ("_ptr",  (SymScope CType 0 ("Type",0) []))
    ]
scopeOne :: [(String,SymScope)]
scopeOne = [
    ("malloc", (SymScope CFunc  1 ("Unit",0) [("size",2)])),
    ("size",   (SymScope CParam 3 ("Int",0) [])),
    ("free",   (SymScope CFunc  1 ("Unit",0) [("ptr",3)])),
    ("ptr",    (SymScope CParam 4 ("_ptr",0) []))
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

searchTable :: String -> ParseMonad (String,Integer)
searchTable id = do
    ss <- lift get
    case (lookupTable' id ss) of
        Just sym -> return (id,scope sym)
        Nothing -> throwE $ "Undeclared symbol: " ++ id

isPervasive :: String -> ScopeStack -> Bool
isPervasive id (sym,_,_) = case search of
            Just _ -> True
            Nothing -> False
    where
        search = M.lookup id sym >>= (\x -> M.lookup 0 x)

pervasiveCheck :: String -> ParseMonad ()
pervasiveCheck s = do
    ss <- lift get
    if (isPervasive s ss) 
        then (throwE $ "Can't redefine: " ++ s) 
        else return ()
