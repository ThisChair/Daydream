module SymTable where
import Data.Map as M
import Data.List as L
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class(lift)
import Lexer

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { category :: Category    -- Representacion de un alcance particular.
                         , scope    :: Integer
                         , typeS    :: (String,Integer)
                         , otherS   :: [(String,Integer)]
                         , pos      :: AlexPosn  } deriving (Show, Eq)

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
    Nothing -> search filtered_stack
        where
            filtered_stack = L.filter (\x -> elem (actualScope x) (keys chain)) stack 
            search [] = Nothing
            search l  = M.lookup ((actualScope . head) filtered_stack) chain

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

noPos :: AlexPosn
noPos = AlexPn 0 0 0

scopeZero :: [(String,SymScope)]
scopeZero = [
    ("Type",  (SymScope CType 0 ("Type",0) [] noPos)),
    ("Unit",  (SymScope CType 0 ("Type",0) [] noPos)),
    ("Int",   (SymScope CType 0 ("Type",0) [] noPos)),
    ("Float", (SymScope CType 0 ("Type",0) [] noPos)),
    ("Char",  (SymScope CType 0 ("Type",0) [] noPos)),
    ("String",(SymScope CType 0 ("Type",0) [] noPos)),
    ("Bool",  (SymScope CType 0 ("Type",0) [] noPos)),
    ("_list", (SymScope CType 0 ("Type",0) [] noPos)),
    ("_tuple",(SymScope CType 0 ("Type",0) [] noPos)),
    ("_array",(SymScope CType 0 ("Type",0) [] noPos)),
    ("_dict", (SymScope CType 0 ("Type",0) [] noPos)),
    ("_ptr",  (SymScope CType 0 ("Type",0) [] noPos))
    ]
scopeOne :: [(String,SymScope)]
scopeOne = [
    ("malloc", (SymScope CFunc  1 ("Unit",0) [("size",2)] noPos)),
    ("size",   (SymScope CParam 3 ("Int",0) [] noPos)),
    ("free",   (SymScope CFunc  1 ("Unit",0) [("ptr",3)] noPos)),
    ("ptr",    (SymScope CParam 4 ("_ptr",0) [] noPos))
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
        Nothing -> throwE $ "Undeclared symbol: " ++ id


searchType :: String -> ParseMonad (String,Integer)
searchType id = do
    ss <- lift get
    case (lookupTable' id ss) of
        Just sym -> return (id,scope sym)
        Nothing -> throwE $ "Unexistent type: " ++ id

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
