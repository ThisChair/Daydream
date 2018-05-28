module SymTable where
import Data.Map as M
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { category :: Category    -- Representacion de un alcance particular.
                         , scope    :: Integer
                         , typeS    :: (String,Integer)
                         , otherS   :: [(String,Integer)]  } deriving (Show, Eq)

data Category = CFunc | CVar | CType | CParam | CField | CCons deriving (Show, Eq)

insertSym :: String -> SymScope -> SymTable -> SymTable
insertSym elem elemScope table = 
        M.insertWith (M.union) elem newSym table
    where
        newSym = M.fromList [(scope elemScope,elemScope)]

insertSym' :: (String,SymScope) -> SymTable -> SymTable
insertSym' (elem,elemScope) table = insertSym elem elemScope table

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
initialSymTable = P.foldr insertSym' M.empty scopeZero

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

addNumber :: Monad m => StateT ScopeStack m ()
addNumber = modify addScopeNumber

pushS :: Monad m => StackEntry -> StateT ScopeStack m ()
pushS = modify . modifyStack . push

popS :: Monad m => StateT ScopeStack m ()
popS = modify $ modifyStack $ pop

insertSymS :: Monad m => String -> SymScope -> StateT ScopeStack m ()
insertSymS elem elemScope = modify . modifySymTable $ insertSym elem elemScope
