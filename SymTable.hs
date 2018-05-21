module SymTable where
import Data.Map as M
import Prelude as P
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { category :: String    -- Representacion de un alcance particular.
                         , scope    :: Integer
                         , typeS    :: Integer
                         , otherS   :: String  } deriving (Show, Eq)

insertSym :: String -> SymScope -> SymTable -> SymTable
insertSym elem elemScope table = 
        M.insertWith (M.union) elem newSym table
    where
        newSym = M.fromList [(scope elemScope,elemScope)]

type ScopeStack = (SymTable,Stack)

type ParseMonad = ExceptT String (StateT ScopeStack IO)

type Stack = [StackEntry]

data StackEntry = StackEntry { actualScope :: Integer
                             , closed      :: Bool
                             , other       :: String  } deriving (Show, Eq)

pop :: Stack -> Stack
pop (x:xs) = xs

push :: StackEntry -> Stack -> Stack
push x xs = x:xs 

initialState :: ScopeStack
initialState = (M.empty, [])

modifySymTable :: (SymTable -> SymTable) -> ScopeStack -> ScopeStack
modifySymTable f (sym,st) = (f sym,st)

modifyStack :: (Stack -> Stack) -> ScopeStack -> ScopeStack
modifyStack f (sym,st) = (sym,f st)

pushS :: Monad m => StackEntry -> StateT ScopeStack m ()
pushS = modify . modifyStack . push

popS :: Monad m => StateT ScopeStack m ()
popS = modify $ modifyStack $ pop

insertSymS :: Monad m => String -> SymScope -> StateT ScopeStack m ()
insertSymS elem elemScope = modify . modifySymTable $ insertSym elem elemScope
