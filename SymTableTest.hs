-- Just a test
module SymTableTest where
import Data.Map as M
import Prelude as P

type SymTable = Map String (Map Integer SymScope)    -- Tabla de simbolos, una tabla de hash de listas de alcances, indexadas por nombre.

data SymScope = SymScope { category :: String    -- Representacion de un alcance particular.
                         , scope :: Integer
                         , type_ :: Integer
                         , other :: String } deriving (Show, Eq)

type ScopeStack = [StackEntry]

data StackEntry = StackEntry { scope_stack :: Integer
                             , closed :: Bool
                             , other_stack :: String } deriving (Show, Eq)

-- variables de prueba
a = "a"
a0 = "a"
b = "b"
c = "c"
g = "global"
a_scope = SymScope "variable" 1 1 "nothing"
b_scope = SymScope "variable" 2 2 "nothing"
c_scope = SymScope "function" 3 1 "parameters"
a0_scope = SymScope "procedure" 2 1 "nothing"
global_scope = SymScope "global_escope" 0 0 "predefined"

-- Scope stack, inicializada con el scope global.
stack = [s3,s2,s1,s0]

-- Scopes
s0 = StackEntry 0 True "nothing"
s1 = StackEntry 1 False "nothing"
s2 = StackEntry 2 False "nothing"
s3 = StackEntry 3 False "nothing"


-- Tabla temporal, se supone que debe inicializarse con el scope global, esquema tentativo.
symbolTableInit = M.fromList [("GLOBAL", M.fromList [(0, global_scope)])]
symbolTable1 = insertSym symbolTableInit a a_scope
symbolTable2 = insertSym symbolTable1 b b_scope
symbolTable3 = insertSym symbolTable2 a0 a0_scope
symbolTable = insertSym symbolTable3 c c_scope


insertSym :: SymTable -> String -> SymScope -> SymTable
insertSym table elem elem_scope = M.insertWith (M.union) elem (M.fromList [(scope elem_scope,elem_scope)]) table

pop :: ScopeStack -> ScopeStack
pop [] = []
pop (x:xs) = xs

push :: StackEntry -> ScopeStack -> ScopeStack
push x xs = x:xs  

lookupTable :: String -> SymTable -> ScopeStack -> Maybe SymScope
lookupTable name table stack = checkChain stack chain
    where chain = M.lookup name table

checkChain :: ScopeStack -> Maybe (Map Integer SymScope) -> Maybe SymScope
checkChain stack Nothing = Nothing
checkChain stack (Just chain) = case (M.lookup 0 chain) of (Just x) -> Just (SymScope "PERVASIVE" 0 0 "PERVASIVE")
                                                           Nothing -> snd $ head $ M.toDescList $ M.map (checkScopeStack stack best) chain where best = SymScope "nothing" 0 0 "nothing"

checkScopeStack :: ScopeStack -> SymScope -> SymScope -> Maybe SymScope
checkScopeStack stack best entry
 | scope entry == ((scope_stack . head) stack) = Just entry
 | (best /= SymScope "nothing" 0 0 "nothing") && ((scope_stack . head) stack) == scope best = Just best
 | (closed . head) stack == True = Nothing 
 | otherwise = checkScopeStack (pop stack) best entry  
