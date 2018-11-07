module TAC where

import SymTable
import SyntaxTree
import Lexer
import Control.Monad.Trans.State.Lazy
import Control.Monad (forM)
import Data.Map as M
import Data.List as L

class TAC_convertible a where
    toTAC :: SymTable -> a -> State (Int,Int,String,String,String) [TAC]

    --toTACArray :: SymTable -> a -> State (Int,Int,String,String,String) [[TAC]]

data TAC = Quadruplet { op :: Operator
                      , arg1 :: String
                      , arg2 :: String
                      , result :: String
                      , label :: String } |
           IfRegister { cond :: TAC
                      , truejumplabel :: String
                      , truecode :: [TAC]
                      , falsejumplabel :: String
                      , falsecode :: [TAC]
                      , result :: String
                      , label :: String } deriving (Show) -- quitar el campo label

data Operator = OSum |
                ODif |
                OMul |
                ODiv |
                OMod |
                OPot |
                ODivE |
                OLShift |
                ORShift |
                OBitOr |
                OBitAnd | 
                OBitXor |
                OOr |
                OAnd |
                OGEq |
                OGreat |
                OLEq |
                OLess |
                ONEq |
                OEqual |
                ONeg |
                ONot |
                OBitNot |
                OAssign |
                OJump |
                OIdent | 
                OPrint |  
                OPrintLn |    
                ONone
                deriving (Eq)

instance Show Operator where
    show OSum = "+"
    show ODif = "-"
    show OMul = "*"
    show ODiv = "/"
    show OMod = "%"
    show OPot = "**"
    show ODivE = "//"
    show OLShift = "<<"
    show ORShift = ">>"
    show OBitOr = "|"
    show OBitAnd = "&"
    show OBitXor = "^"
    show OOr = "||"
    show OAnd = "&&"
    show OGEq = ">="
    show OGreat = ">"
    show OLEq = "<="
    show OLess = "<"
    show ONEq = "!="
    show OEqual = "=="
    show ONone = "no-op"
    show ONeg = "-" -- Cambiar a uminus?
    show ONot = "¬"
    show OBitNot = "~"
    show OAssign = ":="
    show OJump = "goto"
    show OPrint = "print"
    show OPrintLn = "println"

-- Funciones para el acceso a tetrapletas
fst' :: (a,b,c,d) -> a
fst' (a,_,_,_) = a

snd' :: (a,b,c,d) -> b
snd' (_,b,_,_) = b

trd' :: (a,b,c,d) -> c
trd' (_,_,c,_) = c

fth' :: (a,b,c,d) -> d
fth' (_,_,_,d) = d

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------- FUNCIONES PARA MANIPULACION DEL STATE --------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

genLabelCode :: String -> TAC
genLabelCode string = Quadruplet {op=ONone,arg1="",arg2="",result="",label=string}

-- Retorna un nuevo nombre temporal
genNewTemp :: State (Int,Int,String,String,String) String
genNewTemp = state $ \(temp_counter,label_counter,next_code,true_code,false_code) -> ('t' : (show $ temp_counter+1),(temp_counter+1,label_counter,next_code,true_code,false_code))

genNewLabel :: State (Int,Int,String,String,String) String
genNewLabel = state $ \(temp_counter,label_counter,next_code,true_code,false_code) -> ("L" ++ (show $ label_counter+1),(temp_counter,label_counter+1,next_code,true_code,false_code))

-- Retorna el ultimo temporal utilizado en una lista de TACs
getLatestTemp :: [TAC] -> String
getLatestTemp tacList = (result . head) tacList

-- Genera codigo para la incorporacion de etiquetas
genJumpCode :: String -> TAC
genJumpCode string = Quadruplet {op=OJump,arg1=string,arg2="",result="",label=""}

-- Retorna el nombre de la etiqueta actual
getCurrentLabel :: State (Int,Int,String,String,String) String
getCurrentLabel = do
    state <- get
    let label_counter = getLabelFromStateTuple state
        in return ("L" ++ label_counter)

getLabelFromStateTuple :: (Int,Int,String,String,String) -> String
getLabelFromStateTuple (_,l,_,_,_) = show l

-- Retorna la etiqueta correspondiente a S.next actual
getNextCode :: State (Int,Int,String,String,String) String
getNextCode = do
    state <- get
    return (getNextCodeFromStateTuple state)

getNextCodeFromStateTuple :: (Int,Int,String,String,String) -> String
getNextCodeFromStateTuple (_,_,l,_,_) = l

-- Asigna al valor de S.next una string dada
setNextCode :: String -> State (Int,Int,String,String,String) ()
setNextCode string = state $ \(c1,c2,nc,tc,fc) -> ((),(c1,c2,string,tc,fc)) 

-- Retorna la etiqueta correspondiente a E.true actual
getTrueCode :: State (Int,Int,String,String,String) String
getTrueCode = do
    state <- get
    return (getTrueCodeFromStateTuple state)

getTrueCodeFromStateTuple :: (Int,Int,String,String,String) -> String
getTrueCodeFromStateTuple (_,_,_,t,_) = t

-- Retorna la etiqueta correspondiente a E.false actual
getFalseCode :: State (Int,Int,String,String,String) String
getFalseCode = do
    state <- get
    return (getFalseCodeFromStateTuple state)

getFalseCodeFromStateTuple :: (Int,Int,String,String,String) -> String
getFalseCodeFromStateTuple (_,_,_,_,f) = f

-- Asigna al valor de E.true una string dada
setTrueCode :: String -> State (Int,Int,String,String,String) ()
setTrueCode string = state $ \(c1,c2,nc,tc,fc) -> ((),(c1,c2,nc,string,fc))

-- Asigna al valor de E.false una string dada
setFalseCode :: String -> State (Int,Int,String,String,String) ()
setFalseCode string = state $ \(c1,c2,nc,tc,fc) -> ((),(c1,c2,nc,tc,string))

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
----------------------- FUNCIONES PARA MANIPULACION DE TAC ---------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

-- Convierte un IfRegister en una String para su pronta impresion           
convertIfRegistertoString :: TAC -> String -> [TAC] -> String -> [TAC] -> String -> String -> String
convertIfRegistertoString c j1 fc j2 tc r l= l ++ ": " ++ "if " ++ cond_string ++ " goto " ++ j1 ++ "\n: goto " ++ j2 ++ truecode_string ++ falsecode_string   
    where cond_string = (arg1 c) ++ " " ++ (show $ op c) ++ " " ++ arg2 c
          falsecode_string = "\n" ++ ((intercalate "\n" . reverse . L.map convertTACtoString) fc)
          truecode_string = "\n" ++ ((intercalate "\n" . reverse . L.map convertTACtoString) tc) 
 
-- Convierte una Quadruplet en una String para su pronta impresion
convertQuadrupletoString :: Operator -> String -> String -> String -> String -> String
convertQuadrupletoString o a1 a2 r l
 | o == OAssign = l ++ ": " ++ r ++ " := " ++ a1
 | ((o == ONone) || (o == OIdent)) && (l == "") = "" 
 | ((o == ONone) || (o == OIdent)) && (l /= "") = l ++ ": " ++ (show ONone)
 | (o == ONeg) || (o == ONot) || (o == OBitNot) = l ++ ": " ++ r ++ " = " ++ (show o) ++ " " ++ (a1)
 | (o == OJump) = l ++ ": " ++ (show o) ++ " " ++ a1 
 | (o == OPrint) || (o == OPrintLn) = ": " ++ (show o) ++ " " ++ a1
 | otherwise = l ++ ": " ++ r ++ " = " ++ a1 ++ " " ++ (show o) ++ " " ++ a2 

-- Convierte un TAC en una String para su pronta impresion
convertTACtoString :: TAC -> String
convertTACtoString (IfRegister {cond=c,truejumplabel=j1,falsecode=fc,falsejumplabel=j2,truecode=tc,result=r,label=l}) = convertIfRegistertoString c j1 fc j2 tc r l 
convertTACtoString (Quadruplet {op=o,arg1=a1,arg2=a2,result=r,label=l}) = convertQuadrupletoString o a1 a2 r l

-- Imprime una lista de TACs en un formato legible
printTAC :: [TAC] -> IO()
printTAC tacList = do 
    let tacList' = (L.map convertTACtoString . reverse) tacList
        tacList'' = [x | x <- tacList', x /= ""]
        in (putStrLn . unlines) tacList''

setInsLabel :: [TAC] -> [TAC]
setInsLabel tacList = let trueroot = head tacList
                          tacList' = tail tacList
                          root = head tacList'
                          rootlabel = label root
                          top = last tacList
                          newtop = setlabel rootlabel top
                          body = (tail . reverse . tail) tacList'
                          newroot = setlabel "" root
                          in (trueroot : newroot : reverse (newtop : body))
                          where setlabel nl (Quadruplet {op=o,arg1=a1,arg2=a2,result=r,label=l}) =  createQuadruplet o a1 a2 r nl
                                setlabel nl (IfRegister {cond=c,truejumplabel=j1,falsecode=fc,falsejumplabel=j2,truecode=tc,result=r,label=l}) = createIfRegister c j1 fc j2 tc r l

setInsLabel' :: String -> [TAC] -> [TAC]
setInsLabel' label tacList = let trueroot = head tacList
                                 tacList' = tail tacList
                                 root = head tacList'
                                 rootlabel = label
                                 top = last tacList
                                 newtop = setlabel rootlabel top
                                 body = (tail . reverse . tail) tacList'
                                 newroot = setlabel "" root
                                 in (trueroot : newroot : reverse (newtop : body))
                                 where setlabel nl (Quadruplet {op=o,arg1=a1,arg2=a2,result=r,label=l}) =  createQuadruplet o a1 a2 r nl
                                       setlabel nl (IfRegister {cond=c,truejumplabel=j1,falsecode=fc,falsejumplabel=j2,truecode=tc,result=r,label=l}) = createIfRegister c j1 fc j2 tc r l

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
----------------- FUNCIONES AUXILIARES PARA GENERACION DE TAC ------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

-- Crea una Quadruplet
createQuadruplet :: Operator -> String -> String -> String -> String -> TAC
createQuadruplet o a1 a2 r l = Quadruplet {op=o, arg1=a1, arg2=a2, result=r, label=l}

-- Crea un IfRegister
createIfRegister :: TAC -> String -> [TAC] -> String -> [TAC] -> String -> String -> TAC
createIfRegister c j1 tc j2 fc r l = IfRegister {cond=c, truejumplabel=j1, truecode=tc, falsejumplabel=j2, falsecode=fc, result=r, label=l}

-- Revisa la tabla de simbolos jerarquica y retorna el offset asociado a las variables
lookupTierTable :: SymTable -> String -> Integer -> Integer
lookupTierTable symtable varname varscope = case M.lookup varname symtable of
                                                 Just var_scope_list -> case M.lookup varscope var_scope_list of
                                                                             Just offset -> scope offset

-- Funcion generica que genera TAC para cualquier operador binario aritmetico
genBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genBinOpTAC symtable bin_op exp1 exp2 = do
    leftTAC <- toTAC symtable exp1
    rightTAC <- toTAC symtable exp2
    newlabel <- genNewTemp
    let subTAC = rightTAC ++ leftTAC
        newTAC = createQuadruplet bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) newlabel ""
        in return (newTAC : subTAC)      

-- Funcion generica que genera TAC para cualquier operador binario booleano relacional
genRelBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genRelBinOpTAC symtable rel_bin_op exp1 exp2 = do
    newlabel <- genNewTemp -- Temporal para albergar el valor booleano
    t_jump <- genNewLabel  -- Etiqueta para saltar en caso de True
    f_jump <- genNewLabel  -- Etiqueta para saltar en caso de False
    leftTAC <- toTAC symtable exp1 
    rightTAC <- toTAC symtable exp2
    nextcode <- genNewLabel -- El nextcode de esta instruccion
    let subTAC = rightTAC ++ leftTAC
        labelcode = genJumpCode nextcode -- Codigo para saltar a la siguiente instruccion
        c = createQuadruplet rel_bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) "" "" -- TAC para el condicional del IfRegister
        true_code = [labelcode,createQuadruplet OAssign "1" "" newlabel t_jump] -- codigo en caso de True
        false_code = [labelcode,createQuadruplet OAssign "0" "" newlabel f_jump] -- codigo en caso de False
        newTAC = createIfRegister c t_jump true_code f_jump false_code newlabel ""
        in return (newTAC : subTAC)

-- Funcion generica que genera TAC para cualquier operador unario
genUnOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> State (Int,Int,String,String,String) [TAC]
genUnOpTAC symtable un_op exp = do
    expTAC <- toTAC symtable exp
    newlabel <- genNewTemp
    let newTAC = createQuadruplet un_op (getLatestTemp expTAC) "" newlabel ""
        in return (newTAC : expTAC)

-- Funcion generica que genera TAC de control de flujo para cualquier operador relacional binario
genFlowRelBinOPTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genFlowRelBinOPTAC symtable rel_bin_op exp1 exp2 = do
    t_jump <- getTrueCode
    f_jump <- getFalseCode
    leftTAC <- toTAC symtable exp1
    rightTAC <- toTAC symtable exp2
    let subTAC = rightTAC ++ leftTAC
        id1 = getLatestTemp leftTAC
        id2 = getLatestTemp rightTAC
        c = createQuadruplet rel_bin_op id1 id2 "" "" 
        noop = [createQuadruplet ONone "" "" "" ""]
        newTAC = createIfRegister c t_jump noop f_jump noop "" ""
        in return (newTAC : subTAC)

-- Funcion para generaicon de TAC para asignaciones de arreglos
genArrayTAC :: (String,[[TAC]]) -> State (Int,Int,String,String,String) [TAC]
genArrayTAC (id,array) = let id_decomposition = [ id ++ "[" ++ (show x) ++ "]" | x <- [0..length array] ]
                             assigments = [ createQuadruplet OAssign y "" x "" | x <- (L.map getLatestTemp array), y <- id_decomposition ]
                             in return (assigments) 

-- Funcion para generacion de TAC para codigo de funciones
genFuncTAC :: (TAC_convertible a) => SymTable -> [(String,a)] -> State (Int,Int,String,String,String) [TAC]
genFuncTAC symtable func_tuples = do
    funcTAC <- mapM (funcTupleToTAC symtable) func_tuples -- [[TAC]]
    return (concat funcTAC)

-- Funcion que genera TAC para tuplas de funciones
funcTupleToTAC :: (TAC_convertible a) => SymTable -> (String,a) -> State (Int,Int,String,String,String) [TAC]
funcTupleToTAC symtable (func_name,func_code) = do
    func_codesTAC <- toTAC symtable func_code -- Lista de [TAC] del codigo de las funciones
    let nameTAC = genLabelCode func_name -- TAC para la label de la funcion
        in return (nameTAC : func_codesTAC)

----------------------------------------------------------------------------
----------------------------------------------------------------------------
--------------------- FUNCIONES PARA GENERACION DE TAC ---------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- Inicio del arbol sintactico
instance TAC_convertible Init where
   toTAC symtable (Init _ _ _ ins_list) = do
        nextcode <- genNewLabel
        setNextCode nextcode
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de la lista de instrucciones del programa
        return (concat $ reverse insTAC)
  
-- Instrucciones
instance TAC_convertible Instruction where
    -- Bloques
    toTAC symtable (Block _ ins_list) = do
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de las instrucciones
        return (concat $ reverse insTAC)

    -- Asignaciones
    {-toTAC symtable (Assign _ (id_list,vr_list)) = do
        nextcode <- getNextCode -- El nextcode de esta instruccion
        vrTAC <- mapM (toTACArray symtable) vr_list -- lista de [[TAC]] de los right value
        idTAC <- mapM (toTAC symtable) id_list -- lista de TAC de los identificadores
        next_ins_nextcode <- genNewLabel -- El nextcode de la siguiente instruccion
        setNextCode next_ins_nextcode
        let idTAClist = concat idTAC
            idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
            assignmentTuples = reverse (zip idStringlist vrTAC) -- tripletas con las asignaciones correspondientes [(id,[[TAC]])]
            in do
                array_assignments_list <- mapM genArrayTAC assignmentTuples -- lista de asignaciones id := array :: [[TAC]]
                return (concat array_assignments_list)-}

    toTAC symtable (Assign _ (id_list,vr_list)) = do
        nextcode <- getNextCode -- El nextcode de esta instruccion
        vrTAC <- mapM (toTAC symtable) vr_list -- lista de TACs de los right value 
        idTAC <- mapM (toTAC symtable) id_list -- lista de TACs de los identificadores
        next_ins_nextcode <- genNewLabel -- El nextcode de la siguiente instruccion
        setNextCode next_ins_nextcode
        let vrTemplist = L.map getLatestTemp vrTAC -- lista de temporales finales de los rv a ser asignados a los id
            idTAClist = concat idTAC
            idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
            assignmentTuples = reverse (zip idStringlist vrTemplist) -- tripletas con las asignaciones correspondientes [(id,rv_temp)]
            newTAC = [ createQuadruplet OAssign (snd x) "" (fst x) "" | x <- assignmentTuples ] -- lista de TACs 'id := rv_temp' :: [TAC]
            newTAC' = [ fst x : snd x | x <- (zip newTAC (reverse idTAC)) ] -- [ [TAC] | ( TAC,[TAC]) ]
            finalTAC = concat [ fst x ++ snd x | x <- (zip newTAC' (reverse vrTAC)) ] -- :: [ [TAC] | ([TAC],[TAC]) ]
            in return finalTAC

    -- Selectores 
    toTAC symtable (IfThen _ exp ins) = do -- S -> if E then S1
        nextcode <- getNextCode -- El nextcode de esta instruccion
        exp_f_jump <- getNextCode -- E.false := S.next
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp    
        insTAC <- toTAC symtable ins -- S1.next := S1.next
        let labelcode = genLabelCode exp_t_jump
            in return (insTAC ++ (labelcode : expTAC))

    toTAC symtable (IfElse _ exp ins1 ins2) = do -- S -> if E then S1 else S2
        nextcode <- getNextCode -- El nextcode de esta instruccion
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- genNewLabel -- E.false := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp
        ins_list1TAC <- toTAC symtable ins1
        ins_list2TAC <- toTAC symtable ins2
        let labelcode1 = genLabelCode exp_t_jump
            labelcode2 = genLabelCode exp_f_jump
            jumpcode = genJumpCode nextcode
            labelcodefinal = genLabelCode nextcode
            in return (labelcodefinal : (ins_list2TAC ++ (labelcode2 : jumpcode : ins_list1TAC) ++ (labelcode1 : expTAC)))

    -- Iteradores
    toTAC symtable (While _ exp ins) = do -- S -> while E do S1
        nextcode <- getNextCode -- El next de esta instruccion
        begin <- genNewLabel -- S.begin = newlabel()
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- getNextCode -- E.false := S.next
        setNextCode begin -- S1.next := S.begin
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp
        ins_listTAC <- toTAC symtable ins
        let labelcode1 = genLabelCode begin
            labelcode2 = genLabelCode exp_t_jump
            jumpcode = genJumpCode begin
            labelcodefinal = genLabelCode nextcode
            in return (labelcodefinal : (jumpcode : (ins_listTAC ++ (labelcode2 : (reverse (labelcode1 : expTAC))))))

    -- Prints
    toTAC symtable (Print _ exp) = do 
        nextcode <- getNextCode -- El next de esta instruccion
        expTAC <- toTAC symtable exp
        let newTAC = createQuadruplet OPrint (getLatestTemp expTAC) "" "" ""
            labelcode = genLabelCode nextcode
            in return (labelcode : newTAC : expTAC)

    toTAC symtable (PrintLn _ exp) = do
        nextcode <- getNextCode -- El next de esta instruccion
        expTAC <- toTAC symtable exp
        let newTAC = createQuadruplet OPrintLn (getLatestTemp expTAC) "" "" ""
            labelcode = genLabelCode nextcode
            in return (labelcode : newTAC : expTAC)
 
-- Expresiones
instance TAC_convertible Exp where
    -- Binarias
    toTAC symtable (ESum _ exp1 exp2) = genBinOpTAC symtable OSum exp1 exp2
    toTAC symtable (EDif _ exp1 exp2) = genBinOpTAC symtable ODif exp1 exp2
    toTAC symtable (EMul _ exp1 exp2) = genBinOpTAC symtable OMul exp1 exp2
    toTAC symtable (EDiv _ exp1 exp2) = genBinOpTAC symtable ODiv exp1 exp2
    toTAC symtable (EMod _ exp1 exp2) = genBinOpTAC symtable OMod exp1 exp2
    toTAC symtable (EPot _ exp1 exp2 ) = genBinOpTAC symtable OPot exp1 exp2
    toTAC symtable (EDivE _ exp1 exp2 ) = genBinOpTAC symtable ODivE exp1 exp2
    toTAC symtable (ELShift _ exp1 exp2 ) = genBinOpTAC symtable OLShift exp1 exp2
    toTAC symtable (ERShift _ exp1 exp2 ) = genBinOpTAC symtable ORShift exp1 exp2
    toTAC symtable (EBitOr _ exp1 exp2 ) = genBinOpTAC symtable OBitOr exp1 exp2
    toTAC symtable (EBitAnd _ exp1 exp2 ) = genBinOpTAC symtable OBitAnd exp1 exp2
    toTAC symtable (EBitXor _ exp1 exp2 ) = genBinOpTAC symtable OBitXor exp1 exp2
    toTAC symtable (EOr _ exp1 exp2 ) = genBinOpTAC symtable OOr exp1 exp2
    toTAC symtable (EAnd _ exp1 exp2 ) = genBinOpTAC symtable OAnd exp1 exp2

    -- Booleanas relacionales aritmeticas
    toTAC symtable (EGEq _ exp1 exp2 ) = genRelBinOpTAC symtable OGEq exp1 exp2
    toTAC symtable (EGreat _ exp1 exp2 ) = genRelBinOpTAC symtable OGreat exp1 exp2
    toTAC symtable (ELEq _ exp1 exp2 ) = genRelBinOpTAC symtable OLEq exp1 exp2
    toTAC symtable (ELess _ exp1 exp2 ) = genRelBinOpTAC symtable OLess exp1 exp2
    toTAC symtable (ENEq _ exp1 exp2 ) = genRelBinOpTAC symtable ONEq exp1 exp2
    toTAC symtable (EEqual _ exp1 exp2 ) = genRelBinOpTAC symtable OEqual exp1 exp2

    -- Unarias
    toTAC symtable (ENeg _ exp ) = genUnOpTAC symtable ONeg exp
    toTAC symtable (ENot _ exp ) = genUnOpTAC symtable ONot exp 
    toTAC symtable (EBitNot _ exp ) = genUnOpTAC symtable OBitNot exp 

    -- Tokens
    toTAC symtable (EToken _ (TNum _ s)) = return [createQuadruplet ONone "" "" s "" ]
    toTAC symtable (EToken _ (TString _ s)) = return [createQuadruplet ONone "" "" s "" ]
    toTAC symtable (EToken _ (TChar _ s)) = return [createQuadruplet ONone "" "" s "" ]
    toTAC symtable (EToken _ (TTrue _)) = return [createQuadruplet ONone "" "" "true" "" ]
    toTAC symtable (EToken _ (TFalse _)) = return [createQuadruplet ONone "" "" "false" "" ]


    -- Identificadores (expresiones)
    toTAC symtable (EIdent _ id) = do
        idTAC <- toTAC symtable id
        return idTAC

    -- Arreglos
    {-toTACArray symtable (EArr _ exp_list) = do
        explist_TAC <- mapM (toTAC symtable) exp_list -- [[TAC]]
        return explist_TAC-}

    -- Control de flujo
toTACFlow :: SymTable -> Exp -> State (Int,Int,String,String,String) [TAC]
    -- Booleanas relacionales
toTACFlow symtable (EGEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OGEq exp1 exp2
toTACFlow symtable (EGreat _ exp1 exp2) = genFlowRelBinOPTAC symtable OGreat exp1 exp2
toTACFlow symtable (ELEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OLEq exp1 exp2
toTACFlow symtable (ELess _ exp1 exp2) = genFlowRelBinOPTAC symtable OLess exp1 exp2
toTACFlow symtable (ENEq _ exp1 exp2) = genFlowRelBinOPTAC symtable ONEq exp1 exp2
toTACFlow symtable (EEqual _ exp1 exp2) = genFlowRelBinOPTAC symtable OEqual exp1 exp2

    -- Booleanas
toTACFlow symtable (EOr _ exp1 exp2) = do  -- E -> E1 or E2
    exp1_t_jump <- getTrueCode   -- E1.true := E.true 
    exp1_f_jump <- genNewLabel   -- E2.false := newlabel
    exp2_t_jump <- getTrueCode   -- E2.true := E.true
    exp2_f_jump <- getFalseCode  -- E2.false := E.false
    setTrueCode exp1_t_jump
    setFalseCode exp1_f_jump
    leftTAC <- toTACFlow symtable exp1
    setTrueCode exp2_t_jump
    setFalseCode exp2_f_jump
    rightTAC <- toTACFlow symtable exp2
    let labelcode = genLabelCode exp1_f_jump
        in return (rightTAC ++ (labelcode : leftTAC))

toTACFlow symtable (EAnd _ exp1 exp2) = do -- E -> E1 and E2
    exp1_t_jump <- genNewLabel  -- E1.true := newlabel
    exp1_f_jump <- getFalseCode -- E1.false := E.false
    exp2_t_jump <- getTrueCode  -- E2.true := E.true
    exp2_f_jump <- getFalseCode -- E.false := E.false
    setTrueCode exp1_t_jump
    setFalseCode exp1_f_jump
    leftTAC <- toTACFlow symtable exp1
    setTrueCode exp2_t_jump
    setFalseCode exp2_f_jump
    rightTAC <- toTACFlow symtable exp2
    let labelcode = genLabelCode exp1_t_jump
        in return (rightTAC ++ (labelcode : leftTAC))

toTACFlow symtable (ENot _ exp) = do -- E -> not E1
    exp_t_jump <- getFalseCode  -- E1.true := E.false
    exp_f_jump <- getTrueCode   -- E1.false := E.true
    setTrueCode exp_t_jump
    setFalseCode exp_f_jump
    expTAC <- toTACFlow symtable exp
    return expTAC

toTACFlow symtable (EToken _ (TTrue _)) = do
    t_jump <- getTrueCode
    return [createQuadruplet OJump t_jump "" "" ""]

toTACFlow symtable (EToken _ (TFalse _)) = do
    f_jump <- getFalseCode
    return [createQuadruplet OJump f_jump "" "" ""]

-- Identificadores
instance TAC_convertible Identifier where
    toTAC symtable (Variable _ (name,scope,_)) -- 'name' contiene el nombre del identificador , 'scope' el alcance asociado 
     | offset == 2 = return [createQuadruplet OIdent "" "" name ""]
     | otherwise = return [createQuadruplet OIdent "" "" (name++"["++(show offset)++"]") ""]
     where offset = lookupTierTable symtable name scope
                
    -- Acceso a arreglos (indices)
    toTAC symtable (Index t id exp)  = do
        idTAC <- toTAC symtable id 
        expTAC <- toTAC symtable exp 
        newlabel1 <- genNewTemp
        newlabel2 <- genNewTemp
        let width = "width" -- falta una forma de obtener la width
            subTAC = createQuadruplet OAssign (getLatestTemp expTAC) "" newlabel1 ""
            offsetTAC = createQuadruplet OMul newlabel2 width newlabel2 "" -- temp := exp * width
            newTAC = createQuadruplet OIdent "" "" ((idString id) ++ "["++newlabel2++"]") "" -- id[temp]
            in return (newTAC : offsetTAC : subTAC : expTAC)

instance TAC_convertible RightValue where
    toTAC symtable (ValueExp exp) = do
        toTAC symtable exp 

{-   toTACArray symtable (ValueExp exp) = do
        toTACArray symtable exp-}

--instance TAC_convertible FCall where
    --toTAC symtable (FCall _ t param_list) = do
    --    param_listTAC <- mapM (toTAC symtable) param_list [[TAC]] -- Lista de TACs de las expresiones correspondientes a los parámetros
    --    let tempList = mapM getLatestTemp param_listTAC  -- Lista de últimos temporales utilizados en cada [TAC] de las expresiones