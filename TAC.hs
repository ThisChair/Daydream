{-|
Module : TAC
Authors : Carlos Infante
          Daniel Varela

Everything concerning the generation and manipulation of Three Address Code
-}
module TAC where

import SymTable
import SyntaxTree
import Lexer
import Control.Monad (forM)
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import Data.List as L

class TAC_convertible a where
    -- Funcion que genera TAC
    toTAC :: SymTable -> a -> State (Int,Int,String,String,String) [TAC] -- State (contador de temporales, contador de etiquetas, S.next, E.true, E.false)

    -- Funcion que genera TAC para estructuras de control de flujo
    toTACFlow :: SymTable -> a -> State (Int,Int,String,String,String) [TAC]

-- Tipo de data para la representacion de TAC en registros
data TAC = Quadruplet { op :: Operator
                      , arg1 :: Maybe String
                      , arg2 :: Maybe String
                      , result :: Maybe String } |
           IfRegister { op :: Operator
                      , arg1 :: Maybe String
                      , arg2 :: Maybe String
                      , truejumplabel :: String
                      , falsejumplabel :: String
                      , result :: Maybe String } | 
           Label { name :: String } deriving (Eq, Show)

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

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------- FUNCIONES PARA MANIPULACION DEL STATE --------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

-- Retorna un nuevo nombre temporal
genNewTemp :: State (Int,Int,String,String,String) String
genNewTemp = state $ \(temp_counter,label_counter,next_code,true_code,false_code) -> ('t' : (show $ temp_counter+1),(temp_counter+1,label_counter,next_code,true_code,false_code))

-- Retorna un nuevo nombre de etiqueta
genNewLabel :: State (Int,Int,String,String,String) String
genNewLabel = state $ \(temp_counter,label_counter,next_code,true_code,false_code) -> ("L" ++ (show $ label_counter+1),(temp_counter,label_counter+1,next_code,true_code,false_code))

-- Retorna la etiqueta correspondiente a S.next actual
getNextCode :: State (Int,Int,String,String,String) String
getNextCode = do
    state <- get
    return (getNextCodeFromStateTuple state)

getNextCodeFromStateTuple :: (Int,Int,String,String,String) -> String
getNextCodeFromStateTuple (_,_,l,_,_) = l 

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

-- Asigna al valor de S.next una string dada
setNextCode :: String -> State (Int,Int,String,String,String) ()
setNextCode string = state $ \(c1,c2,nc,tc,fc) -> ((),(c1,c2,string,tc,fc))

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
convertIfRegistertoString :: Operator -> String -> String -> String -> String -> String -> String 
convertIfRegistertoString o a1 a2 j1 j2 r = ": " ++ "if " ++ cond_string ++ " goto " ++ j1
    where cond_string = a1 ++ " " ++ (show $ o) ++ " " ++ a2
 
-- Convierte una Quadruplet en una String para su pronta impresion
convertQuadrupletoString :: Operator -> String -> String -> String -> String
convertQuadrupletoString o a1 a2 r
 | o == OAssign = ": " ++ r ++ " := " ++ a1
 | ((o == ONone) || (o == OIdent)) = "" 
 | (o == ONeg) || (o == ONot) || (o == OBitNot) = ": " ++ r ++ " = " ++ (show o) ++ " " ++ (a1)
 | (o == OJump) = ": " ++ (show o) ++ " " ++ a1 
 | (o == OPrint) || (o == OPrintLn) = ": " ++ (show o) ++ " " ++ a1
 | otherwise = ": " ++ r ++ " = " ++ a1 ++ " " ++ (show o) ++ " " ++ a2

-- Convierte una Label en una String para su pronta impresion
convertLabeltoString :: String -> String
convertLabeltoString l = l ++ ":"

-- Convierte un TAC en una String para su pronta impresion
convertTACtoString :: TAC -> String
convertTACtoString (IfRegister {op=o,arg1=a1,arg2=a2,truejumplabel=j1,falsejumplabel=j2,result=r}) = let a1' = fromJustString a1
                                                                                                         a2' = fromJustString a2
                                                                                                         r' = fromJustString r
                                                                                                         in convertIfRegistertoString o a1' a2' j1 j2 r'
convertTACtoString (Quadruplet {op=o,arg1=a1,arg2=a2,result=r}) = let a1' = fromJustString a1
                                                                      a2' = fromJustString a2 
                                                                      r' = fromJustString r 
                                                                      in convertQuadrupletoString o a1' a2' r'
convertTACtoString (Label {name=n}) = convertLabeltoString n

-- Imprime una lista de TAC en un formato legible
printTAC :: [TAC] -> IO ()
printTAC tacList = let tacList' = (L.map convertTACtoString . reverse) tacList
                       tacList'' = [x | x <- tacList', x /= ""]
                       in (putStrLn . unlines) tacList''

-- Funcion para obtener la string de un tipo Maybe String
fromJustString :: Maybe String -> String
fromJustString a = case a of
                    Just x -> x
                    Nothing -> ""

-- Funcion para filtrar varios registros TAC innecesarios para la generacion de codigo final,
-- en particular todos aquellos referentes a los nombres de identificadores y tokens.
filterTACList :: [TAC] -> [TAC]
filterTACList tacList = L.filter filterTAC tacList

filterTAC :: TAC -> Bool
filterTAC (Quadruplet op _ _ _) 
 | (op == ONone) || (op == OIdent) = False
 | otherwise = True
filterTAC (IfRegister _ _ _ _ _ _) = True
filterTAC (Label _) = True

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
----------------- FUNCIONES AUXILIARES PARA GENERACION DE TAC ------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

-- Retorna el ultimo temporal utilizado en una lista de TACs
getLatestTemp :: [TAC] -> Maybe String
getLatestTemp tacList = (result . head) tacList

-- Crea una Quadruplet
createQuadruplet :: Operator -> Maybe String -> Maybe String -> Maybe String -> TAC
createQuadruplet o a1 a2 r = Quadruplet {op=o, arg1=a1, arg2=a2, result=r}

-- Crea un IfRegister
createIfRegister :: Operator -> Maybe String -> Maybe String -> String -> String -> Maybe String -> TAC
createIfRegister o a1 a2 j1 j2 r = IfRegister {op=o, arg1=a1, arg2=a2, truejumplabel=j1, falsejumplabel=j2, result=r}

-- Crea un Label
genLabelCode :: String -> TAC
genLabelCode string = Label {name=string}

-- Genera codigo para la incorporacion de saltos a etiquetas
genJumpCode :: String -> TAC
genJumpCode string = Quadruplet {op=OJump,arg1=Just string,arg2=Nothing,result=Nothing}

lookupIdType :: Identifier -> Type
lookupIdType (Variable t _) = t 
lookupIdType (Index t _ _) = t

-- Revisa la tabla de simbolos jerarquica y retorna el offset asociado a las variables
lookupTierTable :: SymTable -> String -> Integer -> Integer
lookupTierTable symtable varname varscope = case M.lookup varname symtable of -- Revisamos el nombre
                                                 Just var_scope_list -> case M.lookup varscope var_scope_list of -- Revisamos el scope
                                                                             Just offset -> scope offset

-- Funcion generica que genera TAC para cualquier operador binario aritmetico
genBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genBinOpTAC symtable bin_op exp1 exp2 = do
    leftTAC <- toTAC symtable exp1 -- TAC de la expresion izquierda
    rightTAC <- toTAC symtable exp2 -- TAC de la expresion derecha
    newlabel <- genNewTemp -- Temporal para almacenar el resultado
    let subTAC = rightTAC ++ leftTAC 
        newTAC = createQuadruplet bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) (Just newlabel)
        in return (newTAC : subTAC)      

-- Funcion generica que genera TAC para cualquier operador relacional binario booleano 
genRelBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genRelBinOpTAC symtable rel_bin_op exp1 exp2 = do
    newlabel <- genNewTemp -- Temporal para albergar el valor booleano
    t_jump <- genNewLabel  -- Etiqueta para saltar en caso de True
    f_jump <- genNewLabel  -- Etiqueta para saltar en caso de False
    leftTAC <- toTAC symtable exp1 -- TAC de la expresion izquierda
    rightTAC <- toTAC symtable exp2 -- TAC de la expresion derecha
    finallabel <- genNewLabel -- Etiqueta para al asignacion final
    let subTAC = rightTAC ++ leftTAC
        jumpcode = genJumpCode finallabel -- Codigo para saltar a la siguiente instruccion
        finallabelcode = genLabelCode finallabel -- Etiqueta de la siguiente instruccion
        truelabelcode = genLabelCode t_jump -- Etiqueta de la instruccion correspondiente al salto en caso True
        falselabelcode = genLabelCode f_jump -- Etiqueta de la instruccion correspondiente al salto en caso False
        true_code = createQuadruplet OAssign (Just "true") Nothing (Just newlabel) -- codigo en caso de True
        false_code = createQuadruplet OAssign (Just "false") Nothing (Just newlabel) -- codigo en caso de False
        newTAC = createIfRegister rel_bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) t_jump f_jump (Just newlabel)
        in return (finallabelcode : jumpcode : false_code : falselabelcode : jumpcode : true_code : truelabelcode : newTAC : subTAC)

-- Funcion generica que genera TAC para cualquier operador unario
genUnOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> State (Int,Int,String,String,String) [TAC]
genUnOpTAC symtable un_op exp = do
    expTAC <- toTAC symtable exp -- TAC de la expresion
    newlabel <- genNewTemp -- Temporal para almacenar el resultado
    let newTAC = createQuadruplet un_op (getLatestTemp expTAC) Nothing (Just newlabel)
        in return (newTAC : expTAC)

-- Funcion generica que genera TAC de control de flujo para cualquier operador relacional binario 
genFlowRelBinOPTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String) [TAC]
genFlowRelBinOPTAC symtable rel_bin_op exp1 exp2 = do
    t_jump <- getTrueCode -- Etiqueta para saltar en caso de True
    f_jump <- getFalseCode -- Etiqueta para saltar en caso de False
    leftTAC <- toTACFlow symtable exp1 -- TAC de la expresion izquierda
    rightTAC <- toTACFlow symtable exp2 -- TAC de la expresion derehca
    let subTAC = rightTAC ++ leftTAC
        id1 = getLatestTemp leftTAC 
        id2 = getLatestTemp rightTAC
        falsejump = genJumpCode f_jump
        newTAC = createIfRegister rel_bin_op id1 id2 t_jump f_jump Nothing -- Instruccion vacia, control de flujo solo necesita las etiquetas a donde saltar, no el codigo
        in return (falsejump : newTAC : subTAC)

-- Funcion para generacion de TAC para la lista de expresiones de un arreglo
toTACArray :: SymTable -> RightValue -> State (Int,Int,String,String,String) [[TAC]]
toTACArray symtable (ValueExp (EArr _ exp_list)) = do
    mapM (toTAC symtable) exp_list -- TAC de la lista de expresiones del arreglo :: [[TAC]] -- AQUI HAY UN BETA CON LOS TIPOS NO BÁSICOS

-- Funcion para generacion de TAC para asignaciones de arreglos
genArrayAssignTAC :: SymTable -> [Identifier] -> [RightValue] -> State (Int,Int,String,String,String) [TAC]
genArrayAssignTAC symtable id_list rv_list = do
    rvTAC <- mapM (toTACArray symtable) rv_list -- lista de [[TAC]], correspondiente a cada arreglo :: [[[TAC]]]
    idTAC <- mapM (toTAC symtable) id_list -- lista de [TAC] de los identificadores : [[TAC]]
    let idTAClist = concat idTAC
        idStringlist = [ (fromJustString . result) x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
        assignmentTuples = reverse (zip idStringlist rvTAC) -- :: [(id,[[TAC]])]
        in do
           newTAC <- mapM genArrayTAC assignmentTuples -- lista de TAC 'id[0],..,id[n] := temp_0,..,temp_n :: [TAC]
           let newTAC' = concat [ fst x ++ snd x | x <- (zip newTAC (reverse idTAC)) ] -- :: [ [TAC] | (TAC,[TAC]) ]
               in return newTAC' 

-- Función auxiliar para generacion de TAC para asignaciones de arreglos, genera las asignaciones en sí
genArrayTAC :: (String,[[TAC]]) -> State (Int,Int,String,String,String) [TAC]
genArrayTAC (id,array) = let id_decomposition = [ id ++ "[" ++ (show x) ++ "]" | x <- [0..length array] ] -- Lista de accesos indexados al identificador, id[0]..id[n]
                             tempList = L.map getLatestTemp array
                             assigments = [ createQuadruplet OAssign (fst x) Nothing (Just (snd x)) | x <- (zip tempList id_decomposition) ] -- Lista de asignaciones id[0],..,id[n] := temp0,..,tempn :: [TAC]
                             assigments' = concat $ reverse [ fst x : snd x | x <- (zip assigments array) ] -- [ [TAC] | (TAC,[TAC])]
                             in return (assigments') 

-- Funcion para generacion de TAC para asignaciones comunes, ex: id := token, id := id
genNormalAssignTAC :: SymTable -> [Identifier] -> [RightValue] -> State (Int,Int,String,String,String) [TAC]
genNormalAssignTAC symtable id_list rv_list = do
    rvTAC <- mapM (toTAC symtable) rv_list -- lista de [TAC] de los right value :: [[TAC]]
    idTAC <- mapM (toTAC symtable) id_list -- lista de [TAC] de los identificadores :: [[TAC]]
    let rvTemplist = L.map getLatestTemp rvTAC -- lista de temporales finales de los right value a ser asignados a los id
        idTAClist = concat idTAC
        idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
        assignmentTuples = reverse (zip rvTemplist idStringlist) -- tuplas con las asignaciones correspondientes [(rv_temp,id)]
        newTAC = [ createQuadruplet OAssign (fst x) Nothing (snd x) | x <- assignmentTuples ] -- lista de TAC 'id := rv_temp' :: [TAC]
        newTAC' = [ fst x : snd x | x <- (zip newTAC (reverse idTAC)) ] -- concatenamos la lista de asignaciones con el codigo auxiliar de cada id, en caso de existir :: [ [TAC] | ( TAC,[TAC]) ]
        finalTAC = concat [ fst x ++ snd x | x <- (zip newTAC' (reverse rvTAC)) ] -- finalmente concatenamos el codigo auxiliar de cada right value :: [ [TAC] | ([TAC],[TAC]) ]
        in return finalTAC

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
        nextcode <- genNewLabel -- Caso base para S.next
        setNextCode nextcode
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de la lista de instrucciones del programa
        return (concat $ reverse insTAC)
  
-- Instrucciones
instance TAC_convertible Instruction where
    -- Bloques
    toTAC symtable (Block _ ins_list) = do
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de las instrucciones
        return (concat insTAC)

    -- Asignaciones
    {-toTAC symtable (Assign _ (id_list,vr_list)) = do
        vrTAC <- mapM (toTACArray symtable) vr_list -- lista de [[TAC]] de los right value
        idTAC <- mapM (toTAC symtable) id_list -- lista de TAC de los identificadores
        let idTAClist = concat idTAC
            idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
            assignmentTuples = reverse (zip idStringlist vrTAC) -- tripletas con las asignaciones correspondientes [(id,[[TAC]])]
            in do
                array_assignments_list <- mapM genArrayTAC assignmentTuples -- lista de asignaciones id := array :: [[TAC]]
                return (concat array_assignments_list)-}

    toTAC symtable (Assign _ (id_list,rv_list)) = let rv_list_sample = head rv_list
                                                      in case rv_list_sample of
                                                              (ValueExp (EArr _ _ )) -> genArrayAssignTAC symtable id_list rv_list
                                                              (ValueExp _) -> genNormalAssignTAC symtable id_list rv_list

    -- Selectores 
    -- If then
    toTAC symtable (IfThen _ exp ins) = do -- S -> if E then S1
        nextcode <- getNextCode -- El nextcode de esta instruccion
        exp_f_jump <- getNextCode -- E.false := S.next
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- TAC de la expresion condicional
        insTAC <- toTAC symtable ins -- S1.next := S1.next, TAC de la instruccion
        nextcode' <- genNewLabel
        setNextCode nextcode'
        let labelcode = genLabelCode exp_t_jump -- Etiqueta de la instruccion a ejecutar
            labelcodefinal = genLabelCode nextcode -- Etiqueta de la instruccion siguiente al if
            in return (labelcodefinal : (insTAC ++ (labelcode : expTAC)))

    -- If then Else
    toTAC symtable (IfElse _ exp ins1 ins2) = do -- S -> if E then S1 else S2
        nextcode <- getNextCode -- El nextcode de esta instruccion
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- genNewLabel -- E.false := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- TAC de la expresion condicional
        ins_list1TAC <- toTAC symtable ins1 -- TAC de la instruccion 1
        ins_list2TAC <- toTAC symtable ins2 -- TAC de la instruccion 2
        nextcode' <- genNewLabel
        setNextCode nextcode'
        let labelcode1 = genLabelCode exp_t_jump -- Etiqueta de la instruccion 1
            labelcode2 = genLabelCode exp_f_jump -- Etiqueta de la instruccion 2
            jumpcode = genJumpCode nextcode -- Codigo para saltar a la siguiente instruccion
            labelcodefinal = genLabelCode nextcode -- Etiqueta de la siguiente instruccion
            in return (labelcodefinal : (ins_list2TAC ++ (labelcode2 : jumpcode : ins_list1TAC) ++ (labelcode1 : expTAC)))

    -- Iteradores indeterminados
    -- While
    toTAC symtable (While _ exp ins) = do -- S -> while E do S1
        nextcode <- getNextCode -- El next de esta instruccion
        begin <- genNewLabel -- S.begin = newlabel(), header del iterador
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- getNextCode -- E.false := S.next
        setNextCode begin -- S1.next := S.begin
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- TAC de la expresion condicional
        insTAC <- toTAC symtable ins -- TAC de la instruccion
        nextcode' <- genNewLabel
        setNextCode nextcode'
        let labelcodeheader = genLabelCode begin -- Etiqueta del header
            labelcode1 = genLabelCode exp_t_jump -- Etiqueta de la instruccion 1
            jumpcode = genJumpCode begin -- Codigo para saltar devuelta al header
            labelcodefinal = genLabelCode nextcode -- Etiqueta de la siguiente instruccion
            in return (labelcodefinal : (jumpcode : (insTAC ++ (labelcode1 : reverse (labelcodeheader : reverse expTAC)))))

    -- Iteradores determinados 
    -- For
    toTAC symtable (Det _ for) = do 
        forTAC <- toTAC symtable for -- TAC del ciclo determinado
        return forTAC

    -- Prints
    -- Print normal
    toTAC symtable (Print _ exp) = do 
        nextcode <- getNextCode -- El next de esta instruccion
        expTAC <- toTAC symtable exp -- TAC de la expresion a imprimir
        let newTAC = createQuadruplet OPrint (getLatestTemp expTAC) Nothing Nothing
            labelcode = genLabelCode nextcode
            in return (labelcode : newTAC : expTAC)

    -- Print new line
    toTAC symtable (PrintLn _ exp) = do
        nextcode <- getNextCode -- El next de esta instruccion
        expTAC <- toTAC symtable exp -- TAC de la expresion a imprimir
        let newTAC = createQuadruplet OPrintLn (getLatestTemp expTAC) Nothing Nothing
            labelcode = genLabelCode nextcode
            in return (labelcode : newTAC : expTAC)
 
-- Iteradores determinados
instance TAC_convertible For where

    -- For-From-To
    {-toTAC symtable (FromTo _ begin end ins) = do -- S -> for ID from E1 to E2 do S1
        nextcode <- getNextCode -- El next de esta instruccion
        begin <- genNewLabel -- S.begin = newlabel(), header del iterador
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- getNextCode -- E.false := S.next
        setNextCode begin -- S1.next := S.begin
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        beginTAC <- toTAC symtable begin -- TAC de la expresion de inicio
        endTAC <- toTAC symtable end -- TAC de la expresion de culminacion
        insTAC <- toTAC symtable ins -- TAC de la instruccion
        let iterator_var = createQuadruplet OAssign (getLatestTemp beginTAC) Nothing (Just "iter_var")-}

-- Expresiones
instance TAC_convertible Exp where
    -- Binarias
    -- Suma (+)
    toTAC symtable (ESum _ exp1 exp2) = genBinOpTAC symtable OSum exp1 exp2 
    -- Resta (-)
    toTAC symtable (EDif _ exp1 exp2) = genBinOpTAC symtable ODif exp1 exp2
    -- Multiplicacion (*)
    toTAC symtable (EMul _ exp1 exp2) = genBinOpTAC symtable OMul exp1 exp2
    -- Division (/)
    toTAC symtable (EDiv _ exp1 exp2) = genBinOpTAC symtable ODiv exp1 exp2
    -- Modulo (%)
    toTAC symtable (EMod _ exp1 exp2) = genBinOpTAC symtable OMod exp1 exp2
    -- Potencia (**)
    toTAC symtable (EPot _ exp1 exp2 ) = genBinOpTAC symtable OPot exp1 exp2
    -- Division entera (//)
    toTAC symtable (EDivE _ exp1 exp2 ) = genBinOpTAC symtable ODivE exp1 exp2
    -- Shift izquierdo (<<)
    toTAC symtable (ELShift _ exp1 exp2 ) = genBinOpTAC symtable OLShift exp1 exp2
    -- Shift derecho (>>)
    toTAC symtable (ERShift _ exp1 exp2 ) = genBinOpTAC symtable ORShift exp1 exp2
    -- Disyuncion bit a bit (|)
    toTAC symtable (EBitOr _ exp1 exp2 ) = genBinOpTAC symtable OBitOr exp1 exp2
    -- Conjuncion bit a bit (&)
    toTAC symtable (EBitAnd _ exp1 exp2 ) = genBinOpTAC symtable OBitAnd exp1 exp2
    -- XOR bit a bit(^)
    toTAC symtable (EBitXor _ exp1 exp2 ) = genBinOpTAC symtable OBitXor exp1 exp2
    -- Disjuncion logica (||)
    toTAC symtable (EOr _ exp1 exp2 ) = genBinOpTAC symtable OOr exp1 exp2
    -- Conjuncion logica (&&)
    toTAC symtable (EAnd _ exp1 exp2 ) = genBinOpTAC symtable OAnd exp1 exp2

    -- Booleanas relacionales aritmeticas
    -- Mayor o igual (>=)
    toTAC symtable (EGEq _ exp1 exp2 ) = genRelBinOpTAC symtable OGEq exp1 exp2
    -- Mayor (>)
    toTAC symtable (EGreat _ exp1 exp2 ) = genRelBinOpTAC symtable OGreat exp1 exp2
    -- Menor o igual (<=)
    toTAC symtable (ELEq _ exp1 exp2 ) = genRelBinOpTAC symtable OLEq exp1 exp2
    -- Less (<)
    toTAC symtable (ELess _ exp1 exp2 ) = genRelBinOpTAC symtable OLess exp1 exp2
    -- No igual (/=)
    toTAC symtable (ENEq _ exp1 exp2 ) = genRelBinOpTAC symtable ONEq exp1 exp2
    -- Igual (==)
    toTAC symtable (EEqual _ exp1 exp2 ) = genRelBinOpTAC symtable OEqual exp1 exp2

    -- Unarias
    -- Negacion aritmetica (-)
    toTAC symtable (ENeg _ exp ) = genUnOpTAC symtable ONeg exp
    -- Negacion logica (uminus)
    toTAC symtable (ENot _ exp ) = genUnOpTAC symtable ONot exp 
    -- Negacion bit a bit (~)
    toTAC symtable (EBitNot _ exp ) = genUnOpTAC symtable OBitNot exp 

    -- Tokens
    -- Numericos
    toTAC symtable (EToken _ (TNum _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]
    -- Strings
    toTAC symtable (EToken _ (TString _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]
    -- Caracteres
    toTAC symtable (EToken _ (TChar _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]
    -- Booleanos
    toTAC symtable (EToken _ (TTrue _)) = return [createQuadruplet ONone Nothing Nothing (Just "true")]
    toTAC symtable (EToken _ (TFalse _)) = return [createQuadruplet ONone Nothing Nothing (Just "false")]

    -- Identificadores (vistos como expresiones)
    toTAC symtable (EIdent _ id) = do
        idTAC <- toTAC symtable id
        return idTAC

    -- Arreglos
    -- Revisar la funcion auxiliar toTACArray

    -------------------------------------------
    -------------------------------------------
    ----- Funciones para Control de flujo -----
    -------------------------------------------
    -------------------------------------------

    -- Binarias
    -- Suma (+)
    toTACFlow symtable (ESum _ exp1 exp2) = genBinOpTAC symtable OSum exp1 exp2 
    -- Resta (-)
    toTACFlow symtable (EDif _ exp1 exp2) = genBinOpTAC symtable ODif exp1 exp2
    -- Multiplicacion (*)
    toTACFlow symtable (EMul _ exp1 exp2) = genBinOpTAC symtable OMul exp1 exp2
    -- Division (/)
    toTACFlow symtable (EDiv _ exp1 exp2) = genBinOpTAC symtable ODiv exp1 exp2
    -- Modulo (%)
    toTACFlow symtable (EMod _ exp1 exp2) = genBinOpTAC symtable OMod exp1 exp2
    -- Potencia (**)
    toTACFlow symtable (EPot _ exp1 exp2 ) = genBinOpTAC symtable OPot exp1 exp2
    -- Division entera (//)
    toTACFlow symtable (EDivE _ exp1 exp2 ) = genBinOpTAC symtable ODivE exp1 exp2
    -- Shift izquierdo (<<)
    toTACFlow symtable (ELShift _ exp1 exp2 ) = genBinOpTAC symtable OLShift exp1 exp2
    -- Shift derecho (>>)
    toTACFlow symtable (ERShift _ exp1 exp2 ) = genBinOpTAC symtable ORShift exp1 exp2
    -- Disyuncion bit a bit (|)
    toTACFlow symtable (EBitOr _ exp1 exp2 ) = genBinOpTAC symtable OBitOr exp1 exp2
    -- Conjuncion bit a bit (&)
    toTACFlow symtable (EBitAnd _ exp1 exp2 ) = genBinOpTAC symtable OBitAnd exp1 exp2
    -- XOR bit a bit(^)
    toTACFlow symtable (EBitXor _ exp1 exp2 ) = genBinOpTAC symtable OBitXor exp1 exp2

    -- Tokens (ningun cambio con respecto a los normales)
    -- Numericos
    toTACFlow symtable (EToken _ (TNum _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]
    -- Strings
    toTACFlow symtable (EToken _ (TString _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]
    -- Caracteres
    toTACFlow symtable (EToken _ (TChar _ s)) = return [createQuadruplet ONone Nothing Nothing (Just s)]

    -- Identificadores booleanos (vistos como expresiones)
    toTACFlow symtable (EIdent TypeBool id) = do -- E -> id (true,false)
        t_jump <- getTrueCode
        f_jump <- getFalseCode
        idTAC <- toTAC symtable id
        let falsejump = genJumpCode f_jump
            newTAC = createIfRegister OEqual (getLatestTemp idTAC) (Just "true") t_jump f_jump Nothing -- 'if ID then', se traduce en 'if ID == true then'
            in return (falsejump : newTAC : idTAC)

    -- Identificadores no booleanos (vistos como expresiones) -- E -> id
    toTACFlow symtable (EIdent _ id) = do 
        idTAC <- toTAC symtable id
        return idTAC

    -- Booleanas relacionales
    -- Mayor o igual (>=)
    toTACFlow symtable (EGEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OGEq exp1 exp2
    -- Mayor (>)
    toTACFlow symtable (EGreat _ exp1 exp2) = genFlowRelBinOPTAC symtable OGreat exp1 exp2
    -- Menor o igual (<=)
    toTACFlow symtable (ELEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OLEq exp1 exp2
    -- Menor (<)
    toTACFlow symtable (ELess _ exp1 exp2) = genFlowRelBinOPTAC symtable OLess exp1 exp2

    -- Desigualdad (/=)
    toTACFlow symtable (ENEq _ exp1 exp2) = do -- E -> E1 /= E2
        t_jump <- getTrueCode 
        f_jump <- getFalseCode
        exp1_t_jump <- genNewLabel -- E1.true := newlabel
        exp2_t_jump <- genNewLabel -- E2.true := newlabel
        setTrueCode exp1_t_jump    
        setFalseCode exp1_t_jump   -- E1.false := E1.true
        leftTAC <- toTAC symtable exp1
        setTrueCode exp2_t_jump  
        setFalseCode exp2_t_jump   -- E2.false := E1.true
        rightTAC <- toTAC symtable exp2
        let subTAC = rightTAC ++ leftTAC
            falsejump = genJumpCode f_jump
            newTAC = createIfRegister ONEq (getLatestTemp leftTAC) (getLatestTemp rightTAC) t_jump f_jump Nothing
            in return (falsejump : newTAC : subTAC)

    -- Igualdad (==)
    toTACFlow symtable (EEqual _ exp1 exp2) = do -- E -> E1 == E2
        t_jump <- getTrueCode 
        f_jump <- getFalseCode
        exp1_t_jump <- genNewLabel -- E1.true := newlabel
        exp2_t_jump <- genNewLabel -- E2.true := newlabel
        setTrueCode exp1_t_jump    
        setFalseCode exp1_t_jump   -- E1.false := E1.true
        leftTAC <- toTAC symtable exp1
        setTrueCode exp2_t_jump  
        setFalseCode exp2_t_jump   -- E2.false := E1.true
        rightTAC <- toTAC symtable exp2
        let subTAC = rightTAC ++ leftTAC
            falsejump = genJumpCode f_jump
            newTAC = createIfRegister OEqual (getLatestTemp leftTAC) (getLatestTemp rightTAC) t_jump f_jump Nothing
            in return (falsejump : newTAC : subTAC)

    -- Booleanas
    -- Disjuncion (||)
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

    -- Conjuncion (&&)
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

    -- Negacion (!)
    toTACFlow symtable (ENot _ exp) = do -- E -> not E1
        exp_t_jump <- getFalseCode  -- E1.true := E.false
        exp_f_jump <- getTrueCode   -- E1.false := E.true
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp
        return expTAC

    -- True
    toTACFlow symtable (EToken _ (TTrue _)) = do -- E -> true
        t_jump <- getTrueCode
        return [createQuadruplet OJump (Just t_jump) Nothing Nothing]

    -- False
    toTACFlow symtable (EToken _ (TFalse _)) = do -- E -> false
        f_jump <- getFalseCode
        return [createQuadruplet OJump (Just f_jump) Nothing Nothing]

-- Identificadores
instance TAC_convertible Identifier where
    -- Nombres de identificadores
    toTAC symtable (Variable _ (name,scope,_)) -- 'name' contiene el nombre del identificador , 'scope' el alcance asociado 
     | offset == 2 = return [createQuadruplet OIdent Nothing Nothing (Just name)]
     | otherwise = return [createQuadruplet OIdent Nothing Nothing (Just (name++"["++(show offset)++"]"))]
     where offset = lookupTierTable symtable name scope
                
    -- Acceso a arreglos (indices)
    toTAC symtable (Index t id exp)  = do
        idTAC <- toTAC symtable id 
        expTAC <- toTAC symtable exp 
        newlabel1 <- genNewTemp
        newlabel2 <- genNewTemp
        let width = "width" -- falta una forma de obtener la width
            subTAC = createQuadruplet OAssign (getLatestTemp expTAC) Nothing (Just newlabel1)
            offsetTAC = createQuadruplet OMul (Just newlabel1) (Just width) (Just newlabel2) -- temp := exp * width
            newTAC = createQuadruplet OIdent Nothing Nothing (Just ((idString id) ++ "["++newlabel2++"]")) -- id[temp]
            in return (newTAC : offsetTAC : subTAC : expTAC)

instance TAC_convertible RightValue where
    toTAC symtable (ValueExp exp) = do
        toTAC symtable exp 

--instance TAC_convertible FCall where
    --toTAC symtable (FCall _ t param_list) = do
    --    param_listTAC <- mapM (toTAC symtable) param_list [[TAC]] -- Lista de TACs de las expresiones correspondientes a los parámetros
    --    let tempList = mapM getLatestTemp param_listTAC  -- Lista de últimos temporales utilizados en cada [TAC] de las expresiones