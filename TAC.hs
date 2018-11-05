module TAC where

import SymTable
import SyntaxTree
import Lexer
import Control.Monad.Trans.State.Lazy
import Control.Monad (forM)
import Data.Map as M
import Data.List as L

class TAC_convertible a where
    toTAC :: SymTable -> a -> State (Int,Int,String,String) [TAC]

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
                      , label :: String } deriving (Show)

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
                OIndex |
                OJump |
                OIdent |       
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
    show OIndex = "[]="
    show OJump = "goto"

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

-- Retorna un nuevo nombre temporal
genNewTemp :: State (Int,Int,String,String) String
genNewTemp = state $ \(temp_counter,label_counter,true_code,false_code) -> ('t' : (show $ temp_counter+1),(temp_counter+1,label_counter,true_code,false_code))

genNewLabel :: State (Int,Int,String,String) String
genNewLabel = state $ \(temp_counter,label_counter,true_code,false_code) -> ("L" ++ (show $ label_counter+1),(temp_counter,label_counter+1,true_code,false_code))

-- Retorna el ultimo temporal utilizado en una lista de TACs
getLatestTemp :: [TAC] -> String
getLatestTemp tacList = (result . head) tacList

-- Genera codigo para la incorporacion de etiquetas
genLabelCode :: String -> TAC
genLabelCode string = Quadruplet {op=OJump,arg1=string,arg2="",result="",label=""}

-- Retorna el nombre de la etiqueta actual
getCurrentLabel :: State (Int,Int,String,String) String
getCurrentLabel = do
    state <- get
    let label_counter = getLabelFromStateTuple state
        in return ("L" ++ label_counter)

getLabelFromStateTuple :: (Int,Int,String,String) -> String
getLabelFromStateTuple (_,l,_,_) = show l

setCurrentLabel :: String -> State (Int,Int,String,String) ()
setCurrentLabel string = state $ \(c1,c2,tc,fc) -> ((),(c1,read $ tail string :: Int,tc,fc))

-- Retorna la etiqueta correspondiente a E.true actual
getTrueCode :: State (Int,Int,String,String) String
getTrueCode = do
    state <- get
    return (getTrueCodeFromStateTuple state)

getTrueCodeFromStateTuple :: (Int,Int,String,String) -> String
getTrueCodeFromStateTuple (_,_,t,_) = t

-- Retorna la etiqueta correspondiente a E.false actual
getFalseCode :: State (Int,Int,String,String) String
getFalseCode = do
    state <- get
    return (getFalseCodeFromStateTuple state)

getFalseCodeFromStateTuple :: (Int,Int,String,String) -> String
getFalseCodeFromStateTuple (_,_,_,f) = f

-- Asigna al valor de E.true una string dada
setTrueCode :: String -> State (Int,Int,String,String) ()
setTrueCode string = state $ \(c1,c2,tc,fc) -> ((),(c1,c2,string,fc))

-- Asigna al valor de E.false una string dada
setFalseCode :: String -> State (Int,Int,String,String) ()
setFalseCode string = state $ \(c1,c2,tc,fc) -> ((),(c1,c2,tc,string))

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
 | ((o == ONone) && (l == "")) || (o == OIdent) = "" 
 | (o == ONone) && (l /= "") = l ++ ": " ++ (show o)
 | o == OAssign = l ++ ": " ++ r ++ " := " ++ a1
 | (o == ONeg) || (o == ONot) || (o == OBitNot) = l ++ ": " ++ r ++ " = " ++ (show o) ++ " " ++ (a1)
 | (o == OJump) = l ++ ": " ++ (show o) ++ " " ++ a1 
 | (o == OIndex) = l ++ ": " ++ r ++ " " ++ (show o) ++ " " ++ a1
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
setInsLabel tacList = let root = head tacList
                          rootlabel = (label . head) tacList
                          top = last tacList
                          newtop = setlabel rootlabel top
                          body = (tail . reverse . tail) tacList
                          newroot = setlabel "" root
                          in (newroot : reverse (newtop : body))
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
genBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String) [TAC]
genBinOpTAC symtable bin_op exp1 exp2 = do
    leftTAC <- toTAC symtable exp1
    rightTAC <- toTAC symtable exp2
    newlabel <- genNewTemp
    let subTAC = rightTAC ++ leftTAC
        newTAC = createQuadruplet bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) newlabel ""
        in return (newTAC : subTAC)      

-- Funcion generica que genera TAC para cualquier operador binario booleano
genRelBinOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String) [TAC]
genRelBinOpTAC symtable rel_bin_op exp1 exp2 = do
    nextcode <- getCurrentLabel
    newlabel <- genNewTemp
    t_jump <- genNewLabel
    f_jump <- genNewLabel
    leftTAC <- toTAC symtable exp1 
    rightTAC <- toTAC symtable exp2
    nextcode' <- genNewLabel
    let subTAC = rightTAC ++ leftTAC
        labelcode = genLabelCode nextcode'
        c = createQuadruplet rel_bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) "" "" 
        true_code = [labelcode,createQuadruplet OAssign "1" "" newlabel t_jump ]
        false_code = [labelcode,createQuadruplet OAssign "0" "" newlabel f_jump ]
        newTAC = createIfRegister c t_jump true_code f_jump false_code newlabel nextcode
        in return (newTAC : subTAC)

-- Funcion generica que genera TAC para cualquier operador unario
genUnOpTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> State (Int,Int,String,String) [TAC]
genUnOpTAC symtable un_op exp = do
    expTAC <- toTAC symtable exp
    currentLabel <- getCurrentLabel 
    newlabel <- genNewTemp
    let newTAC = createQuadruplet un_op (getLatestTemp expTAC) "" newlabel currentLabel
        in return (newTAC : expTAC)

-- Funcion generica que genera TAC de control de flujo para cualquier operador relacional binario
genFlowRelBinOPTAC :: (TAC_convertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String) [TAC]
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

----------------------------------------------------------------------------
----------------------------------------------------------------------------
--------------------- FUNCIONES PARA GENERACION DE TAC ---------------------
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- Inicio del arbol sintactico
instance TAC_convertible Init where
   toTAC symtable (Init _ _ _ ins_list) = do
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de la lista de instrucciones del programa
        finalLabel <- getCurrentLabel
        let finalTAC = (createQuadruplet ONone "" "" "" finalLabel) : (concat $ reverse insTAC)
            in return finalTAC
  
-- Instrucciones
instance TAC_convertible Instruction where
    -- Bloques
    toTAC symtable (Block _ ins_list) = do
        insTAC <- mapM (toTAC symtable) ins_list -- lista de TACs de las instrucciones
        return (concat $ reverse insTAC)

    -- Asignaciones
    toTAC symtable (Assign _ (id_list,vr_list)) = do
        label <- getCurrentLabel -- El next code de esta instruccion
        vrTAC <- mapM (toTAC symtable) vr_list -- lista de TACs de los right value 
        idTAC <- mapM (toTAC symtable) id_list -- lista de TACs de los identificadores
        labellist <- mapM (\x -> genNewLabel) [1..((length $ concat idTAC))-1]
        nextcode <- genNewLabel -- El next code de la instruccion por venir
        let labellist' = label : labellist
            labellist'' = (tail . reverse ) (nextcode : (reverse labellist'))
            vrTAClist = (concat . reverse) vrTAC 
            vrTemplist = L.map getLatestTemp vrTAC -- lista de temporales finales de los rv a ser asignados a los id
            vrTAClenght = L.map length vrTAC -- lista de tamaños de las derivaciones de los rv
            idTAClist = concat idTAC -- -- lista de id
            idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- lista de nombres de los identificadores
            assignmentTuples = reverse (zip4 idStringlist vrTemplist vrTAClenght labellist') -- tripletas con las asignaciones correspondientes (id,rv_temp,length vrTAC,label)
            newTAC = [ createQuadruplet OAssign (snd' x) "" (fst' x) (fth' x) | x <- assignmentTuples ] -- lista de TACs 'id := rv_temp' [TAC]
            labelcodelist = L.map genLabelCode (labellist'') -- codigo para ir a la siguiente instruccion de cada asignación [TAC]
            newTAC' = [ reverse (fst x : [snd x]) | x <- (zip newTAC (reverse labelcodelist)) ] -- [ [TAC] | (TAC,TAC) ] -> [[TAC]]
            finalTAC = concat $ L.map setInsLabel [ fst x ++ snd x | x <- (zip newTAC' (reverse vrTAC)) ] -- concat [ [TAC] | ([TAC],[TAC]) ]
            in return (idTAClist ++ finalTAC)

    -- Selectores 
    toTAC symtable (IfThen _ exp ins) = do -- S -> if E then S1
        label <- getCurrentLabel -- El label de esta instruccion
        exp_t_jump <- genNewLabel     -- E.true := newlabel
        nextcode <- genNewLabel  -- El nextcode de esta instruccion
        exp_f_jump <- getCurrentLabel -- E.false := S.next
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp
        setCurrentLabel exp_t_jump    -- S1.next := S.next
        insTAC <- toTAC symtable ins
        --newTAC = createIfRegister c exp_t_jump insTAC nextcode' [createQuadruplet ONone "" "" "" ""] "" nextcode
        return (insTAC ++ expTAC)

    --toTAC symtable (IfElse _ )

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

    -- Arreglos
    --toTAC symtable (EArr t exp_list) =

    -- Identificadores (expresiones)
    toTAC symtable (EIdent _ id) = do
        idTAC <- toTAC symtable id
        return idTAC

    -- Control de flujo
toTACFlow :: SymTable -> Exp -> State (Int,Int,String,String) [TAC]
    -- Booleanas relacionales
toTACFlow symtable (EGEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OGEq exp1 exp2
toTACFlow symtable (EGreat _ exp1 exp2) = genFlowRelBinOPTAC symtable OGreat exp1 exp2
toTACFlow symtable (ELEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OLEq exp1 exp2
toTACFlow symtable (ELess _ exp1 exp2) = genFlowRelBinOPTAC symtable OLess exp1 exp2
toTACFlow symtable (ENEq _ exp1 exp2) = genFlowRelBinOPTAC symtable ONEq exp1 exp2
toTACFlow symtable (EEqual _ exp1 exp2) = genFlowRelBinOPTAC symtable OEqual exp1 exp2

    -- Booleanas
toTACFlow symtable (EOr _ exp1 exp2) = do  -- E -> E1 or E2
    label <- getCurrentLabel -- El label de esta instruccion
    exp1_t_jump <- getTrueCode   -- E1.true := E.true 
    exp1_f_jump <- genNewLabel   -- E2.false := newlabel
    exp2_t_jump <- getTrueCode   -- E2.true := E.true
    exp2_f_jump <- getFalseCode  -- E2.false := E.false
    setTrueCode exp1_t_jump
    setFalseCode exp1_f_jump
    leftTAC <- toTACFlow symtable exp1
    setTrueCode exp2_t_jump
    setFalseCode exp2_f_jump
    setCurrentLabel exp1_f_jump
    rightTAC <- toTACFlow symtable exp2
    return (rightTAC ++ leftTAC)

toTACFlow symtable (EAnd _ exp1 exp2) = do -- E -> E1 and E2
    label <- getCurrentLabel -- El label de esta instruccion
    exp1_t_jump <- genNewLabel  -- E1.true := newlabel
    exp1_f_jump <- getFalseCode -- E1.false := E.false
    exp2_t_jump <- getTrueCode  -- E2.true := E.true
    exp2_f_jump <- getFalseCode -- E.false := E.false
    setTrueCode exp1_t_jump
    setFalseCode exp1_f_jump
    leftTAC <- toTACFlow symtable exp1
    setTrueCode exp2_t_jump
    setFalseCode exp2_f_jump
    setCurrentLabel exp1_t_jump
    rightTAC <- toTACFlow symtable exp2
    return (rightTAC ++ leftTAC)

toTACFlow symtable (ENot _ exp) = do -- E -> not E1
    label <- getCurrentLabel -- El label de esta instruccion
    exp_t_jump <- getFalseCode  -- E1.true := E.false
    exp_f_jump <- getTrueCode   -- E1.false := E.true
    setTrueCode exp_t_jump
    setFalseCode exp_f_jump
    expTAC <- toTACFlow symtable exp
    return expTAC

toTACFlow symtable (EToken _ (TTrue _)) = do
    label <- getCurrentLabel
    t_jump <- getTrueCode
    return [createQuadruplet OJump t_jump "" "" ""]

toTACFlow symtable (EToken _ (TFalse _)) = do
    label <- getCurrentLabel
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
        label <- getCurrentLabel
        idTAC <- toTAC symtable id 
        expTAC <- toTAC symtable exp 
        newlabel1 <- genNewTemp
        let width = "width" -- falta una forma de obtener la width
            tac0 = createQuadruplet OMul (getLatestTemp expTAC) width newlabel1 ""  -- temp1 = i * width
            tac1 = createQuadruplet OIndex newlabel1 "" (idString id) label  -- x []= temp1
        return (tac1 : tac0 : expTAC)

instance TAC_convertible RightValue where
    toTAC symtable (ValueExp exp) = do
        toTAC symtable exp 
