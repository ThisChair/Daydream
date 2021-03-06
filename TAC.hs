{-|
Module : TAC
Authors : Carlos Infante
          Daniel Varela

Everything concerning the generation and manipulation of Three Address Code.
-}
module TAC where

import SymTable
import SyntaxTree
import Lexer
import Control.Monad (forM)
import Control.Monad.Trans.State.Lazy
import Data.Map as M
import Data.List as L
import Data.Maybe (fromMaybe)

class TACConvertible a where
    -- Main TAC generator function.
    toTAC :: SymTable -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC] -- State Monad (temporals counter, labels counter, S.next, E.true, E.false, width table).

    -- Generates special TAC specifically for Flow-Control structures.
    toTACFlow :: SymTable -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC]

-- Data type for register-like TAC representation.
data TAC = Quadruplet { op :: Operator -- Operator used.
                      , arg1 :: Maybe TACField -- Operator's left-side argument.
                      , arg2 :: Maybe TACField -- Operator's right-side argument.
                      , result :: Maybe TACField } | -- Result of the operation.
           IfRegister { op :: Operator -- Operator used.
                      , arg1 :: Maybe TACField -- Operator's left-side argument.
                      , arg2 :: Maybe TACField -- Operator's right-side argument.
                      , truejumplabel :: String -- Label for the True branch.
                      , falsejumplabel :: String -- Label for the False branch.
                      , result :: Maybe TACField } | -- Result of the operation.
           Label      { name :: String} deriving (Show, Eq, Ord) -- Name for the label.

-- Data type for representing the many operators available.
data Operator = OSum     |
                ODif     |
                OMul     |
                ODiv     |
                OMod     |
                OPot     |
                ODivE    | 
                OLShift  |
                ORShift  |
                OBitOr   |
                OBitAnd  | 
                OBitXor  |
                OOr      |
                OAnd     |
                OGEq     |
                OGreat   |
                OLEq     |
                OLess    |
                ONEq     |
                OEqual   |
                ONeg     |
                ONot     |
                OBitNot  |
                OAssign  |
                OJump    |
                OIdent   | 
                OPrint   |  
                OPrintLn |    
                ONone
                deriving (Eq, Ord)

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

-- Data type for representing the values available for the 'arg1', 'arg2' and 'result' fields of a TAC.
data TACField = Temp      {value :: String} | -- Temporal identificator for holding intermediate values.
                Var       {value :: String} | -- Variable identificator.
                TokNum    {value :: String} | -- Numeric Token value.
                TokStr    {value :: String} | -- String Token value.
                TokChar   {value :: String} | -- Char Token value.
                Lab       {value :: String} | -- Label.
                VarArr    {value :: String    -- Variable identificator specifically for arrays.
                           , index :: TACField}  deriving (Eq, Ord)

instance Show TACField where
    show (Temp s) = s
    show (Var s) = s
    show (TokNum s) = s
    show (TokStr s) = s
    show (TokChar s) = s
    show (Lab s) = s
    show (VarArr s i) = s ++ "[" ++ show i ++ "]"

-- ################################################################################ --
--------------------------------------------------------------------------------------
--------------------- FUNCTIONS FOR STATE MONAD MANIPULATION -------------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

-- Returns a new temporal name, increases the temporal name counter by one.
genNewTemp :: State (Int,Int,String,String,String,Map String Integer) String
genNewTemp = state $ \(temp_counter,label_counter,next_code,true_code,false_code,width_map) -> ('t' : show (temp_counter+1),(temp_counter+1,label_counter,next_code,true_code,false_code,width_map))

-- Returns a new label name, increases the label name counter by one.
genNewLabel :: State (Int,Int,String,String,String,Map String Integer) String
genNewLabel = state $ \(temp_counter,label_counter,next_code,true_code,false_code,width_map) -> ("L" ++ show (label_counter+1),(temp_counter,label_counter+1,next_code,true_code,false_code,width_map))

-- Returns the current label associated with S.next.
getNextCode :: State (Int,Int,String,String,String,Map String Integer) String
getNextCode = getNextCodeFromStateTuple <$> get
    where getNextCodeFromStateTuple (_,_,l,_,_,_) = l

-- Returns the current label associated with E.true.
getTrueCode :: State (Int,Int,String,String,String,Map String Integer) String
getTrueCode = getTrueCodeFromStateTuple <$> get
    where getTrueCodeFromStateTuple (_,_,_,t,_,_) = t

-- Returns the current label associated with E.false.
getFalseCode :: State (Int,Int,String,String,String,Map String Integer) String
getFalseCode = getFalseCodeFromStateTuple <$> get
    where getFalseCodeFromStateTuple (_,_,_,_,f,_) = f

-- Sets S.next to a given value.
setNextCode :: String -> State (Int,Int,String,String,String,Map String Integer) ()
setNextCode string = state $ \(c1,c2,nc,tc,fc,wm) -> ((),(c1,c2,string,tc,fc,wm))

-- Sets E.true to a given value.
setTrueCode :: String -> State (Int,Int,String,String,String,Map String Integer) ()
setTrueCode string = state $ \(c1,c2,nc,tc,fc,wm) -> ((),(c1,c2,nc,string,fc,wm))

-- Sets E.false to a given value.
setFalseCode :: String -> State (Int,Int,String,String,String,Map String Integer) ()
setFalseCode string = state $ \(c1,c2,nc,tc,fc,wm) -> ((),(c1,c2,nc,tc,string,wm))

insertInWidthMap :: String -> Integer -> State (Int,Int,String,String,String,Map String Integer) ()
insertInWidthMap var width = state $ \(c1,c2,nc,tc,fc,wm) -> ((),(c1,c2,nc,tc,fc,M.insert var width wm))

-- ################################################################################ --
--------------------------------------------------------------------------------------
------------------------- FUNCTIONS FOR TAC MANIPULATION -----------------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

-- Conversion of an IfRegister into a String for later printing  .        
convertIfRegistertoString :: Operator -> TACField -> TACField -> String -> String -> TACField -> String 
convertIfRegistertoString o a1 a2 j1 j2 r = ": " ++ "if " ++ cond_string ++ " goto " ++ j1
    where cond_string = value a1 ++ " " ++ show o ++ " " ++ value a2
 
-- Conversion of a Quadruplet into a String for later printing.
convertQuadrupletoString :: Operator -> TACField -> TACField -> TACField -> String
convertQuadrupletoString o a1 a2 r 
 | o == OAssign = ": " ++ show r ++ " := " ++ show a1
 | (o == ONeg) || (o == ONot) || (o == OBitNot) = ": " ++ show r ++ " = " ++ show o ++ " " ++ show a1
 | o == OJump = ": " ++ show o ++ " " ++ show a1 
 | (o == OPrint) || (o == OPrintLn) = ": " ++ show o ++ " " ++ show a1
 | otherwise = ": " ++ show r ++ " = " ++ value a1 ++ " " ++ show o ++ " " ++ show a2

-- Conversion of a Label into a String for later printing.
convertLabeltoString :: String -> String
convertLabeltoString l = l ++ ":"

-- Conversion of a TAC into a String for later printing.
convertTACtoString :: TAC -> String
convertTACtoString IfRegister {op=o,arg1=a1,arg2=a2,truejumplabel=j1,falsejumplabel=j2,result=r} = let a1' = fromMaybe (Temp "") a1
                                                                                                       a2' = fromMaybe (Temp "") a2
                                                                                                       r' = fromMaybe (Temp "") r
                                                                                                       in convertIfRegistertoString o a1' a2' j1 j2 r'
convertTACtoString Quadruplet {op=o,arg1=a1,arg2=a2,result=r} = let a1' = fromMaybe (Temp "") a1
                                                                    a2' = fromMaybe (Temp "") a2 
                                                                    r' = fromMaybe (Temp "") r 
                                                                    in convertQuadrupletoString o a1' a2' r'
convertTACtoString Label {name=n} = convertLabeltoString n 

-- Prints a list of TAC into a human readable string.
printTAC :: [TAC] -> IO ()
printTAC tacList = let tacList' = filterTACList tacList
                       tacList'' = (L.map convertTACtoString . reverse) tacList'
                       in (putStrLn . unlines) tacList''

-- Filters unnecessary TAC registers, in particular, those concerning the names of identifiers and tokens.
filterTACList :: [TAC] -> [TAC]
filterTACList = L.filter filterTAC
    where filterTAC (Quadruplet op _ _ _) 
           | (op == ONone) || (op == OIdent) = False
           | otherwise = True
          filterTAC IfRegister {} = True
          filterTAC (Label _) = True

-- ################################################################################ --
--------------------------------------------------------------------------------------
------------------- AUXILIARY FUNCTIONS FOR TAC GENERATION ---------------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

-- Returns the latest name (temporal or identifier) used in a TAC list.
getLatestTemp :: [TAC] -> Maybe TACField
getLatestTemp = result . head

-- Creates a Quadruplet.
createQuadruplet :: Operator -> Maybe TACField -> Maybe TACField -> Maybe TACField -> TAC
createQuadruplet o a1 a2 r = Quadruplet {op=o, arg1=a1, arg2=a2, result=r}

-- Creates an IfRegister.
createIfRegister :: Operator -> Maybe TACField -> Maybe TACField -> String -> String -> Maybe TACField -> TAC
createIfRegister o a1 a2 j1 j2 r = IfRegister {op=o, arg1=a1, arg2=a2, truejumplabel=j1, falsejumplabel=j2, result=r}

-- Generates code for incorporating a label.
genLabelCode :: String -> TAC
genLabelCode string = Label {name=string}

-- Generates code for jumping to a certain label.
genJumpCode :: TACField -> TAC
genJumpCode label = Quadruplet {op=OJump, arg1=Just label, arg2=Nothing, result=Nothing}

-- Looks up in the hierarchic symbol table, and returns the width asociated with a given name.
lookupWidth :: SymTable -> String -> Integer -> Integer
lookupWidth symtable varname varscope = case M.lookup varname symtable of -- Name look up.
                                         Just var_scope_list -> case M.lookup varscope var_scope_list of -- Scope look up.
                                                                 Just scope -> width scope 

-- TAC template for binary operators
genBinOpTAC :: (TACConvertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genBinOpTAC symtable bin_op exp1 exp2 = do
    leftTAC <- toTAC symtable exp1 -- left-side expression TAC.
    rightTAC <- toTAC symtable exp2 -- right-side expression TAC.
    newlabel <- genNewTemp -- new temporal name for holding the value.
    insertInWidthMap newlabel 4
    let subTAC = rightTAC ++ leftTAC 
        newTemp = Temp newlabel
        newTAC = createQuadruplet bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) (Just newTemp)
        in return (newTAC : subTAC)      

-- TAC template for relational binary operators
genRelBinOpTAC :: (TACConvertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genRelBinOpTAC symtable rel_bin_op exp1 exp2 = do
    t_jump <- genNewLabel  -- Label for the True branch.
    f_jump <- genNewLabel  -- Label for the False branch.
    leftTAC <- toTAC symtable exp1 -- left-side expression TAC.
    rightTAC <- toTAC symtable exp2 -- right-side expression TAC.
    newlabel <- genNewTemp -- new temporal name for holding the value.
    insertInWidthMap newlabel 4
    finallabel <- genNewLabel -- next instruction's label.
    let subTAC = rightTAC ++ leftTAC
        newTemp = Temp newlabel
        jumpcode = genJumpCode (Lab finallabel) -- code for jumping to the next instruction.
        finallabelcode = genLabelCode finallabel -- code for incorpotaring the next instruction's label.
        truelabelcode = genLabelCode t_jump -- code for incorporating the True branch's label.
        falselabelcode = genLabelCode f_jump -- code for incorporating the False branch's label.
        true_code = createQuadruplet OAssign (Just (TokNum "1")) Nothing (Just newTemp) -- True branch's code, newTemp := true.
        false_code = createQuadruplet OAssign (Just (TokNum "0")) Nothing (Just newTemp) -- False branch's code, newTemp := false.
        newTAC = createIfRegister rel_bin_op (getLatestTemp leftTAC) (getLatestTemp rightTAC) t_jump f_jump (Just newTemp)
        resultTAC = createQuadruplet ONone Nothing Nothing (Just newTemp) -- placeholder TAC register for retrieving the result's temporal name later.
        falsejumpcode = genJumpCode (Lab f_jump)
        in return (resultTAC : finallabelcode : jumpcode : false_code : falselabelcode : jumpcode : true_code : truelabelcode : falsejumpcode : newTAC : subTAC)

-- TAC template for unary operators.
genUnOpTAC :: (TACConvertible a) => SymTable -> Operator -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genUnOpTAC symtable un_op exp = do
    expTAC <- toTAC symtable exp -- expression TAC.
    newlabel <- genNewTemp -- new temporal name for holding the value.
    insertInWidthMap newlabel 4
    let newTemp = Temp newlabel
        newTAC = createQuadruplet un_op (getLatestTemp expTAC) Nothing (Just newTemp)
        in return (newTAC : expTAC)

-- Flow-Control TAC template for relational binary operators.
genFlowRelBinOPTAC :: (TACConvertible a) => SymTable -> Operator -> a -> a -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genFlowRelBinOPTAC symtable rel_bin_op exp1 exp2 = do
    t_jump <- getTrueCode -- Label for the True branch.
    f_jump <- getFalseCode -- Label for the False branch.
    leftTAC <- toTAC symtable exp1 -- left-side expression TAC.
    rightTAC <- toTAC symtable exp2 -- right-side expresssion TAC.
    let subTAC = rightTAC ++ leftTAC
        id1 = getLatestTemp leftTAC 
        id2 = getLatestTemp rightTAC
        falsejump = genJumpCode (Lab f_jump)
        newTAC = createIfRegister rel_bin_op id1 id2 t_jump f_jump Nothing
        in return (falsejump : newTAC : subTAC)
                                                   
-- TAC generation for an array's expression list, returns a list of list of TAC.
{-toTACArray :: SymTable -> RightValue -> State (Int,Int,String,String,String) [[TAC]]
toTACArray symtable (ValueExp (EArr _ exp_list)) = mapM (toTACArray symtable) exp_list -- AQUI HAY UN BETA CON LOS TIPOS NO BÁSICOS
-}

-- TAC generation for assignments of type Array.
genArrayAssignTAC :: SymTable -> [Identifier] -> [RightValue] -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genArrayAssignTAC symtable id_list rv_list = do
    idTAC <- mapM (toTAC symtable) id_list -- a list of [TAC], one for each identifier. :: [[TAC]]
    let idTACList = concat idTAC
        idStringList = [ (fromMaybe (Temp "") . result) x | x <- idTACList, op x == OIdent ] -- list of identifier names.
        assignmentTuples = reverse (zip id_list rv_list) -- :: [(Identifier,RightValue)]
        in do
           newTAC <- mapM (genArrayTAC symtable) assignmentTuples -- a list of TAC, containing the n-assignments 'id[0],..,id[n] := temp_0,..,temp_n' :: [TAC]
           let newTAC' = concat [ uncurry (++) x | x <- zip newTAC (reverse idTAC) ] -- for each id, add it's auxiliar TAC code, if it has any. :: [ [TAC] | (TAC,[TAC]) ]
               in return newTAC' 

-- Auxiliar function for TAC generation for assignments of type Array, in charge of generating the actual assignments.
genArrayTAC :: SymTable -> (Identifier, RightValue) -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genArrayTAC symtable (id,ValueExp (EArr t exp_list) ) = let n = length exp_list -- length of the array
                                                            idIndexList = [ Index t id (EToken TypeInt (TNum (AlexPn 0 0 0) (show x))) | x <- [0..n] ] -- list of indexed identifier accesses, 'id[0]..id[n]'
                                                            rightValueList = [ ValueExp exp | exp <- exp_list ] -- transform the list of expressions into a list of right values.
                                                            assignmentsList = [ Assign t ([fst x],[snd x]) | x <- zip idIndexList rightValueList ] -- list of assignment nodes 'id[0] := rv_0,..,id[n] := rv_n'
                                                            in do
                                                                assignmentsTAC <- mapM (toTAC symtable) assignmentsList -- a list of [TAC], one for each assignment :: [[TAC]] 
                                                                let newTAC = (concat . reverse) assignmentsTAC
                                                                    in return newTAC

-- TAC generation for common assignments, ex: id := token, id := id.
genNormalAssignTAC :: SymTable -> [Identifier] -> [RightValue] -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genNormalAssignTAC symtable id_list rv_list = do
    rvTAC <- mapM (toTAC symtable) rv_list -- right value [TAC] list. :: [[TAC]]
    idTAC <- mapM (toTAC symtable) id_list -- identifiers [TAC] list. :: [[TAC]]
    let rvTemplist = L.map getLatestTemp rvTAC -- list of latest used temporal names of each right value, to be assigned to the identifiers. 
        idTAClist = concat idTAC
        idStringlist = [ result x | x <- idTAClist, op x == OIdent ] -- list of identifier names.
        assignmentTuples = reverse (zip rvTemplist idStringlist) -- assignment tuples [(rv_temp,id)].
        newTAC = [ createQuadruplet OAssign (fst x) Nothing (snd x) | x <- assignmentTuples ] -- TAC list 'id := rv_temp' :: [TAC]
        newTAC' = [ uncurry (:) x | x <- zip newTAC (reverse idTAC) ] -- for each id, add it's auxiliar TAC code, if it has any. :: [ [TAC] | ( TAC,[TAC]) ]
        finalTAC = concat [ uncurry (++) x | x <- zip newTAC' (reverse rvTAC) ] -- for each right value, add it's auxiliar TAC code, if it has any. :: [ [TAC] | ([TAC],[TAC]) ]
        in return finalTAC

-- TAC generation for all of the function's code.
genFuncTAC :: (TACConvertible a) => SymTable -> [(String,a)] -> State (Int,Int,String,String,String,Map String Integer) [TAC]
genFuncTAC symtable func_tuples = do
    funcTAC <- mapM (funcTupleToTAC symtable) func_tuples -- [[TAC]]
    return (concat funcTAC)

-- TAC generation for an individual function's code.
funcTupleToTAC :: (TACConvertible a) => SymTable -> (String,a) -> State (Int,Int,String,String,String,Map String Integer) [TAC]
funcTupleToTAC symtable (func_name,func_code) = do
    func_codesTAC <- toTAC symtable func_code -- [TAC] list of the function's code.
    let nameTAC = genLabelCode func_name -- code for incorporating the function's label.
        in return (nameTAC : func_codesTAC)

-- ################################################################################ --
--------------------------------------------------------------------------------------
------------------------ TAC GENERATION INSIDE THE SYNTAX TREE -----------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

-- Beginning of the Syntax Tree.
instance TACConvertible Init where
   toTAC symtable (Init _ _ _ ins_list) = do
        nextcode <- genNewLabel -- Base case for S.next.
        setNextCode nextcode
        insTAC <- mapM (toTAC symtable) ins_list -- TAC list of the program's instructions.
        return (concat $ reverse insTAC)
  
-- Instructions.
instance TACConvertible Instruction where
    -- Blocks.
    toTAC symtable (Block _ ins_list) = do
        insTAC <- mapM (toTAC symtable) (reverse ins_list) -- TAC list of the block's instructions.
        return ((concat . reverse) insTAC)

    -- Assignments.
    toTAC symtable (Assign _ (id_list,rv_list)) = let rv_list_sample = head rv_list -- since lists are homogenic, take a sample to check it's type.
                                                      in case rv_list_sample of
                                                              (ValueExp (EArr _ _ )) -> genArrayAssignTAC symtable id_list rv_list -- Array assignment.
                                                              (ValueExp _) -> genNormalAssignTAC symtable id_list rv_list          -- Normal assignment.

    -- Selectors.
    -- If Exp then Instruction.
    toTAC symtable (IfThen _ exp ins) = do -- S -> if E then S1
        nextcode <- getNextCode -- this instruction's nextcode.
        exp_f_jump <- getNextCode -- E.false := S.next
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- conditional expression TAC.
        nextcode' <- genNewLabel -- next instruction's label.
        setNextCode nextcode'
        insTAC <- toTAC symtable ins -- S1.next := S.next, instruction TAC.
        let labelcode = genLabelCode exp_t_jump -- code for incorporating the instruction's label. 
            labelcodefinal = genLabelCode nextcode -- Etiqueta de la instruccion siguiente al if
            in return (labelcodefinal : (insTAC ++ (labelcode : expTAC)))

    -- If Exp then Instruction Else Instruction.
    toTAC symtable (IfElse _ exp ins1 ins2) = do -- S -> if E then S1 else S2
        nextcode <- getNextCode -- this instruction's nextcode.
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- genNewLabel -- E.false := newlabel()
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- conditional expression TAC.
        nextcode' <- genNewLabel -- next instruction's label.
        setNextCode nextcode'
        ins_list1TAC <- toTAC symtable ins1 -- instruction #1 TAC.
        ins_list2TAC <- toTAC symtable ins2 -- instruction #2 TAC.
        let labelcode1 = genLabelCode exp_t_jump -- code for incorporating the instruction #1's label.
            labelcode2 = genLabelCode exp_f_jump -- code for incorporating the instruction #2's label.
            jumpcode = genJumpCode (Lab nextcode) -- code for jumping to the next instruction.
            labelcodefinal = genLabelCode nextcode -- code for incorporating the next instruction's label.
            in return (labelcodefinal : (ins_list2TAC ++ (labelcode2 : jumpcode : ins_list1TAC) ++ (labelcode1 : expTAC)))

    -- Undetermined Cycles.
    -- While Exp then Instruction.
    toTAC symtable (While _ exp ins) = do -- S -> while E do S1
        nextcode <- getNextCode -- this instruction's nextcode.
        begin <- genNewLabel -- S.begin = newlabel(), cycle header.
        exp_t_jump <- genNewLabel -- E.true := newlabel()
        exp_f_jump <- getNextCode -- E.false := S.next
        --setNextCode begin -- S1.next := S.begin
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        expTAC <- toTACFlow symtable exp -- conditional expression TAC.
        nextcode' <- genNewLabel -- next instruction's label.
        setNextCode nextcode'
        insTAC <- toTAC symtable ins -- instruction TAC.
        let labelcodeheader = genLabelCode begin -- code for incorporating the header's label.
            labelcode1 = genLabelCode exp_t_jump -- code for incorporating the instruction's label.
            jumpcode = genJumpCode (Lab begin) -- code for jumping back to the header.
            labelcodefinal = genLabelCode nextcode -- code for incorporating the next instruction's label.
            in return (labelcodefinal : (jumpcode : (insTAC ++ (labelcode1 : reverse (labelcodeheader : reverse expTAC)))))

    -- Determined Cycles .
    -- For
    toTAC symtable (Det _ for) = toTAC symtable for

    -- Prints.
    -- Print.
    toTAC symtable (Print _ exp) = do 
        expTAC <- toTAC symtable exp -- TAC for the expression to be printed.
        let newTAC = createQuadruplet OPrint (getLatestTemp expTAC) Nothing Nothing
            in return (newTAC : expTAC)

    -- Print with new line.
    toTAC symtable (PrintLn _ exp) = do
        expTAC <- toTAC symtable exp -- TAC for the expression to be printed.
        let newTAC = createQuadruplet OPrintLn (getLatestTemp expTAC) Nothing Nothing
            in return (newTAC : expTAC)
 
-- Iteradores determinados.
instance TACConvertible For where

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

-- Expressions.
instance TACConvertible Exp where
    -- Binary.
    -- Addition. (+)
    toTAC symtable (ESum _ exp1 exp2) = genBinOpTAC symtable OSum exp1 exp2 
    -- Substraction. (-)
    toTAC symtable (EDif _ exp1 exp2) = genBinOpTAC symtable ODif exp1 exp2
    -- Multiplication. (*)
    toTAC symtable (EMul _ exp1 exp2) = genBinOpTAC symtable OMul exp1 exp2
    -- Division. (/)
    toTAC symtable (EDiv _ exp1 exp2) = genBinOpTAC symtable ODiv exp1 exp2
    -- Module. (%)
    toTAC symtable (EMod _ exp1 exp2) = genBinOpTAC symtable OMod exp1 exp2
    -- Exponentiation. (**)
    toTAC symtable (EPot _ exp1 exp2 ) = genBinOpTAC symtable OPot exp1 exp2
    -- Whole division. (//)
    toTAC symtable (EDivE _ exp1 exp2 ) = genBinOpTAC symtable ODivE exp1 exp2
    -- Left shift. (<<)
    toTAC symtable (ELShift _ exp1 exp2 ) = genBinOpTAC symtable OLShift exp1 exp2
    -- Right shift. (>>)
    toTAC symtable (ERShift _ exp1 exp2 ) = genBinOpTAC symtable ORShift exp1 exp2
    -- Bit to Bit disyunction. (|)
    toTAC symtable (EBitOr _ exp1 exp2 ) = genBinOpTAC symtable OBitOr exp1 exp2
    -- Bit to Bit conjuction. (&)
    toTAC symtable (EBitAnd _ exp1 exp2 ) = genBinOpTAC symtable OBitAnd exp1 exp2
    -- Bit to Bit XOR. (^)
    toTAC symtable (EBitXor _ exp1 exp2 ) = genBinOpTAC symtable OBitXor exp1 exp2


    -- Logic disyunction. (||)
    toTAC symtable (EOr _ exp1 exp2 ) = genBinOpTAC symtable OOr exp1 exp2
    -- Logic conjunction. (&&)
    toTAC symtable (EAnd _ exp1 exp2 ) = genBinOpTAC symtable OAnd exp1 exp2

    -- Relational booleans.
    -- Greater or Equal than. (>=)
    toTAC symtable (EGEq _ exp1 exp2 ) = genRelBinOpTAC symtable OGEq exp1 exp2
    -- Greater than. (>)
    toTAC symtable (EGreat _ exp1 exp2 ) = genRelBinOpTAC symtable OGreat exp1 exp2
    -- Less or Equal than. (<=)
    toTAC symtable (ELEq _ exp1 exp2 ) = genRelBinOpTAC symtable OLEq exp1 exp2
    -- Less. (<)
    toTAC symtable (ELess _ exp1 exp2 ) = genRelBinOpTAC symtable OLess exp1 exp2
    -- Not equal. (/=)
    toTAC symtable (ENEq _ exp1 exp2 ) = genRelBinOpTAC symtable ONEq exp1 exp2
    -- Equal. (==)
    toTAC symtable (EEqual _ exp1 exp2 ) = genRelBinOpTAC symtable OEqual exp1 exp2

    -- Unary.
    -- Arithmetic negation. (-)
    toTAC symtable (ENeg _ exp ) = genUnOpTAC symtable ONeg exp
    -- Logic negation. (uminus)
    toTAC symtable (ENot _ exp ) = genUnOpTAC symtable ONot exp 
    -- Bit to Bit negation. (~)
    toTAC symtable (EBitNot _ exp ) = genUnOpTAC symtable OBitNot exp 

    -- Tokens.
    -- Numeric.
    toTAC symtable (EToken _ (TNum _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokNum s))]
    -- Strings.
    toTAC symtable (EToken _ (TString _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokStr s))]
    -- Characters.
    toTAC symtable (EToken _ (TChar _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokChar s))]
    -- Booleans.
    toTAC symtable (EToken _ (TTrue _)) = return [createQuadruplet ONone Nothing Nothing (Just (TokNum "1"))]
    toTAC symtable (EToken _ (TFalse _)) = return [createQuadruplet ONone Nothing Nothing (Just (TokNum "0"))]

    -- Identifiers as expressions.
    toTAC symtable (EIdent _ id) = case id of
                                        (Variable _ _) -> toTAC symtable id
                                        Index {} -> do 
                                                    indexTAC <- toTAC symtable id
                                                    newtemp <- genNewTemp
                                                    insertInWidthMap newtemp 4
                                                    let newTemp = Temp newtemp
                                                        newTAC = createQuadruplet OAssign (getLatestTemp indexTAC) Nothing (Just newTemp)
                                                        in return (newTAC : indexTAC)
    
    -- Arrays.
    -- Check the auxiliary function 'toTACArray'.

    -- ##################################### --
    -------------------------------------------
    -------- FUNCTIONS FOR FLOW-CONTROL -------
    -------------------------------------------
    -- ##################################### --

    -- Binary.
    -- Addition. (+)
    toTACFlow symtable (ESum _ exp1 exp2) = genBinOpTAC symtable OSum exp1 exp2 
    -- Substraction. (-)
    toTACFlow symtable (EDif _ exp1 exp2) = genBinOpTAC symtable ODif exp1 exp2
    -- Multiplication. (*)
    toTACFlow symtable (EMul _ exp1 exp2) = genBinOpTAC symtable OMul exp1 exp2
    -- Division. (/)
    toTACFlow symtable (EDiv _ exp1 exp2) = genBinOpTAC symtable ODiv exp1 exp2
    -- Module. (%)
    toTACFlow symtable (EMod _ exp1 exp2) = genBinOpTAC symtable OMod exp1 exp2
    -- Exponentiation. (**)
    toTACFlow symtable (EPot _ exp1 exp2 ) = genBinOpTAC symtable OPot exp1 exp2
    -- Whole division. (//)
    toTACFlow symtable (EDivE _ exp1 exp2 ) = genBinOpTAC symtable ODivE exp1 exp2
    -- Left shift. (<<)
    toTACFlow symtable (ELShift _ exp1 exp2 ) = genBinOpTAC symtable OLShift exp1 exp2
    -- Right shift. (>>)
    toTACFlow symtable (ERShift _ exp1 exp2 ) = genBinOpTAC symtable ORShift exp1 exp2
    -- Bit to Bit disyunction. (|)
    toTACFlow symtable (EBitOr _ exp1 exp2 ) = genBinOpTAC symtable OBitOr exp1 exp2
    -- Bit to Bit conjunction. (&)
    toTACFlow symtable (EBitAnd _ exp1 exp2 ) = genBinOpTAC symtable OBitAnd exp1 exp2
    -- Bit to Bit XOR. (^)
    toTACFlow symtable (EBitXor _ exp1 exp2 ) = genBinOpTAC symtable OBitXor exp1 exp2

    -- Tokens (no change compared to the normal 'toTAC' function).
    -- Numeric.
    toTACFlow symtable (EToken _ (TNum _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokNum s))]
    -- Strings.
    toTACFlow symtable (EToken _ (TString _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokStr s))]
    -- Characters.
    toTACFlow symtable (EToken _ (TChar _ s)) = return [createQuadruplet ONone Nothing Nothing (Just (TokChar s))]

    -- Boolean identifiers seen as expressions.
    toTACFlow symtable (EIdent TypeBool id) = do -- E -> id, :: Bool id;
        t_jump <- getTrueCode
        f_jump <- getFalseCode
        idTAC <- toTAC symtable id
        let falsejump = genJumpCode (Lab f_jump)
            newTAC = createIfRegister OEqual (getLatestTemp idTAC) (Just (TokNum "1")) t_jump f_jump Nothing -- 'if ID then', is translated into 'if ID == true then'
            in return (falsejump : newTAC : idTAC)

    -- Non-boolean identifiers seen as expressions. -- E -> id
    toTACFlow symtable (EIdent _ id) = toTAC symtable id

    -- Relational booleans.
    -- Greater or equal than. (>=)
    toTACFlow symtable (EGEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OGEq exp1 exp2
    -- Greater than. (>)
    toTACFlow symtable (EGreat _ exp1 exp2) = genFlowRelBinOPTAC symtable OGreat exp1 exp2
    -- Less or equal than. (<=)
    toTACFlow symtable (ELEq _ exp1 exp2) = genFlowRelBinOPTAC symtable OLEq exp1 exp2
    -- Less. (<)
    toTACFlow symtable (ELess _ exp1 exp2) = genFlowRelBinOPTAC symtable OLess exp1 exp2
    -- Not equal. (/=)
    toTACFlow symtable (ENEq _ exp1 exp2) = genFlowRelBinOPTAC symtable ONEq exp1 exp2
    -- Equal. (==)
    toTACFlow symtable (EEqual _ exp1 exp2) = genFlowRelBinOPTAC symtable OEqual exp1 exp2

    {--- Desigualdad (/=)
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
            falsejump = genJumpCode (Lab f_jump)
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
            falsejump = genJumpCode (Lab f_jump)
            newTAC = createIfRegister OEqual (getLatestTemp leftTAC) (getLatestTemp rightTAC) t_jump f_jump Nothing
            in return (falsejump : newTAC : subTAC)-}

    -- Booleans.
    -- Disjunction. (||)
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

    -- Conjuncion. (&&)
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

    -- Negation. (!)
    toTACFlow symtable (ENot _ exp) = do -- E -> not E1
        exp_t_jump <- getFalseCode  -- E1.true := E.false
        exp_f_jump <- getTrueCode   -- E1.false := E.true
        setTrueCode exp_t_jump
        setFalseCode exp_f_jump
        toTACFlow symtable exp

    -- True.
    toTACFlow symtable (EToken _ (TTrue _)) = do -- E -> true
        t_jump <- getTrueCode
        return [createQuadruplet OJump (Just (Lab t_jump)) Nothing Nothing]

    -- False.
    toTACFlow symtable (EToken _ (TFalse _)) = do -- E -> false
        f_jump <- getFalseCode
        return [createQuadruplet OJump (Just (Lab f_jump)) Nothing Nothing]

-- Identifiers.
instance TACConvertible Identifier where
    -- Identifier names.
    toTAC symtable (Variable _ (name,scope,_)) = let width = lookupWidth symtable name scope
                                                     in do
                                                        insertInWidthMap name width
                                                        return [createQuadruplet OIdent Nothing Nothing (Just (Var (name++show scope)))]-- 'name' contains, quite obviously, the identifier name , 'scope' the associated scope.
    -- |offset == 2 = return [createQuadruplet OIdent Nothing Nothing (Just (Var name))] -- if the identifier exists in the global scope, it doesn't need an offset.
    -- |otherwise = return [createQuadruplet OIdent Nothing Nothing (Just (Var (name ++ "[" ++ show offset ++ "]")))] -- otherwise, we need the offset.
                
    -- Array accesses. (Indexation)
    toTAC symtable (Index (TypeArray t _) id exp)  = do
        idTAC <- toTAC symtable id -- a list of TAC for the identifier :: [TAC]
        expTAC <- toTAC symtable exp -- a list of TAC for the index expression :: [TAC] 
        newlabel1 <- genNewTemp
        insertInWidthMap newlabel1 4
        newlabel2 <- genNewTemp
        insertInWidthMap newlabel2 4
        let width = lookupWidth symtable (idString id) (idScope id) -- total width of the array.
            offsetWidth = getWidth t
            newTemp1 = Temp newlabel1
            newTemp2 = Temp newlabel2
            in case id of 
                Variable t (name,_,_) -> let indexTAC = createQuadruplet OAssign (getLatestTemp expTAC) Nothing (Just newTemp1)              -- temp1 := index_exp
                                             offsetTAC = createQuadruplet OMul (Just newTemp1) (Just (TokNum (show offsetWidth))) (Just newTemp2)  -- temp2 := temp1 * width
                                             newTAC = createQuadruplet OIdent Nothing Nothing (Just (VarArr (idString id) (Temp newlabel2))) -- id[temp2]
                                             in do 
                                                insertInWidthMap (idString id) width
                                                return (newTAC : offsetTAC : indexTAC : expTAC)
                Index t subId _ -> do 
                                   newlabel3 <- genNewTemp
                                   insertInWidthMap newlabel3 4
                                   let subWidth = "subWidth" -- UwU
                                       subTAC =  tail idTAC
                                       indexTAC = createQuadruplet OAssign (getLatestTemp expTAC) Nothing (Just newTemp1)              -- temp1 := index_exp 
                                       offsetTAC = createQuadruplet OMul (Just newTemp1) (Just (TokNum (show offsetWidth))) (Just newTemp2)  -- temp2 := temp1 * width
                                       newTemp3 = Temp newlabel3 
                                       greaterOffsetTAC = createQuadruplet OSum (getLatestTemp subTAC) (Just newTemp2) (Just newTemp3) -- temp3 :=  + temp2
                                       newTAC = createQuadruplet OIdent Nothing Nothing (Just (VarArr (idString id) (Temp newlabel3))) -- id[temp3]
                                       in do 
                                          insertInWidthMap (idString id) width
                                          return (newTAC : greaterOffsetTAC : offsetTAC : indexTAC : (expTAC ++ subTAC))

-- Right Values.
instance TACConvertible RightValue where
    toTAC symtable (ValueExp exp) = toTAC symtable exp 

    --toTACFlow symtable (ValueExp exp) = return ?

-- Function calls.
--instance TACConvertible FCall where
    --toTAC symtable (FCall _ t param_list) = do
    --    param_listTAC <- mapM (toTAC symtable) param_list [[TAC]] -- Lista de TACs de las expresiones correspondientes a los parámetros
    --    let tempList = mapM getLatestTemp param_listTAC  -- Lista de últimos temporales utilizados en cada [TAC] de las expresiones