{-|
Module: TargetCode
Authors : Carlos Infante,
          Daniel Varela
        
Everything concerning the generation and manipulation of target code, in particular MIPS.
-}

module TargetCode where

import TAC

import Data.Graph as G
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Data.Maybe (fromMaybe)

-- Data type representing a Basic Block structure.
data BasicBlock = Block { ins_list :: [TAC]
                        , succ_list :: [Int]
                        , in_live :: Set TACField
                        , out_live :: Set TACField } deriving (Show, Eq, Ord)

type FlowGraph = Map Int BasicBlock

-- Data type represeting the MIPS instruction set.
data MIPSTargetCode 
    -- Declarations
    = Decl String Integer
    -- Arithmetic
    | Add Register Register Register   -- Addition
    | Sub Register Register Register   -- Substraction
    | Addi Register Register Constant  -- Addition with inmediate, 'inmediate' means a constant number. 
    | Addu Register Register Register  -- Addition, but values are treated as unsigned integers, not two's complement integers.
    | Subu Register Register Register  -- Substraction, but values are treated as unsigned integers, not two's complement integers.
    | Addiu Register Register Constant -- Addition with inmediate, but values are treated as unsigned integers, not two's complement integers.
    | Mul Register Register Register   -- Multiply (without overflow), result is only 32 bits!.
    | Mult Register Register           -- Upper 32 bits stored in special register 'hi'. Lower 32 bits stored in special register 'lo'.
    | Div Register Register            -- Remainder stored in special register 'hi'. Quotient stored in special register 'lo'.
    -- Logical
    | And Register Register Register   -- Bitwise AND.
    | Or Register Register Register    -- Bitwise OR.
    | Andi Register Register Constant  -- Bitwise AND with immediate value.
    | Ori Register Register Constant   -- Bitwise OR with immediate value.
    | Sll Register Register Constant   -- Shift left by constant number of bits.
    | Srl Register Register Constant   -- Shift right by constant number of bits.
    -- Data Transfer
    | Lw Register Memory               -- Load word. Copy from memory to register.
    | Sw Register Memory               -- Save word. Copy from register to memory.
    | Lui Register Constant            -- Load constant into upper 16 bits. Lower 16 bits are set to zero.
    -- | La Register Label (?) 
    -- | Li Register Constant (?)
    | Mfhi Register                    -- Copy from special register 'hi' to general register.
    | Mflo Register                    -- Copy from special register 'lo' to general register.
    | Move Register Register           -- Pseudo-instruction (provided by assembler, not processor!). Copy from register to register.
    -- Conditional Branch 
    | Beq Register Register Constant   -- Branch on equal. Test if registers are equal.
    | Bne Register Register Constant   -- Branch on not equal. Test if registers are not equal.
    | Bgt Register Register Constant   -- Branch on greater than. Pseudo-instruction.
    | Bge Register Register Constant   -- Branch on greater than or equal. Pseudo-instruction.
    | Blt Register Register Constant   -- Branch on less than. Pseudo-instruction.
    | Ble Register Register Constant   -- Branch on less than or equal. Pseudo-instruction.
    -- Comparison
    | Slt Register Register Register   -- Test if less than. If true, set $1 to 1. Otherwise, set $1 to 0.
    | Slti Register Register Constant  -- Test if less than. If true, set $1 to 1. Otherwise, set $1 to 0.
    -- Unconditional Jump
    | Jump Constant                    -- Jump to target address.
    | Jr Register                      -- For switch, procedure return.
    | Jal Constant                     -- Use when making procedure call. This saves the return address in $ra.
    -- System Calls               
    | PrintInt                         -- Print integer number (32 bit), $a0 = integer to be printed. $v0 = 1.
    | PrintFlt                         -- Print floating-point number (32 bit), $f12 = float to be printed. $v0 = 2.
    | PrintDbl                         -- Print floating-point number (64 bit), $f12 = double to be printed. $v0 = 3.
    | PrintStr                         -- Print null-terminated character string, $a0 = address of string in memory. $v0 = 4.
    | PrintChr                         -- Print character, $a0 = character to be printed. $v0 = 11.

instance Show MIPSTargetCode where
    show (Decl s v) = s ++ ":" ++ " .space " ++ show v

-- Data type representing a register, which is just an alias for a String.
newtype Register = Reg {value :: String} deriving (Show, Eq,Ord)

-- Data type representing a constant, which is just an alias for a Int.
type Constant = Int

-- Data type representing a memory access, which is a just an alias for a String.
type Memory = String

-- Data type representing the Range Table, where the usage ranges of the variables are stored.
type RangeTable = Map String (Int,Int)

-- Data type representing the Descriptor Table, where the usage of registers and ubication of variables are stored.
type DescriptorTable = Map Descriptor (Set Descriptor)

data Descriptor = Regi Register | TField TACField deriving (Show, Eq, Ord)

-- Generates target code
genTargetCode :: Map String Integer -> [TAC] -> ([MIPSTargetCode],[MIPSTargetCode])
genTargetCode declarations tacList = let dataDeclarations = L.map makeDeclaration (M.toList declarations) 
                                         optimizedTAC = L.map constantFolding tacList
                                         varsList = L.map TField (S.toList (L.foldr getVariables S.empty optimizedTAC))
                                         --enumTAC = enumerateTAC tacList -- Enumerated TAC list.
                                         --rangeTable = L.foldl buildRangeTable M.empty enumTAC -- Range Table for usage ranges of variables.
                                         --varsList = M.keys rangeTable -- List of variables.
                                         --varEnumerationMap = M.fromList (zip varsList [0..length varsList]) -- Map of variables names to their corresponding interger enumeration.
                                         --edgeList = buildInterferenceEdges rangeTable -- Edge list for graph construction.
                                         --(graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
                                         --graphColoring = bruteSearchGraphColoring graph 16 -- MIPS in particular has 16 non-reserved assignable registers (?).
                                         registersList = L.map Reg ["$s0","$s1","$s2","$s3","$s4","$s5","$s6","$s7","$t0","$t1","$t2","$t3","$t4","$t5","$t6","$t7","$t8","$t9"]
                                         registersListasDescriptors = L.map Regi registersList
                                         partitionedTAC = partitionTACList' optimizedTAC 
                                         finalPartitionedTAC = finalPartitioning partitionedTAC
                                         basicBlocks = genBasicBlocks finalPartitionedTAC -- List of basic blocks
                                         flowGraph = buildFlowGraph basicBlocks
                                         flowGraphUpdated = liveVariables flowGraph ([S.empty],[S.empty])
                                         descriptorTableKeys = registersListasDescriptors ++ varsList
                                         descriptorTable = M.fromList (zip descriptorTableKeys [ S.empty | x <- [0..length descriptorTableKeys] ]) -- :: DescriptorTable
                                         {-in case graphColoring of
                                           Just g -> let registersAssignmentMap = createRegisterAssignmentMap varEnumerationMap g registersMap
                                              in registersAssignmentMap
                                          Nothing -> let registersAssignmentMap = M.empty
                                              in registersAssignmentMap-}
                                         in (dataDeclarations,[])

makeDeclaration :: (String,Integer) -> MIPSTargetCode
makeDeclaration (var,width) = Decl var width          

-- ################################################################################ --
--------------------------------------------------------------------------------------
-------------------- FUNCTIONS FOR DEALING WITH MIPS TARGET CODE  --------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

printMIPSCode :: ([MIPSTargetCode],[MIPSTargetCode]) -> IO ()
printMIPSCode (dataDeclarations,textInstructions) = let header = ".data\n"
                                                        l1 = (unlines . L.map show) dataDeclarations
                                                        in putStrLn (header ++ l1 ++ ".text\n")

-- ################################################################################ --
--------------------------------------------------------------------------------------
--------------------------- FUNCTIONS FOR OPTIMIZING TAC  ----------------------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

constantFolding :: TAC -> TAC
constantFolding (Quadruplet OSum (Just (TokNum v1)) (Just (TokNum v2)) r) = createQuadruplet OAssign (Just (TokNum (show (read v1 + read v2 :: Int)))) Nothing r
constantFolding (Quadruplet ODif (Just (TokNum v1)) (Just (TokNum v2)) r) = let dif = read v1 - read v2 :: Int
                                                                                in if dif < 0 
                                                                                    then createQuadruplet ONeg (Just (TokNum (show (abs dif)))) Nothing r
                                                                                    else createQuadruplet OAssign (Just (TokNum (show dif))) Nothing r
constantFolding (Quadruplet OMul (Just (TokNum v1)) (Just (TokNum v2)) r) = createQuadruplet OAssign (Just (TokNum (show (read v1 * read v2 :: Int)))) Nothing r
constantFolding (Quadruplet OMod (Just (TokNum v1)) (Just (TokNum v2)) r) = createQuadruplet OAssign (Just (TokNum (show (read v1 `mod` read v2 :: Int)))) Nothing r
constantFolding (Quadruplet ODivE (Just (TokNum v1)) (Just (TokNum v2)) r) = createQuadruplet OAssign (Just (TokNum (show (read v1 `div` read v2 :: Int)))) Nothing r
constantFolding (Quadruplet OPot (Just (TokNum v1)) (Just (TokNum v2)) r) = createQuadruplet OAssign (Just (TokNum (show (read v1 ^ read v2 :: Int)))) Nothing r
constantFolding tac = tac 

-- ################################################################################ --
--------------------------------------------------------------------------------------
-------------- FUNCTIONS FOR BASIC BLOCKS AND FLOW GRAPH CONSTRUCTION ----------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

partitionTACList' :: [TAC] -> [[TAC]]
partitionTACList' tacList = partitionTACList x xs
    where partition_index = findNextLeaderInstruction tacList
          (x,xs) = L.splitAt partition_index tacList

-- Partitions a TAC list, so we can build the Basic Blocks later
partitionTACList :: [TAC] -> [TAC] -> [[TAC]]
partitionTACList [] xs = [xs]
partitionTACList x [] = [x]
partitionTACList x xs = let partition_index = findNextLeaderInstruction (tail xs) + 1
                            (y,ys) = L.splitAt partition_index xs
                            in x : partitionTACList y ys

-- Finds the index for the next leader instruction in a list of TAC
findNextLeaderInstruction :: [TAC] -> Int
findNextLeaderInstruction = fromJustInt . L.findIndex checkIfLeader
    where checkIfLeader Label {} = True
          checkIfLeader Quadruplet {} = False
          checkIfLeader IfRegister {} = False
    
-- Extracts the value of a Maybe Int
fromJustInt :: Maybe Int -> Int
fromJustInt Nothing = 0
fromJustInt (Just x) = x 

checkBlockforLabelValue :: String -> BasicBlock -> Bool
checkBlockforLabelValue v bb = let blockHead = (head . ins_list) bb
                                   in case blockHead of
                                       Label n -> n == v
                                       Quadruplet {} -> False
                                       IfRegister {} -> False

-- Generates a list of Basic Blocks from a list of previously partitioned TAC
genBasicBlocks :: [[TAC]] -> [BasicBlock]
genBasicBlocks = L.map genBasicBlockfromTAC

-- Auxiliar function for converting a TAC into a Basic Block
genBasicBlockfromTAC :: [TAC] -> BasicBlock
genBasicBlockfromTAC tac = Block {ins_list=tac, succ_list=[], in_live=S.empty , out_live=S.empty}

-- Prints a list of Basic Blocks in a readable manner
printBasicBlocks :: [BasicBlock] -> IO ()
printBasicBlocks bb_list = let bb_list' = L.map convertBasicBlockToString bb_list
                               in (putStrLn . unlines) bb_list'

-- Converts a Basic Block into a human readable String for later printing
convertBasicBlockToString :: BasicBlock -> String 
convertBasicBlockToString bb = let tac_string_list = L.map convertTACtoString (ins_list bb)
                                   block_header = "Block #\n"
                                   in block_header ++ unlines tac_string_list

finalPartitioning :: [[TAC]] -> [[TAC]]
finalPartitioning l = L.filter (/=[]) (concatMap finalPartitionSubTACLists l)

finalPartitionSubTACLists :: [TAC] -> [[TAC]]
finalPartitionSubTACLists xs = let (y,ys) = L.splitAt (partitionIndex xs) xs
                                   in [y,ys]
    where checkIfIfRegister IfRegister {} = True
          checkIfIfRegister Quadruplet {} = False
          checkIfIfRegister Label {} = False
          partitionIndex l = case L.findIndex checkIfIfRegister l of
                              Just x -> x + 1
                              Nothing -> -1
                                    
buildFlowGraph :: [BasicBlock] -> FlowGraph
buildFlowGraph bb_list = let bbEnumerationList = zip [0..length bb_list] bb_list
                             initialFlowGraph = M.fromList bbEnumerationList
                             in L.foldr addFlowGraphEdges initialFlowGraph (reverse bbEnumerationList)
                             
addFlowGraphEdges :: (Int,BasicBlock) -> FlowGraph -> FlowGraph
addFlowGraphEdges (bb_num,bb) flowGraph = let blockBottom = (last . ins_list) bb
                                              in case blockBottom of
                                                IfRegister _ _ _ jumpDest _ _ -> let destBlockNumber = searchForDestBlock (M.toList flowGraph) jumpDest
                                                                                     in M.insert bb_num (Block {ins_list=ins_list bb, succ_list=[bb_num+1,destBlockNumber], in_live=S.empty, out_live=S.empty}) flowGraph
                                                Quadruplet OJump (Just (Lab jumpDest)) _ _ -> let destBlockNumber = searchForDestBlock (M.toList flowGraph) jumpDest
                                                                                                  in M.insert bb_num (Block {ins_list=ins_list bb, succ_list=[destBlockNumber], in_live=S.empty, out_live=S.empty}) flowGraph
                                                Quadruplet {} -> M.insert bb_num (Block {ins_list=ins_list bb, succ_list=[bb_num+1], in_live=S.empty, out_live=S.empty}) flowGraph
                                                Label {} -> M.insert bb_num (Block {ins_list=ins_list bb, succ_list=[bb_num+1], in_live=S.empty, out_live=S.empty}) flowGraph
  
searchForDestBlock :: [(Int,BasicBlock)] -> String -> Int
searchForDestBlock list label = let newMap = M.fromList [ (snd x,fst x) | x <- list ]
                                    blockList = [ snd x | x <- list ]
                                    destBlock = L.find (checkBlockforLabelValue label) blockList
                                    in fromMaybe (-1) (M.lookup (fromMaybe (Block [] [] S.empty S.empty) destBlock) newMap) 
                                    
printFlowGraph :: FlowGraph -> IO ()
printFlowGraph flowGraph = let list = M.toList flowGraph
                               string = L.map convertFlowBlockToString list
                               in (putStrLn . unlines) string

convertFlowBlockToString :: (Int,BasicBlock) -> String
convertFlowBlockToString (bb_num,bb) = let tac_string_list = L.map convertTACtoString (ins_list bb)
                                           blockHeader = "Block # " ++ show bb_num ++ "\n"
                                           blockSucc = "Block successors: " ++ show (succ_list bb) ++ "\n"
                                           blockIN = "Block IN: " ++ show (in_live bb) ++ "\n"
                                           blockOUT = "Block OUT: " ++ show (out_live bb) ++ "\n"
                                           in blockHeader ++ blockIN ++ unlines tac_string_list ++ blockSucc ++ blockOUT

liveVariables :: FlowGraph -> ([Set TACField], [Set TACField]) -> FlowGraph
liveVariables flowGraph (inSetList,outSetList) = if inSetList == new_inSetList && outSetList == new_outSetList then flowGraph else liveVariables newFlowGraph (new_inSetList,new_outSetList)
    where newFlowGraph = L.foldr liveVariablesInBlock flowGraph (M.toList flowGraph) 
          blockList = M.elems newFlowGraph
          new_inSetList = L.map in_live blockList
          new_outSetList = L.map out_live blockList

liveVariablesInBlock :: (Int,BasicBlock) -> FlowGraph -> FlowGraph
liveVariablesInBlock (bb_num,bb) flowGraph = let blockSuccList = if bb_num == (length . keys) flowGraph - 1 then [] else L.map ((M.!) flowGraph) (succ_list bb) -- List of S successors blocks of B.
                                                 blockSuccInList = if L.null blockSuccList then [S.empty] else L.map in_live blockSuccList -- List of IN[S], S successor of B.
                                                 blockOut = S.unions blockSuccInList -- OUT[B] = UNION IN[S], for all S successor of B.
                                                 blockTAC = ins_list bb -- Block TAC instruction list.
                                                 blockIn = L.foldr checkLiveVariablesInLine blockOut blockTAC
                                                 in M.insert bb_num (Block (ins_list bb) (succ_list bb) blockIn blockOut) flowGraph
                                                 
checkLiveVariablesInLine :: TAC -> Set TACField -> Set TACField
checkLiveVariablesInLine (Label _) acc = acc
checkLiveVariablesInLine (Quadruplet OJump _ _ _) acc = acc
checkLiveVariablesInLine (Quadruplet _ a1 a2 r) acc = let acc' = addToIN a1 acc
                                                          acc'' = addToIN a2 acc'
                                                          accFinal = removeFromIN r acc''
                                                          in accFinal
checkLiveVariablesInLine (IfRegister _ a1 a2 _ _ _) acc = let acc' = addToIN a1 acc
                                                              acc'' = addToIN a2 acc'
                                                              in acc''

addToIN :: Maybe TACField -> Set TACField -> Set TACField
addToIN v s = case v of
                Just x -> case x of
                           (TokNum _) -> s
                           (TokStr _) -> s
                           (TokChar _) -> s 
                           (Temp _) -> S.insert x s 
                           (Var _) -> S.insert x s
                           (VarArr n _) -> S.insert (Var n) s
                Nothing -> s

removeFromIN :: Maybe TACField -> Set TACField -> Set TACField
removeFromIN v s = case v of
                     Just x -> case x of 
                                (TokNum _) -> s
                                (TokStr _) -> s
                                (TokChar _) -> s
                                (Temp _) -> S.delete x s
                                (Var _) -> S.delete x s
                                (VarArr n _) -> S.delete (Var n) s
                     Nothing -> s 

getVariables :: TAC -> Set TACField -> Set TACField
getVariables (Quadruplet OJump _ _ _) acc = acc
getVariables (Quadruplet _ a1 a2 r) acc = L.foldr addVar acc [r,a2,a1]
getVariables (IfRegister _ a1 a2 _ _ _) acc = L.foldr addVar acc [a2,a1]
getVariables (Label _) acc = acc 

addVar :: Maybe TACField -> Set TACField -> Set TACField
addVar Nothing acc = acc
addVar (Just x) acc = case x of 
                       Temp s -> S.insert x acc
                       Var s -> S.insert x acc
                       TokNum s -> acc
                       TokStr s -> acc
                       TokChar s -> acc
                       Lab s -> acc
                       VarArr s e -> S.insert (Var s) acc   

-- ################################################################################ --
--------------------------------------------------------------------------------------
------------ FUNCTIONS FOR RANGE TABLE AND GRAPH COLORING CONSTRUCTION ---------------
--------------------------------------------------------------------------------------
-- ################################################################################ --

-- Assigns a line number to each TAC instruction 
enumerateTAC :: [TAC] -> [(Int,TAC)]
enumerateTAC tac_list = zip [0..length tac_list] tac_list

-- Prints a list of enumerated TAC
printEnumeratedTAC :: [(Int,TAC)] -> IO ()
printEnumeratedTAC enum_tac_list = let enum_tac_string_list = L.map convertEnumeratedTACToString  enum_tac_list
                                       in (putStrLn . unlines) enum_tac_string_list

-- Auxiliar function for converting enumerated TAC to string
convertEnumeratedTACToString :: (Int,TAC) -> String
convertEnumeratedTACToString (num,tac) = show num ++ " :: " ++ convertTACtoString tac

-- Builds the variable's usage range table
buildRangeTable :: RangeTable -> (Int,TAC) -> RangeTable
buildRangeTable range_table (line,tac) = L.foldl addRange range_table vars_to_check
    where vars_to_check = case tac of
                           (Quadruplet OJump _ _ _) -> [Nothing]
                           (Quadruplet _ arg1 arg2 result) -> L.map checkVar [arg1,arg2,result]
                           (IfRegister _ arg1 arg2 _ _ _) -> L.map checkVar [arg1,arg2]
                           (Label _) -> [Nothing]
          addRange t var = case var of
                          Nothing -> t
                          Just x -> case M.lookup x t of
                                     Nothing -> M.insert x (line,line) t
                                     Just (i,j) -> M.insert x (i,line) t
          checkVar (Just (Temp s)) = Just s 
          checkVar (Just (Var s)) = Just s
          checkVar (Just (TokNum s)) = Nothing
          checkVar (Just (TokStr s)) = Nothing
          checkVar (Just (TokChar s)) = Nothing
          checkVar (Just (Lab s)) = Nothing
          checkVar (Just (VarArr s _)) = Just s
          checkVar Nothing = Nothing

-- Checks if two ranges interfere with one another
checkInterference :: (Int,Int) -> (Int,Int) -> Bool
checkInterference (begin1,end1) (begin2,end2) = let range1 = [begin1..end1]
                                                    range2 = [begin2..end2]
                                                    intersection = range1 `L.intersect` range2
                                                    in not (L.null intersection)

-- Builds the edges of the Interference Graph
buildInterferenceEdges :: RangeTable -> [(String,Vertex,[Vertex])]
buildInterferenceEdges range_table = L.map enumerateNode [ (fst var,buildAdyacencyList range_table var) | var <- M.toList range_table ] -- [(String,[String])]
    where enumerateNode (n,l) = let vars_list = M.keys range_table -- enumerateNode :: (String,[String]) -> (String,Int,[Int])
                                    enumeration_map = M.fromList (zip vars_list [0..length vars_list]) 
                                    key = fromMaybe (-1) (M.lookup n enumeration_map)
                                    enum_adyacency_list = L.map (fromMaybe (-1) . lookup' enumeration_map) l
                                    in (n,key,enum_adyacency_list)

-- Returns de adyacency list of a variable
buildAdyacencyList :: RangeTable -> (String,(Vertex,Vertex)) -> [String]
buildAdyacencyList range_table var = L.foldr (buildIntEdges var) [] (M.toList range_table) 
    where buildIntEdges (id1,range1) (id2,range2) acc -- buildIntEdges :: (String,(Int,Int)) -> (String,(Int,Int)) -> [String] -> [String]
           | id1 == id2 = acc
           | otherwise = if checkInterference range1 range2 then id2 : acc else acc

-- Attemps to find a k-coloring for a graph using a brute-search algorithm.
bruteSearchGraphColoring :: Graph -> Int -> Maybe (Map Vertex Int)
bruteSearchGraphColoring graph color_num = let vertex_list = reverse (G.vertices graph) -- :: [Vertex], where a Vertex is defined as: type Vertex = Int
                                               color_list = [0..color_num-1] -- color list, each color is represented by an integer.
                                               graphColoring = L.foldr (buildGraphColoring graph color_list) M.empty vertex_list -- try to create a k-coloring, if impossible, return Nothing.
                                               notAssigned = -1 `L.elem` M.elems graphColoring
                                               in (if notAssigned then Nothing else Just graphColoring)

-- Attemps to assign a color from a color list to each vertex in a graph, if possible.
buildGraphColoring :: Graph -> [Int] -> Vertex -> Map Vertex Int -> Map Vertex Int
buildGraphColoring graph color_list vertex color_map = case M.lookup vertex color_map of
                                                  Nothing -> assignColor graph vertex color_list color_map -- ff the vertex has no color assigned, try to assign it one.
                                                  Just x -> color_map -- otherwise, do nothing.
 where assignColor g v c_list m = let neightbours_colors = L.map (fromMaybe (-1) . lookup' m) (getNeighbours v (G.edges g)) -- list of the colors already assigned to the adyacent vertices of the vertex 'v'. :: [Int]
                                      available_colors = c_list L.\\ neightbours_colors -- list diference. [Total Colors] \\ [Already Assigned Colors]
                                      in (if not (L.null available_colors) then M.insert v (head available_colors) m else M.insert v (-1) m) -- if there's at least one available color, assign it, otherwise leave the vertex uncolored.
       getNeighbours v e = [ snd x | x <- e, fst x == v ]                     

-- Auxiliar function for looking up a key in a map.
lookup' :: (Ord k) => Map k a -> k -> Maybe a 
lookup' m x = M.lookup x m

{-toTargetCode :: BasicBlock -> Map String Vertex -> Map Vertex Register -> Map Register String -> [MIPSTargetCode]
toTargetCode basicBlocks enumerationMap registerAssignmentMap registersMap = (L.map (translateTACtoTarget enumerationMap registerAssignmentMap) . ins_list) basicBlocks

translateTACtoTarget :: TAC -> Map String Vertex -> MIPSTargetCode
translateTACtoTarget (Quadruplet binOp (Just (Tok a1)) (Just (Tok a2)) (Just r)) = translateUsingInmediateOperator binOp r a1 a2

translateUsingInmediateOperator :: Operator -> TACField -> TACField -> TACField -> MIPSTargetCode
translateUsingInmediateOperator OSum dest arg1 arg2 = let destRegister = basicgetReg dest
                                                          in 

basicgetReg :: Vertex -> Map String Vertex -> Map Vertex Register -> Maybe Register
-}

createRegisterAssignmentMap :: Map String Vertex -> Map Vertex Register -> Map Register String -> Map String Register
createRegisterAssignmentMap varEnumerationMap graphColoring registersMap = let varsRegistersNumMap = M.map ((M.!) graphColoring) varEnumerationMap
                                                                               in varsRegistersNumMap

-- ################################################################################ --
--------------------------------------------------------------------------------------
------- FUNCTIONS FOR DESCRIPTOR TABLE MANAGEMENT AND TARGET CODE GENERATION ---------
--------------------------------------------------------------------------------------
-- ################################################################################ --
{-
toTargetCode :: FlowGraph -> DescriptorTable -> [Register] -> [MIPSTargetCode]
toTargetCode flowGraph descriptorTable registerList = let basicBlocksList = M.toList flowGraph
                                                          in L.foldl toTC (registerList,descriptorTable,[]) basicBlocksList
                                                          
toTT :: (Int,BasicBlock) -> ([Register],DescriptorTable,[MIPSTargetCode]) -> ([Register],DescriptorTable,[MIPSTargetCode])
toTT (bb_num,bb) (availableRegisters,descriptorTable,targetCode) = let inSet = in_live bb
                                                                       outSet = out_live bb
                                                                       tacList = ins_list bb
                                                                       in L.foldl translateTACtoTarget (availableRegisters,descriptorTable,targetCode) tacList
                                                        
translateTACtoTarget :: TAC -> ([Register],DescriptorTable,[MIPSTargetCode]) -> ([Register],DescriptorTable,[MIPSTargetCode])
translateTACtoTarget (Quadruplet OSum a1 a2 r) (availableRegisters,descriptorTable,targetCode) = translateSum a1 a2 r availableRegisters descriptorTable targetCode

translateSum :: Maybe TACField -> Maybe TACField -> Maybe TACField -> [Register] -> DescriptorTable -> [MIPSTargetCode] -> ([Register],DescriptorTable,[MIPSTargetCode])
translateSum (Just (Tok v1)) (Just (Tok v2)) (Just x) availableRegisters descriptorTable targetCode = let rDest = getReg x descriptorTable
                                                                                                          in case rDest of
                                                                                                               Nothing -> findReg availableRegisters  
                                                                                                               Just r -> let loadArg1 = Lw r v1
                                                                                                                             targetIns = Addi r r v2
                                                                                                                             newTargetCode = (targetIns : loadArg1 : targetCode)  
                                                                                                                             in (availableRegisters,descriptorTable,newTargetCode)

getReg :: Descriptor -> DescriptorTable -> Maybe Register
getReg value descriptorTable = case M.lookup value descriptorTable of
                                    Nothing -> Nothing
                                    Just S.empty -> Nothing
                                    Just l -> let regList = L.map unwrapDescriptorIntoRegister (L.filter checkIfRegister (S.toList l))
                                                  in Just (head regList)
    where checkIfRegister Regi _ = True
          checkIfRegister TField _ = False

unwrapDescriptorIntoRegister :: Descriptor -> Register
unwrapDescriptorIntoRegister (Regi register) = register-}