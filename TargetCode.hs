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
import Data.Maybe (fromMaybe)

-- Data type representing a Basic Block structure
data BasicBlock = Block { block_num :: Int 
                        , ins_list :: [TAC] } deriving (Show)

type RangeTable = Map String (Int,Int)

-- Generates target code
genTargetCode :: [TAC] -> Maybe (Map Int Int)
genTargetCode tac_list = let enum_tac = enumerateTAC tac_list
                             range_table = L.foldl buildRangeTable M.empty enum_tac
                             edge_list = buildInterferenceEdges range_table
                             (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edge_list
                             graphColoring = bruteSearchGraphColoring graph 10
                             in graphColoring 

partitionTACList' :: [TAC] -> [[TAC]]
partitionTACList' tac_list = partitionTACList x xs
    where partition_index = findNextLeaderInstruction tac_list
          (x,xs) = L.splitAt partition_index tac_list

-- Partitions a TAC list, so we can build the Basic Blocks later
partitionTACList :: [TAC] -> [TAC] -> [[TAC]]
partitionTACList [] xs = [xs]
partitionTACList x [] = [x]
partitionTACList x xs = let partition_index = findNextLeaderInstruction (tail xs) + 1
                            (y,ys) = L.splitAt partition_index xs
                            in x : partitionTACList y ys

-- Finds the index for the next leader instruction in a list of TAC
findNextLeaderInstruction :: [TAC] -> Int
findNextLeaderInstruction = fromJustInt . L.findIndex checkIfLabel

-- Checks if a particular TAC structure is a Label
checkIfLabel :: TAC -> Bool
checkIfLabel (Label _) = True
checkIfLabel Quadruplet {} = False
checkIfLabel IfRegister {} = False
    
-- Extracts the value of a Maybe Int
fromJustInt :: Maybe Int -> Int
fromJustInt Nothing = 0
fromJustInt (Just x) = x 

-- Generates a list of Basic Blocks from a list of previously partitioned TAC
genBasicBlocks :: [[TAC]] -> [BasicBlock]
genBasicBlocks l = zipWith (curry genBasicBlockfromTAC) [0 .. length l] l

-- Auxiliar function for converting a TAC into a Basic Block
genBasicBlockfromTAC :: (Int,[TAC]) -> BasicBlock
genBasicBlockfromTAC (n,tac) = Block {block_num=n, ins_list=tac}

-- Prints a list of Basic Blocks in a readable manner
printBasicBlocks :: [BasicBlock] -> IO ()
printBasicBlocks bb_list = let bb_list' = L.map convertBasicBlockToString bb_list
                               in (putStrLn . unlines) bb_list'

-- Converts a Basic Block into a human readable String for later printing
convertBasicBlockToString :: BasicBlock -> String 
convertBasicBlockToString b = let tac_string_list = L.map convertTACtoString (ins_list b)
                                  block_header = "Block #" ++ (show . block_num) b ++ "\n"
                                  in block_header ++ unlines tac_string_list

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
          checkVar (Just (Tok s)) = Nothing
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
buildInterferenceEdges :: RangeTable -> [(String,Int,[Int])]
buildInterferenceEdges range_table = L.map enumerateNode [ (fst var,buildAdyacencyList range_table var) | var <- M.toList range_table ] -- [(String,[String])]
    where enumerateNode (n,l) = let vars_list = M.keys range_table -- enumerateNode :: (String,[String]) -> (String,Int,[Int])
                                    enumeration_map = M.fromList (zip vars_list [0..length vars_list]) 
                                    key = fromMaybe (-1) (M.lookup n enumeration_map)
                                    enum_adyacency_list = L.map (fromMaybe (-1) . lookup' enumeration_map) l
                                    in (n,key,enum_adyacency_list)

-- Returns de adyacency list of a variable
buildAdyacencyList :: RangeTable -> (String,(Int,Int)) -> [String]
buildAdyacencyList range_table var = L.foldr (buildIntEdges var) [] (M.toList range_table) 
    where buildIntEdges (id1,range1) (id2,range2) acc -- buildIntEdges :: (String,(Int,Int)) -> (String,(Int,Int)) -> [String] -> [String]
           | id1 == id2 = acc
           | otherwise = if checkInterference range1 range2 then id2 : acc else acc

bruteSearchGraphColoring :: Graph -> Int -> Maybe (Map Int Int)
bruteSearchGraphColoring graph color_num = let vertex_list = reverse (G.vertices graph) -- :: [Vertex], type Vertex = Int
                                               color_list = [0..color_num-1] -- color list, each color is represented by an integer.
                                               graphColoring = L.foldr (buildGraphColoring graph color_list) M.empty vertex_list -- try to create a k-coloring, if impossible, return Nothing.
                                               notAssigned = -1 `L.elem` M.elems graphColoring
                                               in (if notAssigned then Nothing else Just graphColoring)
                                                
buildGraphColoring :: Graph -> [Int] -> Int -> Map Int Int -> Map Int Int
buildGraphColoring graph color_list vertex color_map = case M.lookup vertex color_map of
                                                  Nothing -> assignColor graph vertex color_list color_map -- ff the vertex has no color assigned, try to assign it one.
                                                  Just x -> color_map -- otherwise, do nothing.
 where assignColor g v c_list m = let neightbours_colors = L.map (fromMaybe (-1) . lookup' m) (getNeighbours v (G.edges g)) -- list of the colors for adyacent vertices to the vertex 'v'. :: [Int]
                                      available_colors = c_list L.\\ neightbours_colors -- list diference.
                                      in (if not (L.null available_colors) then M.insert v (head available_colors) m else M.insert v (-1) m)
       getNeighbours v e = [ snd x | x <- e, fst x == v ]                     

lookup' :: (Ord k) => Map k a -> k -> Maybe a 
lookup' m x = M.lookup x m

