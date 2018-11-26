{-|
Module: TargetCode
Authors : Carlos Infante,
          Daniel Varela
        
Everything concerning the generation and manipulation of target code
-}

module TargetCode where

import TAC

import Data.Graph as G
import Data.List as L
import Data.Map.Strict as M

-- Data type representing a Basic Block structure
data BasicBlock = Block { block_num :: Int 
                        , ins_list :: [TAC] } deriving (Show)

type RangeTable = Map String (Int,Int)

-- Generates target code
genTargetCode :: [TAC] -> RangeTable
genTargetCode tac_list = L.foldl buildRangeTable M.empty enum_tac
    where enum_tac = enumerateTAC tac_list
--genTargetCode tac_list = genBasicBlocks (partitionTACList' tac_list)

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
findNextLeaderInstruction ins_list = (fromJustInt . L.findIndex (checkIfLabel)) ins_list

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
genBasicBlocks l = L.map genBasicBlockfromTAC (zip [0..length l] l)

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

enumerateTAC :: [TAC] -> [(Int,TAC)]
enumerateTAC tac_list = zip [0..length tac_list] tac_list

printEnumeratedTAC :: [(Int,TAC)] -> IO ()
printEnumeratedTAC enum_tac_list = let enum_tac_string_list = L.map convertEnumeratedTACToString  enum_tac_list
                                       in (putStrLn . unlines) enum_tac_string_list

convertEnumeratedTACToString :: (Int,TAC) -> String
convertEnumeratedTACToString (num,tac) = show num ++ " :: " ++ convertTACtoString tac

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
          checkVar Nothing = Nothing

checkInterference :: (Int,Int) -> (Int,Int) -> Bool
checkInterference (begin1,end1) (begin2,end2) = let range1 = [begin1..end1]
                                                    range2 = [begin2..end2]
                                                    intersection = range1 `L.intersect` range2
                                                    in not (null intersection)
