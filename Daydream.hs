{-|
Module : Daydream
Authors : Carlos Infante
          Daniel Varela

Main program for Daydream language.
-}

module Daydream(main) where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.List
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad(when)
import Data.Map.Strict (empty,Map)

import Lexer
import Parser
import SymTable
import TAC
import TargetCode

data Flag =
    Lexer        |
    Parser       |
    Table        |
    Intermediate |
    Target       | 
    Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['l'] ["lexer"]        (NoArg Lexer)        "Runs only the lexical analyzer and prints the token list."
    ,Option ['p'] ["parser"]       (NoArg Parser)       "Runs the syntax analyzer and prints the syntax tree."
    ,Option ['t'] ["table"]        (NoArg Table)        "Runs the syntax analyzer and prints the symtable."
    ,Option ['h'] ["help"]         (NoArg Help)         "Prints this help message."
    ,Option ['i'] ["intermediate"] (NoArg Intermediate) "Prints three address code."
    ,Option ['f'] ["target"]       (NoArg Target)       "Prints target code."
    ]

-- |Parses the command line options.
parse :: [String] -> IO ([Flag],String)
parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> if Help `elem` args
        then do 
            hPutStrLn stderr (usageInfo header flags)
            exitSuccess
        else do
            file <- filePath fs
            return (nub args, file)
    (_,_,errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header = "Usage: daydream [options] [file]"

-- | Returns true if a token is undefined.
undef :: Token -> Bool
undef (TUndef _ _) = True
undef _            = False

-- | Checks .ddr file extension.
filePath :: [String] -> IO String
filePath [] = do
    hPutStrLn stderr "No file was entered."
    exitWith (ExitFailure 1)
filePath (x:y:_) = do
    hPutStrLn stderr "Enter only one file."
    exitWith (ExitFailure 1)
filePath (x:_) = case reverse x of
    ('r':'d':'d':'.':_) -> return x
    (y:_) -> do
        hPutStrLn stderr "Wrong file extension (must be .ddr)."
        exitWith (ExitFailure 1)

-- | Prints a token list.
printTokList :: [Token] -> IO ()
printTokList = mapM_ print

-- | Depending on options, prints token list and lexical errors.
lexOption :: [Token] -> [Token] -> [Flag] -> IO [Token]
lexOption toks inv opts =
    case inv of
        [] -> if Lexer `elem` opts
            then do
                printTokList toks
                exitSuccess
            else return toks
        et -> do 
            when (Lexer `elem` opts) $ printTokList toks
            putStrLn $ (show . length $ inv) ++ "lexical errors found:"
            printTokList inv
            exitWith (ExitFailure 1)

-- | Unwraps the tree from the error.
reportRes :: Show a => Either String a -> [Flag] -> IO a
reportRes (Left e) _ = putStrLn ("Error: " ++ show e) >> exitWith (ExitFailure 1)
reportRes (Right t) opts = if Parser `elem` opts
    then do
        print t
        exitSuccess
    else return t

-- | Process syntax errors.
syntaxErrors :: [String] -> IO()
syntaxErrors es = case length es of
    0 -> return ()
    n -> do
        putStrLn $ show n ++ " syntax errors found:"
        mapM_ putStrLn es
        exitWith (ExitFailure 1)

-- | Depending on options, may print the symbol table.
printSym :: SymTable -> [Flag] -> IO ()
printSym s opts = when (Table `elem` opts) $ print s

-- | Generates TAC for the syntax tree.
generateTAC :: (TACConvertible a) => a -> SymTable -> [Flag] -> ([TAC],(Int,Int,String,String,String,Map String Integer))
generateTAC tree symtable opts = runState (toTAC symtable tree) (0,-1,"","","",empty)

printTAC' :: [TAC] -> [Flag] -> IO ()
printTAC' tac_list opts = when (Intermediate `elem` opts) $ printTAC tac_list


-- | Generates Target code for the Three Adress Code.
generateTargetCode :: Map String Integer -> [TAC] -> [Flag] -> IO ()
generateTargetCode wm tac_list opts = when (Target `elem` opts) ptc
  where
    ptc = (printMIPSCode . genTargetCode wm . filterTACList . reverse) tac_list


main :: IO ()
main = do
    (opts,file) <- getArgs >>= parse
    handle <- openFile file ReadMode  
    s <- hGetContents handle
    let toks = alexScanTokens s
    let inv = filter undef toks
    putStrLn "Daydream - Carlos Infante, Daniel Varela - 2018\n\n"
    toks <- lexOption toks inv opts
    ((p,(s,_,_)),w) <- runWriterT (runStateT (runExceptT (parseDdr toks)) initialState)
    syntaxErrors w
    t <- reportRes p opts
    printSym s opts
    let (tac_list,(_,_,_,_,_,wm)) = generateTAC t s opts
        in do 
            printTAC' tac_list opts
            generateTargetCode wm tac_list opts
