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

import Lexer
import Parser
import SymTable
import TAC

data Flag =
    Lexer        |
    Parser       |
    Table        |
    Intermediate |
    Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['l'] ["lexer"]        (NoArg Lexer)        "Runs only the lexical analyzer and prints the token list."
    ,Option ['p'] ["parser"]       (NoArg Parser)       "Runs the syntax analyzer and prints the syntax tree."
    ,Option ['t'] ["table"]        (NoArg Table)        "Runs the syntax analyzer and prints the symtable."
    ,Option ['h'] ["help"]         (NoArg Help)         "Prints this help message."
    ,Option ['i'] ["intermediate"] (NoArg Intermediate) "Prints three address code"
    ]

-- |Parses the command line options.
parse :: [String] -> IO ([Flag],String)
parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        if Help `elem` args
            then do 
                hPutStrLn stderr (usageInfo header flags)
                exitWith ExitSuccess
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
printTokList :: [Token] -> IO()
printTokList list = mapM_ putStrLn $ map show list

-- | Depending on options, prints token list and lexical errors.
lexOption :: [Token] -> [Token] -> [Flag] -> IO [Token]
lexOption toks inv opts =
    case inv of
        [] -> if Lexer `elem` opts
            then do
                printTokList toks
                exitWith ExitSuccess
            else return toks
        et -> if Lexer `elem` opts
            then do 
                printTokList toks
                putStrLn $ (show . length $ inv) ++ "lexical errors found:"
                printTokList inv
                exitWith (ExitFailure 1)
            else do 
                putStrLn $ (show . length $ inv) ++ "lexical errors found:"
                printTokList inv
                exitWith (ExitFailure 1)

-- | Unwraps the tree from the error.
reportRes :: Show a => Either String a -> [Flag] -> IO a
reportRes (Left e) _ = putStrLn ("Error: " ++ show e) >> exitWith (ExitFailure 1)
reportRes (Right t) opts = if Parser `elem` opts
    then do
        putStrLn (show t)
        exitWith ExitSuccess
    else return t

-- | Process syntax errors.
syntaxErrors :: [String] -> IO()
syntaxErrors es = case (length es) of
    0 -> return ()
    n -> do
        putStrLn $ (show n) ++ " syntax errors found:"
        mapM_ putStrLn es
        exitWith (ExitFailure 1)

-- | Depending on options, may print the symbol table.
printSym :: SymTable -> [Flag] -> IO()
printSym s opts = if Table `elem` opts
    then putStrLn (show s)
    else return ()

-- | Generates TAC for the syntax tree.
generateTAC :: (TAC_convertible a) => a -> SymTable -> [Flag] -> IO ()
generateTAC tree symtable opts = if Intermediate `elem` opts
    then printTAC (evalState (toTAC symtable tree) (0,0,"",""))
    else return ()

main :: IO()
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
    generateTAC t s opts