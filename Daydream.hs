module Daydream(main) where
import Lexer
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.List
import Parser

data Flag =
    Lexer  |
    Parser |
    Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['l'] ["lexer"]  (NoArg Lexer)  "Runs only the lexical analyzer and prints the token list."
    ,Option ['p'] ["parser"] (NoArg Parser) "Runs the syntax analyzer and prints the syntax tree."
    ,Option ['h'] ["help"]   (NoArg Help)   "Prints this help message."
    ]

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

undef :: Token -> Bool
undef (TUndef _ _) = True
undef _            = False

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
        hPutStrLn stderr "Wrong file format (must be .ddr)."
        exitWith (ExitFailure 1)

printTokList :: [Token] -> IO()
printTokList list = mapM_ putStrLn $ map show list

main::IO ()
main = do
    (opts,file) <- getArgs >>= parse
    handle <- openFile file ReadMode  
    s <- hGetContents handle
    let toks = alexScanTokens s
    let inv = filter undef toks
    let val = (inv /= [])
    if val 
        then printTokList inv
        else if Lexer `elem` opts
            then do
                printTokList toks
                parseDdr toks
            else do
                parseDdr toks
