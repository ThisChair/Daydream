module Daydream(main) where
import Lexer
import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.List
import Parser
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import SymTable
import TAC

data Flag =
    Lexer  |
    Parser |
    Table  |
    Intermediate |
    Help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
    [Option ['l'] ["lexer"]  (NoArg Lexer)  "Runs only the lexical analyzer and prints the token list."
    ,Option ['p'] ["parser"] (NoArg Parser) "Runs the syntax analyzer and prints the syntax tree."
    ,Option ['t'] ["table"]  (NoArg Table)  "Runs the syntax analyzer and prints the symtable."
    ,Option ['i'] ["intermediate"] (NoArg Intermediate) "Runs the syntax analyser, then the intermediate code generator and prints the generated Three-Address Code."  
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

unwrapTree :: Either String a -> IO a
unwrapTree (Right t) = return t

printTokList :: [Token] -> IO()
printTokList list = mapM_ putStrLn $ map show list

reportRes :: Either String a -> IO ()
reportRes (Left e) = putStrLn ("Error: " ++ show e)
reportRes (Right _) = putStrLn ""

reportRes' :: Show a => Either String a -> IO ()
reportRes' (Left e) = putStrLn ("Error: " ++ show e)
reportRes' (Right t) = putStrLn (show t)

main::IO ()
main = do
    (opts,file) <- getArgs >>= parse
    handle <- openFile file ReadMode  
    s <- hGetContents handle
    let toks = alexScanTokens s
    let inv = filter undef toks
    let val = (inv /= [])
    putStrLn "Daydream - Carlos Infante, Daniel Varela - 2018"
    if val 
        then do
            putStrLn $ (show $ length inv) ++ " lexical errors found:"
            printTokList inv
            if Lexer `elem` opts
            then do
                let ftoks = filter (not . undef) toks
                putStrLn $ "Token lists:"
                printTokList ftoks
                ((p,(s,_,_)),w) <- runWriterT (runStateT (runExceptT (parseDdr ftoks)) initialState)
                if Parser `elem` opts
                    then do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes' p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
                    else do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
            else do
                let ftoks = filter (not . undef) toks
                ((p,(s,_,_)),w) <- runWriterT (runStateT (runExceptT (parseDdr ftoks)) initialState)
                if Parser `elem` opts
                    then do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ map show w
                        reportRes' p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
                    else do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
        else if Lexer `elem` opts
            then do
                putStrLn $ "Token lists:"
                printTokList toks
                ((p,(s,_,_)),w) <- runWriterT (runStateT (runExceptT (parseDdr toks)) initialState)
                if Parser `elem` opts
                    then do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes' p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
                    else do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ map show w
                        reportRes p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else putStrLn "OK"
            else do
                ((p,(s,_,_)),w) <- runWriterT (runStateT (runExceptT (parseDdr toks)) initialState)
                if Parser `elem` opts
                    then do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes' p
                        if Table `elem` opts
                            then do
                                putStrLn $ show s
                                if Intermediate `elem` opts
                                    then do
                                        syntaxtree <- unwrapTree p
                                        printTAC (evalState (toTAC s syntaxtree) (0,0,"",""))
                                    else putStrLn "OK"
                            else putStrLn "OK"
                    else do
                        putStrLn $ (show $ length w) ++ " sintax errors found:"
                        mapM_ putStrLn $ w
                        reportRes p
                        if Table `elem` opts
                            then putStrLn $ show s
                            else if Intermediate `elem`opts
                                then do
                                    syntaxtree <- unwrapTree p
                                    printTAC (evalState (toTAC s syntaxtree) (0,0,"",""))
                                else putStrLn "OK"



