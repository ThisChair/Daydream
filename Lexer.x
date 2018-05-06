{
module Main(main) where
import System.IO
import System.Environment
import Data.Char
import Prelude as P
}

%wrapper "posn"

$print  = $printable # [\\\"]
$nothing = ~[]

tokens :-
    $white+                 ;
    \#.*\n                  ;
    \#begin(.*\n*)*\#end    ;
    begin                   { (\p s -> TBegin p) }
    read                    { (\p s -> TRead p) }
    println                 { (\p s -> TPrintLn p) }
    print                   { (\p s -> TPrint p) }
    end                     { (\p s -> TEnd p) }
    if                      { (\p s -> TIf p) }
    then                    { (\p s -> TThen p) }
    else                    { (\p s -> TElse p) }
    while                   { (\p s -> TWhile p) }
    for                     { (\p s -> TFor p) }
    from                    { (\p s -> TFrom p) }
    to                      { (\p s -> TTo p) }
    break                   { (\p s -> TBreak p) }
    continue                { (\p s -> TContinue p) }
    func                    { (\p s -> TFunc p) }
    return                  { (\p s -> TReturn p) }
    \/\=                    { (\p s -> TNotEq p) }
    \&\&                    { (\p s -> TAnd p) }
    \|\|                    { (\p s -> TOr p) }
    \!                      { (\p s -> TNot p) }
    \&                      { (\p s -> TBitAnd p) }
    \|                      { (\p s -> TBitOr p) }
    \^                      { (\p s -> TBitXor p) }
    \<\<                    { (\p s -> TLShift p) }
    \>\>                    { (\p s -> TRShift p) }
    \~                      { (\p s -> TBitNot p) }
    \=\=                    { (\p s -> TEq p) }
    \>\=                    { (\p s -> TGEq p) }
    \<\=                    { (\p s -> TLEq p) }
    \=                      { (\p s -> TAssign p) }
    \+                      { (\p s -> TPlus p) }
    \-                      { (\p s -> TMinus p) }
    \*                      { (\p s -> TStar p) }
    \*\*                    { (\p s -> TDStar p) }
    \/                      { (\p s -> TSlash p) }
    \/\/                    { (\p s -> TDSlash p) }
    \(                      { (\p s -> TOpenP p) }
    \)                      { (\p s -> TCloseP p) }
    \[                      { (\p s -> TOpenB p) }
    \]                      { (\p s -> TCloseB p) }
    \{                      { (\p s -> TOpenC p) }
    \}                      { (\p s -> TCloseC p) }
    \<                      { (\p s -> TLess p) }
    \>                      { (\p s -> TGreat p) }
    \%                      { (\p s -> TPercent p) }
    \,                      { (\p s -> TComma p) }
    \;                      { (\p s -> TSColon p) }
    \:                      { (\p s -> TColon p) }
    \.                      { (\p s -> TPoint p) }
    \?                      { (\p s -> TRef p) }
    \-\>                    { (\p s -> TArrow p) }
    true                    { (\p s -> TTrue p) }
    false                   { (\p s -> TFalse p) }
    [a-z][a-zA-Z0-9_]*\'*   { (\p s -> TIdent p s) }
    [A-Z][a-z]*             { (\p s -> TType p s) }
    [0-9]                   { (\p s -> TNum p (read s)) }
    \"($print | (\\\\) | (\\n) | (\\\"))*\"       
                            { (\p s -> TString p s) }
    \'($print | (\\\\) | (\\n) | (\\\"))\'       
                            { (\p s -> TChar p s) }
    $nothing                {TUndef}


{

-- The token type:
data Token =
    TBegin      AlexPosn            |
    TRead       AlexPosn            |
    TPrintLn    AlexPosn            |
    TPrint      AlexPosn            |
    TEnd        AlexPosn            |
    TIf         AlexPosn            |
    TThen       AlexPosn            |
    TElse       AlexPosn            |
    TWhile      AlexPosn            |
    TFor        AlexPosn            |
    TFrom       AlexPosn            |
    TTo         AlexPosn            |
    TBreak      AlexPosn            |
    TContinue   AlexPosn            |
    TFunc       AlexPosn            |
    TReturn     AlexPosn            |
    TNotEq      AlexPosn            |
    TAnd        AlexPosn            |
    TOr         AlexPosn            |
    TNot        AlexPosn            |
    TBitAnd     AlexPosn            |
    TBitOr      AlexPosn            |
    TBitXor     AlexPosn            |
    TLShift     AlexPosn            |
    TRShift     AlexPosn            |
    TBitNot     AlexPosn            |
    TEq         AlexPosn            |
    TGEq        AlexPosn            |
    TLEq        AlexPosn            |
    TAssign     AlexPosn            |
    TPlus       AlexPosn            |
    TMinus      AlexPosn            |
    TStar       AlexPosn            |
    TDStar      AlexPosn            |
    TSlash      AlexPosn            |
    TDSlash     AlexPosn            |
    TOpenP      AlexPosn            |
    TCloseP     AlexPosn            |
    TOpenB      AlexPosn            |
    TCloseB     AlexPosn            |
    TOpenC      AlexPosn            |
    TCloseC     AlexPosn            |
    TLess       AlexPosn            |
    TGreat      AlexPosn            |
    TPercent    AlexPosn            |
    TComma      AlexPosn            |
    TSColon     AlexPosn            |
    TColon      AlexPosn            |
    TPoint      AlexPosn            |
    TRef        AlexPosn            |
    TArrow      AlexPosn            |
    TTrue       AlexPosn            |
    TFalse      AlexPosn            |
    TIdent      AlexPosn String     |
    TType       AlexPosn String     |
    TNum        AlexPosn Int        |
    TString     AlexPosn String     |
    TChar       AlexPosn String     |
    TUndef      AlexPosn String
    deriving (Eq,Show)

show_pos :: Token -> String
show_pos (TBegin  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRead  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPrintLn  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPrint  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEnd  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIf  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TThen  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TElse  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWhile  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFor  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFrom  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTo  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBreak (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TContinue (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFunc  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TReturn  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNotEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAnd (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOr (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNot (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBitAnd (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBitOr  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBitXor (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLShift (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRShift (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBitNot (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TGEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAssign  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPlus   (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMinus  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TStar  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDStar (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSlash  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDSlash (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenP  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseP  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseB (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenB (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseC (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenC (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLess  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TGreat  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPercent  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TComma  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSColon  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TColon (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPoint (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRef (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TArrow  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTrue  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFalse  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIdent  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TType  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNum  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TString  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TChar  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TUndef (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j

show_token :: Token -> String
show_token tok = show_pos tok ++ ": caracter inesperado" ++ " '" ++ show_val tok ++ "'"

show_val :: Token -> String
show_val (TUndef p s) = s

undef :: Token -> Bool
undef (TUndef p s) = True
undef _            = False

filePath :: [String] -> String
filePath [] = error "No se introdujo un archivo."
filePath (x:y:_) = error "Introduzca un solo argumento."
filePath (x:_) = case reverse x of ('r':'d':'d':'.':_) -> x
                                   (y:_) -> error "Formato de archivo incorrecto."

main::IO ()
main = do
  args <- getArgs
  handle <- openFile (filePath args) ReadMode  
  s <- hGetContents handle  
  let toks = alexScanTokens s
  let inv =  filter undef toks
  let val = (inv == [])
  case val of
      False -> do mapM_ putStrLn $ P.map show_token inv
      True  -> do
                putStr $ "OK"
    
}