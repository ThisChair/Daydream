{
module Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$print = $printable # [\\\"]

tokens :-
    $white+                 ; -- Ignore whitespaces
    \#.*\n                  ; -- Ignore one line comments
    \#dream(.*\n*)*\#wake   ; -- Ignore multi line comments
    -- Reserved words
    dream                   { (\p s -> TDream p) }
    read                    { (\p s -> TRead p) }
    println                 { (\p s -> TPrintLn p) }
    print                   { (\p s -> TPrint p) }
    wake                    { (\p s -> TWake p) }
    import                  { (\p s -> TImport p) }
    if                      { (\p s -> TIf p) }
    then                    { (\p s -> TThen p) }
    else                    { (\p s -> TElse p) }
    while                   { (\p s -> TWhile p) }
    for                     { (\p s -> TFor p) }
    from                    { (\p s -> TFrom p) }
    to                      { (\p s -> TTo p) }
    with                    { (\p s -> TWith p) }
    in                      { (\p s -> TIn p) }
    break                   { (\p s -> TBreak p) }
    continue                { (\p s -> TContinue p) }
    func                    { (\p s -> TFunc p) }
    return                  { (\p s -> TReturn p) }
    data                    { (\p s -> TData p) }
    case                    { (\p s -> TCase p) }
    of                      { (\p s -> TOf p) }
    -- Symbols
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
    -- Literals
    true                    { (\p s -> TTrue p) }
    false                   { (\p s -> TFalse p) }
    [a-z][a-zA-Z0-9_]*\'*   { (\p s -> TIdent p s) }
    [A-Z][a-z]*             { (\p s -> TType p s) }
    digit+(\.$digit+)?       { (\p s -> TNum p s) }
    \"($print | (\\\\) | (\\n) | (\\\"))*\"       
                            { (\p s -> TString p s) }
    \'($print | (\\\\) | (\\n) | (\\\"))\'       
                            { (\p s -> TChar p s) }
    .                       {TUndef}


{

data Token =
    TDream      { tokenPos :: AlexPosn }                     |
    TRead       { tokenPos :: AlexPosn }                     |
    TPrintLn    { tokenPos :: AlexPosn }                     |
    TPrint      { tokenPos :: AlexPosn }                     |
    TWake       { tokenPos :: AlexPosn }                     |
    TImport     { tokenPos :: AlexPosn }                     |
    TIf         { tokenPos :: AlexPosn }                     |
    TThen       { tokenPos :: AlexPosn }                     |
    TElse       { tokenPos :: AlexPosn }                     |
    TWhile      { tokenPos :: AlexPosn }                     |
    TFor        { tokenPos :: AlexPosn }                     |
    TFrom       { tokenPos :: AlexPosn }                     |
    TTo         { tokenPos :: AlexPosn }                     |
    TWith       { tokenPos :: AlexPosn }                     |
    TIn         { tokenPos :: AlexPosn }                     |
    TBreak      { tokenPos :: AlexPosn }                     |
    TContinue   { tokenPos :: AlexPosn }                     |
    TFunc       { tokenPos :: AlexPosn }                     |
    TReturn     { tokenPos :: AlexPosn }                     |
    TData       { tokenPos :: AlexPosn }                     |
    TCase       { tokenPos :: AlexPosn }                     |
    TOf         { tokenPos :: AlexPosn }                     |
    TNotEq      { tokenPos :: AlexPosn }                     |
    TAnd        { tokenPos :: AlexPosn }                     |
    TOr         { tokenPos :: AlexPosn }                     |
    TNot        { tokenPos :: AlexPosn }                     |
    TBitAnd     { tokenPos :: AlexPosn }                     |
    TBitOr      { tokenPos :: AlexPosn }                     |
    TBitXor     { tokenPos :: AlexPosn }                     |
    TLShift     { tokenPos :: AlexPosn }                     |
    TRShift     { tokenPos :: AlexPosn }                     |
    TBitNot     { tokenPos :: AlexPosn }                     |
    TEq         { tokenPos :: AlexPosn }                     |
    TGEq        { tokenPos :: AlexPosn }                     |
    TLEq        { tokenPos :: AlexPosn }                     |
    TAssign     { tokenPos :: AlexPosn }                     |
    TPlus       { tokenPos :: AlexPosn }                     |
    TMinus      { tokenPos :: AlexPosn }                     |
    TStar       { tokenPos :: AlexPosn }                     |
    TDStar      { tokenPos :: AlexPosn }                     |
    TSlash      { tokenPos :: AlexPosn }                     |
    TDSlash     { tokenPos :: AlexPosn }                     |
    TOpenP      { tokenPos :: AlexPosn }                     |
    TCloseP     { tokenPos :: AlexPosn }                     |
    TOpenB      { tokenPos :: AlexPosn }                     |
    TCloseB     { tokenPos :: AlexPosn }                     |
    TOpenC      { tokenPos :: AlexPosn }                     |
    TCloseC     { tokenPos :: AlexPosn }                     |
    TLess       { tokenPos :: AlexPosn }                     |
    TGreat      { tokenPos :: AlexPosn }                     |
    TPercent    { tokenPos :: AlexPosn }                     |
    TComma      { tokenPos :: AlexPosn }                     |
    TSColon     { tokenPos :: AlexPosn }                     |
    TColon      { tokenPos :: AlexPosn }                     |
    TPoint      { tokenPos :: AlexPosn }                     |
    TRef        { tokenPos :: AlexPosn }                     |
    TArrow      { tokenPos :: AlexPosn }                     |
    TTrue       { tokenPos :: AlexPosn }                     |
    TFalse      { tokenPos :: AlexPosn }                     |
    TIdent      { tokenPos :: AlexPosn, tokenVal :: String } |
    TType       { tokenPos :: AlexPosn, tokenVal :: String } |
    TNum        { tokenPos :: AlexPosn, tokenVal :: String } |
    TString     { tokenPos :: AlexPosn, tokenVal :: String } |
    TChar       { tokenPos :: AlexPosn, tokenVal :: String } |
    TUndef      { tokenPos :: AlexPosn, tokenVal :: String }
    deriving (Eq)

instance Show Token where
    show (TDream    (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'dream\'"
    show (TRead     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'read\'"
    show (TPrintLn  (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'printLn\'"
    show (TPrint    (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'print\'"
    show (TWake     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'wake\'"
    show (TImport   (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'import\'"
    show (TIf       (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'if\'"
    show (TThen     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'then\'"
    show (TElse     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'else\'"
    show (TWhile    (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'while\'"
    show (TFor      (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'for\'"
    show (TFrom     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'from\'"
    show (TWith     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'with\'"
    show (TIn       (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'in\'"
    show (TBreak    (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'break\'"
    show (TContinue (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'continue\'"
    show (TFunc     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'func\'"
    show (TReturn   (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'return\'"
    show (TData     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'data\'"
    show (TCase     (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'case\'"
    show (TOf       (AlexPn _ i j)) = (showPos i j) ++ " - Reserved word: \'of\'"
    show (TNotEq    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'/=\'"
    show (TAnd      (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'&&\'"
    show (TOr       (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'||\'"
    show (TNot      (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'!\'"
    show (TBitAnd   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'&\'"
    show (TBitOr    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'|\'"
    show (TBitXor   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'^\'"
    show (TLShift   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'<<\'"
    show (TRShift   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'>>\'"
    show (TBitNot   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'~\'"
    show (TEq       (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'==\'"
    show (TGEq      (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'>=\'"
    show (TLEq      (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'<=\'"
    show (TAssign   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'=\'"
    show (TPlus     (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'+\'"
    show (TMinus    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'-\'"
    show (TStar     (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'*\'"
    show (TDStar    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'**\'"
    show (TSlash    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'/\'"
    show (TDSlash   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'//\'"
    show (TOpenP    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'(\'"
    show (TCloseP   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \')\'"
    show (TOpenB    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'[\'"
    show (TCloseB   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \']\'"
    show (TOpenC    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'{\'"
    show (TCloseC   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'}\'"
    show (TLess     (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'<\'"
    show (TGreat    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'>\'"
    show (TPercent  (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'%\'"
    show (TComma    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \',\'"
    show (TSColon   (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \';\'"
    show (TColon    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \':\'"
    show (TPoint    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'.\'"
    show (TRef      (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'?\'"
    show (TArrow    (AlexPn _ i j)) = (showPos i j) ++ " - Symbol: \'->\'"
    show (TTrue     (AlexPn _ i j)) = (showPos i j) ++ " - Literal: \'true\'"
    show (TFalse    (AlexPn _ i j)) = (showPos i j) ++ " - Literal: \'false'"
    show (TIdent    (AlexPn _ i j) s) = (showPos i j) ++ " - Id: " ++ s
    show (TType     (AlexPn _ i j) s) = (showPos i j) ++ " - Type: " ++ s
    show (TNum      (AlexPn _ i j) s) = (showPos i j) ++ " - Number: " ++ s
    show (TString   (AlexPn _ i j) s) = (showPos i j) ++ " - String: \"" ++ s ++ "\""
    show (TChar     (AlexPn _ i j) s) = (showPos i j) ++ " - Char: \'" ++ s ++ "\'"
    show (TUndef    (AlexPn _ i j) s) = (showPos i j) ++ " - Unexpected token: " ++ s

showPos :: Int -> Int -> String
showPos i j = "Row " ++ show i ++ ", Column " ++ show j
}
