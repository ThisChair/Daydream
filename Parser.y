{
module Parser where
import Lexer
import Control.Monad.Trans.Except
import SyntaxTree
import Control.Monad.IO.Class
}
%monad { ParseMonad }
%name parseDdr
%tokentype { Token }
%error { parseError }

%token

    dream      { TDream _ }
    read       { TRead _ }
    printLn    { TPrintLn _ }
    print      { TPrint _ }
    wake       { TWake _ }
    import     { TImport _ }
    if         { TIf _ }
    then       { TThen _ }
    else       { TElse _ }
    while      { TWhile _ }
    for        { TFor _ }
    from       { TFrom _ }
    to         { TTo _ }
    with       { TWith _ }
    in         { TIn _ }
    break      { TBreak _ }
    continue   { TContinue _ }
    func       { TFunc _ }
    return     { TReturn _ }
    data       { TData _ }
    case       { TCase _ }
    of         { TOf _ }
    module     { TModule _ }
    '/='       { TNotEq _ }
    '&&'       { TAnd _ }
    '||'       { TOr _ }
    '!'        { TNot _ }
    '&'        { TBitAnd _ }
    '|'        { TBitOr _ }
    '^'        { TBitXor _ }
    '<<'       { TLShift _ }
    '>>'       { TRShift _ }
    '~'        { TBitNot _ }
    '=='       { TEq _ }
    '>='       { TGEq _ }
    '<='       { TLEq _ }
    '='        { TAssign _ }
    '+'        { TPlus _ }
    '-'        { TMinus _ }
    '*'        { TStar _ }
    '**'       { TDStar _ }
    '/'        { TSlash _ }
    '//'       { TDSlash _ }
    '('        { TOpenP _ }
    ')'        { TCloseP _ }
    '['        { TOpenB _ }
    ']'        { TCloseB _ }
    '{'        { TOpenC _ }
    '}'        { TCloseC _ }
    '<'        { TLess _ }
    '>'        { TGreat _ }
    '%'        { TPercent _ }
    ','        { TComma _ }
    ';'        { TSColon _ }
    ':'        { TColon _ }
    '.'        { TPoint _ }
    '?'        { TRef _ }
    '->'       { TArrow _ }
    true       { TTrue _ }
    false      { TFalse _ }
    id         { TIdent _ _ }
    type       { TType _ _ }
    num        { TNum _ _ }
    str        { TString _ _ }
    char       { TChar _ _ }

%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '/=' '=='
%nonassoc '>=' '>' '<=' '<'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%' '//' '**'
%right NEG '!' '~'
%left '.'

%%

-- Inicio
S : Mod Imports Ins            { % liftIO $ putStrLn "S -> Mod Imports Ins" }

Mod : module type              { % liftIO $ putStrLn "Mod -> module type" }
    | {- empty -}              { % liftIO $ putStrLn "Mod -> {- empty -}" }

-- Importaciones
Imports : Imports import Type  { % liftIO $ putStrLn "Imports -> Imports import Type" }
        | {- empty -}          { % liftIO $ putStrLn "Imports -> {- empty -}" }

-- Instrucciones
Ins : Ins In                   { % liftIO $ putStrLn "Ins -> Ins In" }
    | {- empty -}              { % liftIO $ putStrLn "Ins -> {- empty -}" }

In : Body ';'                  { % liftIO $ putStrLn "In -> Body ';'" }
   | Block                     { % liftIO $ putStrLn "In -> Block" }
   | Algebraic                 { % liftIO $ putStrLn "In -> Algebraic" }
   | Selector                  { % liftIO $ putStrLn "In -> Selector" }
   | Iterator                  { % liftIO $ putStrLn "In -> Iterator" }
   | Function                  { % liftIO $ putStrLn "In -> Function" }


Body : Declaration             { % liftIO $ putStrLn "Body -> Declaration" }
     | Assign                  { % liftIO $ putStrLn "Body -> Assign" }
     | return Exp              { % liftIO $ putStrLn "Body -> return Exp" }
     | Print                   { % liftIO $ putStrLn "In -> Print" }
     | PrintLn                 { % liftIO $ putStrLn "In -> PrintLn" }
     | Read                    { % liftIO $ putStrLn "In -> Read " }
     | {- empty -}             { % liftIO $ putStrLn "Body -> {- emtpy -}" }

Print : print Exp              { % liftIO $ putStrLn "Print -> print Exp" }

PrintLn : printLn Exp          { % liftIO $ putStrLn "PrintLn -> printLn Exp" }

Read : read Exp                { % liftIO $ putStrLn "Read -> read Exp" }

-- Bloques
Block : dream Ins wake         { % liftIO $ putStrLn "Block -> dream Ins wake" }

Algebraic : data type dream Sums wake  { % liftIO $ putStrLn "Algebraic -> data type dream Sums wake" }

Sums : Sums Sum                { % liftIO $ putStrLn "Sums -> Sums Sum" }
     | Sum                     { % liftIO $ putStrLn "Sums -> Sum " }

Sum : type '(' Prods ')' ';'   { % liftIO $ putStrLn "Sum -> type '(' Prods ')' ';'" }

Declaration : Type Ids         { % liftIO $ putStrLn "Declaration -> Type Ids" }
            | Type DAssign     { % liftIO $ putStrLn "Declaration -> Type DAssign" }

DAssign : id ',' DAssign ',' RV { % liftIO $ putStrLn "DAssign -> id ',' DAssign ',' RV" }
        | id '=' RV             { % liftIO $ putStrLn "DAssign -> id '=' RV" }

Prods : Prods ',' Prod         { % liftIO $ putStrLn "Prods -> Prods ',' Prod" }
      | Prod                   { % liftIO $ putStrLn "Prods -> Prod" }

Prod : Type id                 { % liftIO $ putStrLn "Prod -> Type id" }

-- Identificadores
Ids : Ids ',' id               { % liftIO $ putStrLn "Ids -> Ids ',' id" }
    | id                       { % liftIO $ putStrLn "Ids -> id" }

Id : id                        {  % liftIO $ putStrLn "Id -> id" }
   | Id '[' Exp ']'            { % liftIO $ putStrLn "Id -> Id '[' Exp ']'" }
   | Id '.' MCall              { % liftIO $ putStrLn "Id -> Id '.' MCall" }

MCall : MCall '.' id           { % liftIO $ putStrLn "MCall -> MCall '.' id" }
      | id                     { % liftIO $ putStrLn "MCall -> id" }

Types : Types ',' Type         { % liftIO $ putStrLn "Types -> Types ',' Type" }
      | Type                   { % liftIO $ putStrLn "Types -> Type" }

Type : type                    { % liftIO $ putStrLn "Type -> type" }
     | '[' Type ']'            { % liftIO $ putStrLn "Type -> '[' Type ']'" }
     | '{' Type ':' num '}'    { % liftIO $ putStrLn "Type -> '{' Type ': num '}'" }
     | '[' Type ':' Type ']'   { % liftIO $ putStrLn "Type -> '[' Type ':' Type ']'" }
     | '(' Types ')'           { % liftIO $ putStrLn "Type -> '(' Types ')'" }

Assign : Id ',' Assign ',' RV  { % liftIO $ putStrLn "Assign -> Id ',' Assign ',' RV" }
       | Id '=' RV             { % liftIO $ putStrLn "Assign -> Id '=' RV" }

RV : Exp    { % liftIO $ putStrLn "RV -> Exp" }
   | Cons   { % liftIO $ putStrLn "RV -> Cons" }

Cons : type '(' ')'      { % liftIO $ putStrLn "Cons -> type '(' ')'" }
     | type '(' Exps ')' { % liftIO $ putStrLn "Cons -> type '(' Exps ')'" }

-- Expresiones
Exp : Exp '+' Exp              { % liftIO $ putStrLn "Exp -> Exp '+' Exp" }
    | Exp '-' Exp              { % liftIO $ putStrLn "Exp -> Exp '-' Exp" }
    | Exp '*' Exp              { % liftIO $ putStrLn "Exp -> Exp '*' Exp" }
    | Exp '/' Exp              { % liftIO $ putStrLn "Exp -> Exp '/' Exp" }
    | Exp '%' Exp              { % liftIO $ putStrLn "Exp -> Exp '%' Exp" }
    | Exp '**' Exp             { % liftIO $ putStrLn "Exp -> Exp '**' Exp" }
    | Exp '//' Exp             { % liftIO $ putStrLn "Exp -> Exp '//' Exp" }
    | Exp '<<' Exp             { % liftIO $ putStrLn "Exp -> Exp '<<' Exp" }
    | Exp '>>' Exp             { % liftIO $ putStrLn "Exp -> Exp '>>' Exp" }
    | Exp '|' Exp              { % liftIO $ putStrLn "Exp -> Exp '|' Exp" }
    | Exp '^' Exp              { % liftIO $ putStrLn "Exp -> Exp '^' Exp" }
    | Exp '&' Exp              { % liftIO $ putStrLn "Exp -> Exp '&' Exp" }
    | Exp '||' Exp             { % liftIO $ putStrLn "Exp -> Exp '||' Exp" }
    | Exp '&&' Exp             { % liftIO $ putStrLn "Exp -> Exp '&&' Exp " }
    | Exp '>' Exp              { % liftIO $ putStrLn "Exp -> Exp '>' Exp" }
    | Exp '<' Exp              { % liftIO $ putStrLn "Exp -> Exp '<' Exp" }
    | Exp '>=' Exp             { % liftIO $ putStrLn "Exp -> Exp '>=' Exp" }
    | Exp '<=' Exp             { % liftIO $ putStrLn "Exp -> Exp '<=' Exp" }
    | Exp '==' Exp             { % liftIO $ putStrLn "Exp -> Exp '==' Exp " }
    | Exp '/=' Exp             { % liftIO $ putStrLn "Exp -> Exp '/=' Exp" }
    | '-' Exp %prec NEG        { % liftIO $ putStrLn "Exp -> '-' Exp %prec NEG" }
    | '!' Exp                  { % liftIO $ putStrLn "Exp -> '!' Exp" }
    | '~' Exp                  { % liftIO $ putStrLn "Exp -> '~' Exp" }
    | Exp '?'                  { % liftIO $ putStrLn "Exp ->  Exp '?'" }
    | '(' Exp ')'              { % liftIO $ putStrLn "Exp -> '(' Exp ')'" }
    | Id                       { % liftIO $ putStrLn "Exp -> Id" }
    | num                      { % liftIO $ putStrLn "Exp -> num" }
    | true                     { % liftIO $ putStrLn "Exp -> true" }
    | false                    { % liftIO $ putStrLn "Exp -> false" }
    | str                      { % liftIO $ putStrLn "Exp -> str" }
    | char                     { % liftIO $ putStrLn "Exp -> char" }
    | List                     { % liftIO $ putStrLn "Exp -> List" }
    | Arr                      { % liftIO $ putStrLn "Exp -> Arr" }
    | Dict                     { % liftIO $ putStrLn "Exp -> Dict" }
    | Tup                      { % liftIO $ putStrLn "Exp -> Tup" }
    | FunCall                  { % liftIO $ putStrLn "Exp -> FunCall" }

Exps : Exps ',' Exp            { % liftIO $ putStrLn "Exps -> Exps ',' Exp" }
     | Exp                     { % liftIO $ putStrLn "Exps -> Exp" }

List : '[' Exps ']'            { % liftIO $ putStrLn "List -> '[' Exps ']'" }
     | '[' ']'                 { % liftIO $ putStrLn "List -> '[' ']'" }

Arr : '{' Exps '}'             { % liftIO $ putStrLn "Arr -> '{' Exps '}'" }
    | '{' '}'                  { % liftIO $ putStrLn "Arr -> '{' '}'" }

Dict : '[' KV ']'              { % liftIO $ putStrLn "Dict -> '[' KV ']'" }

KV : KV ',' Exp ':' Exp        { % liftIO $ putStrLn "KV -> KV ',' Exp ':' Exp" }
   | Exp ':' Exp               { % liftIO $ putStrLn "Exp ':' Exp" }

Tup : '(' Exp ',' Exps ')'     { % liftIO $ putStrLn "Tup -> '(' Exp ',' Exps ')'" }

-- Funciones
FunCall : id '(' Exps ')'      {  % liftIO $ putStrLn "FunCall -> id '(' Exps ')'" }
        | id '(' ')'           {  % liftIO $ putStrLn "FunCall -> id '(' ')'" }

Function : func '(' Type Ret Exp ')' Block         { % liftIO $ putStrLn "Function -> func '(' Type Ret Exp ')' Block" }
         | func '(' Type NoRet Exp ')' Block       { % liftIO $ putStrLn "Function -> func '(' Type NoRet Exp ')' Block" }
         | func '(' '->' Type ')' id '(' ')' Block { % liftIO $ putStrLn "Function -> func '(' '->' Type ')' id '(' ')' Block" }
         | func '(' ')' id '(' ')' Block           { % liftIO $ putStrLn "Function -> func '(' ')' id '(' ')' Block" }

Ret : ',' Type Ret Exp ','     { % liftIO $ putStrLn "Ret -> ',' Type Ret Exp ','" }
    | '->' Type ')' id '('     { % liftIO $ putStrLn "Ret -> '->' Type ')' id '('" }

NoRet : '->' ',' Type NoRet Exp ',' { % liftIO $ putStrLn "NoRet -> '->' ',' Type NoRet Exp ','" }
      | ')' id '('                  { % liftIO $ putStrLn "NoRet -> ')' id '('" }

-- Selectores
Selector : If                  { % liftIO $ putStrLn "Selector -> If" }
         | Case                { % liftIO $ putStrLn "Selector -> Case" }

If : if Exp then In            { % liftIO $ putStrLn "If -> if Exp then In" }
   | if Exp then In else In    { % liftIO $ putStrLn "If -> if Exp then In else In" }

Case : case Exp of Conds ';'   { % liftIO $ putStrLn "Case -> case Exp of Conds ';'" }

-- Condiciones
Conds : Conds Cond             { % liftIO $ putStrLn "Conds -> Conds Cond" }
      | {- empty -}            { % liftIO $ putStrLn "Conds -> {- empty -}" }

Cond : Exp In                  {  % liftIO $ putStrLn "Cond -> Exp In" }

-- Iteradores
Iterator : Indet               { % liftIO $ putStrLn "Iterator -> Indet" }
         | Det                 { % liftIO $ putStrLn "Iterator -> Det" }

Indet : while Exp In           { % liftIO $ putStrLn "Indet -> while Exp In" }

Det : for Type Id from Exp to Exp                     {  % liftIO $ putStrLn "Det -> for Type Id from Exp to Exp" }
    | for Type Id from Exp to Exp if Exp In           {  % liftIO $ putStrLn "Det -> for Type Id from Exp to Exp if Exp In" }
    | for Type Id from Exp to Exp with Exp if Exp In  {  % liftIO $ putStrLn "Det -> for Type Id from Exp to Exp with Exp if Exp In" }
    | for Type Id from Exp to Exp with Exp In         {  % liftIO $ putStrLn "Det -> for Type Id from Exp to Exp with Exp In" }
    | for Type Id in Exp if Exp In                    {  % liftIO $ putStrLn "Det -> for Type Id in Exp if Exp In" }

{
parseError [] = throwE "Unexpected ending."
parseError _ = throwE "Unexpected token."
}
