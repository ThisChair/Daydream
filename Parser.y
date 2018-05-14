{
module Parser where
import Lexer
}
%monad { IO }
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
S : Mod Imports Ins                { % putStrLn "S -> Mod Imports Ins" }

Mod : module type              { % putStrLn "Mod -> module type" }
    | {- empty -}              { % putStrLn "Mod -> {- empty -}" }

-- Importaciones
Imports : Imports import Type  { % putStrLn "Imports -> Imports import Type" }
        | {- empty -}          { % putStrLn "Imports -> {- empty -}" }

-- Instrucciones
Ins : Ins In                   { % putStrLn "Ins -> Ins In" }
    | {- empty -}              { % putStrLn "Ins -> {- empty -}" }

In : Body ';'                  { % putStrLn "In -> Body ';'" }
   | Block                     { % putStrLn "In -> Block" }
   | Algebraic                 { % putStrLn "In -> Algebraic" }
   | Selector                  { % putStrLn "In -> Selector" }
   | Iterator                  { % putStrLn "In -> Iterator" }
   | Function                  { % putStrLn "In -> Function" }


Body : Declaration             { % putStrLn "Body -> Declaration" }
     | Assign                  { % putStrLn "Body -> Assign" }
     | return Exp              { % putStrLn "Body -> return Exp" }
     | Print                   { % putStrLn "In -> Print" }
     | PrintLn                 { % putStrLn "In -> PrintLn" }
     | Read                    { % putStrLn "In -> Read " }
     | {- empty -}             { % putStrLn "Body -> {- emtpy -}" }

Print : print Exp              { % putStrLn "Print -> print Exp" }

PrintLn : printLn Exp          { % putStrLn "PrintLn -> printLn Exp" }

Read : read Exp                { % putStrLn "Read -> read Exp" }

-- Bloques
Block : dream Ins wake         { % putStrLn "Block -> dream Ins wake" }

Algebraic : data type dream Sums wake  { % putStrLn "Algebraic -> data type dream Sums wake" }

Sums : Sums Sum                { % putStrLn "Sums -> Sums Sum" }
     | Sum                     { % putStrLn "Sums -> Sum " }

Sum : type '(' Prods ')' ';'   { % putStrLn "Sum -> type '(' Prods ')' ';'" }

Declaration : Type Ids         { % putStrLn "Declaration -> Type Ids" }
            | Type DAssign     { % putStrLn "Declaration -> Type DAssign" }

DAssign : id ',' DAssign ',' RV { % putStrLn "DAssign -> id ',' DAssign ',' RV" }
        | id '=' RV             { % putStrLn "DAssign -> id '=' RV" }

Prods : Prods ',' Prod         { % putStrLn "Prods -> Prods ',' Prod" }
      | Prod                   { % putStrLn "Prods -> Prod" }

Prod : Type id                 { % putStrLn "Prod -> Type id" }

-- Identificadores
Ids : id ',' Ids               { % putStrLn "Ids -> Ids ',' id" }
    | id                       { % putStrLn "Ids -> id" }

Id : id                        {  % putStrLn "Id -> id" }
   | Id '[' Exp ']'            { % putStrLn "Id -> Id '[' Exp ']'" }
   | Id '.' MCall              { % putStrLn "Id -> Id '.' MCall" }

MCall : MCall '.' id           { % putStrLn "MCall -> MCall '.' id" }
      | id                     { % putStrLn "MCall -> id" }

Types : Types ',' Type         { % putStrLn "Types -> Types ',' Type" }
      | Type                   { % putStrLn "Types -> Type" }

Type : type                    { % putStrLn "Type -> type" }
     | '[' Type ']'            { % putStrLn "Type -> '[' Type ']'" }
     | '{' Type ':' num '}'    { % putStrLn "Type -> '{' Type ': num '}'" }
     | '[' Type ':' Type ']'   { % putStrLn "Type -> '[' Type ':' Type ']'" }
     | '(' Types ')'           { % putStrLn "Type -> '(' Types ')'" }

Assign : Id ',' Assign ',' RV  { % putStrLn "Assign -> Id ',' Assign ',' RV" }
       | Id '=' RV             { % putStrLn "Assign -> Id '=' RV" }

RV : Exp    { % putStrLn "RV -> Exp" }
   | Cons   { % putStrLn "RV -> Cons" }

Cons : type '(' ')'      { % putStrLn "Cons -> type '(' ')'" }
     | type '(' Exps ')' { % putStrLn "Cons -> type '(' Exps ')'" }

-- Expresiones
Exp : Exp '+' Exp              { % putStrLn "Exp -> Exp '+' Exp" }
    | Exp '-' Exp              { % putStrLn "Exp -> Exp '-' Exp" }
    | Exp '*' Exp              { % putStrLn "Exp -> Exp '*' Exp" }
    | Exp '/' Exp              { % putStrLn "Exp -> Exp '/' Exp" }
    | Exp '%' Exp              { % putStrLn "Exp -> Exp '%' Exp" }
    | Exp '**' Exp             { % putStrLn "Exp -> Exp '**' Exp" }
    | Exp '//' Exp             { % putStrLn "Exp -> Exp '//' Exp" }
    | Exp '<<' Exp             { % putStrLn "Exp -> Exp '<<' Exp" }
    | Exp '>>' Exp             { % putStrLn "Exp -> Exp '>>' Exp" }
    | Exp '|' Exp              { % putStrLn "Exp -> Exp '|' Exp" }
    | Exp '^' Exp              { % putStrLn "Exp -> Exp '^' Exp" }
    | Exp '&' Exp              { % putStrLn "Exp -> Exp '&' Exp" }
    | Exp '||' Exp             { % putStrLn "Exp -> Exp '||' Exp" }
    | Exp '&&' Exp             { % putStrLn "Exp -> Exp '&&' Exp " }
    | Exp '>' Exp              { % putStrLn "Exp -> Exp '>' Exp" }
    | Exp '<' Exp              { % putStrLn "Exp -> Exp '<' Exp" }
    | Exp '>=' Exp             { % putStrLn "Exp -> Exp '>=' Exp" }
    | Exp '<=' Exp             { % putStrLn "Exp -> Exp '<=' Exp" }
    | Exp '==' Exp             { % putStrLn "Exp -> Exp '==' Exp " }
    | Exp '/=' Exp             { % putStrLn "Exp -> Exp '/=' Exp" }
    | '-' Exp %prec NEG        { % putStrLn "Exp -> '-' Exp %prec NEG" }
    | '!' Exp                  { % putStrLn "Exp -> '!' Exp" }
    | '~' Exp                  { % putStrLn "Exp -> '~' Exp" }
    | Exp '?'                  { % putStrLn "Exp ->  Exp '?'" }
    | '(' Exp ')'              { % putStrLn "Exp -> '(' Exp ')'" }
    | Id                       { % putStrLn "Exp -> Id" }
    | num                      { % putStrLn "Exp -> num" }
    | true                     { % putStrLn "Exp -> true" }
    | false                    { % putStrLn "Exp -> false" }
    | str                      { % putStrLn "Exp -> str" }
    | char                     { % putStrLn "Exp -> char" }
    | List                     { % putStrLn "Exp -> List" }
    | Arr                      { % putStrLn "Exp -> Arr" }
    | Dict                     { % putStrLn "Exp -> Dict" }
    | Tup                      { % putStrLn "Exp -> Tup" }
    | FunCall                  { % putStrLn "Exp -> FunCall" }

Exps : Exps ',' Exp            { % putStrLn "Exps -> Exps ',' Exp" }
     | Exp                     { % putStrLn "Exps -> Exp" }

List : '[' Exps ']'            { % putStrLn "List -> '[' Exps ']'" }
     | '[' ']'                 { % putStrLn "List -> '[' ']'" }

Arr : '{' Exps '}'             { % putStrLn "Arr -> '{' Exps '}'" }
    | '{' '}'                  { % putStrLn "Arr -> '{' '}'" }

Dict : '[' KV ']'              { % putStrLn "Dict -> '[' KV ']'" }

KV : KV ',' Exp ':' Exp        { % putStrLn "KV -> KV ',' Exp ':' Exp" }
   | Exp ':' Exp               { % putStrLn "Exp ':' Exp" }

Tup : '(' Exp ',' Exps ')'     { % putStrLn "Tup -> '(' Exp ',' Exps ')'" }

-- Funciones
FunCall : id '(' Exps ')'      {  % putStrLn "FunCall -> id '(' Exps ')'" }
        | id '(' ')'           {  % putStrLn "FunCall -> id '(' ')'" }

Function : func '(' Type Ret Exp ')' Block         { % putStrLn "Function -> func '(' Type Ret Exp ')' Block" }
         | func '(' Type NoRet Exp ')' Block       { % putStrLn "Function -> func '(' Type NoRet Exp ')' Block" }
         | func '(' '->' Type ')' id '(' ')' Block { % putStrLn "Function -> func '(' '->' Type ')' id '(' ')' Block" }
         | func '(' ')' id '(' ')' Block           { % putStrLn "Function -> func '(' ')' id '(' ')' Block" }

Ret : ',' Type Ret Exp ','     { % putStrLn "Ret -> ',' Type Ret Exp ','" }
    | '->' Type ')' id '('     { % putStrLn "Ret -> '->' Type ')' id '('" }

NoRet : '->' ',' Type NoRet Exp ',' { % putStrLn "NoRet -> '->' ',' Type NoRet Exp ','" }
      | ')' id '('                  { % putStrLn "NoRet -> ')' id '('" }

-- Selectores
Selector : If                  { % putStrLn "Selector -> If" }
         | Case                { % putStrLn "Selector -> Case" }

If : if Exp then In            { % putStrLn "If -> if Exp then In" }
   | if Exp then In else In    { % putStrLn "If -> if Exp then In else In" }

Case : case Exp of Conds ';'   { % putStrLn "Case -> case Exp of Conds ';'" }

-- Condiciones
Conds : Conds Cond             { % putStrLn "Conds -> Conds Cond" }
      | {- empty -}            { % putStrLn "Conds -> {- empty -}" }

Cond : Exp In                  {  % putStrLn "Cond -> Exp In" }

-- Iteradores
Iterator : Indet               { % putStrLn "Iterator -> Indet" }
         | Det                 { % putStrLn "Iterator -> Det" }

Indet : while Exp In           { % putStrLn "Indet -> while Exp In" }

Det : for Type Id from Exp to Exp                     {  % putStrLn "Det -> for Type Id from Exp to Exp" }
    | for Type Id from Exp to Exp if Exp In           {  % putStrLn "Det -> for Type Id from Exp to Exp if Exp In" }
    | for Type Id from Exp to Exp with Exp if Exp In  {  % putStrLn "Det -> for Type Id from Exp to Exp with Exp if Exp In" }
    | for Type Id from Exp to Exp with Exp In         {  % putStrLn "Det -> for Type Id from Exp to Exp with Exp In" }
    | for Type Id in Exp if Exp In                    {  % putStrLn "Det -> for Type Id in Exp if Exp In" }

{
parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError _ = error $ "ERROR"
}
