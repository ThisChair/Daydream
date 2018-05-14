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
    println    { TPrintLn _ }
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
S : Imports Ins                { % putStrLn "S : Imports Ins"  }

-- Importaciones
Imports : Imports import Type  { % putStrLn ""  }
        | {- empty -}          { % putStrLn "" }

-- Instrucciones
Ins : Ins In                   {  % putStrLn "" }
    | {- empty -}              {  % putStrLn "" }

In : Body ';'                  {  % putStrLn "" }
   | Block                     {  % putStrLn "" }
   | Algebraic                 {  % putStrLn "" }

Body : Declaration             {  % putStrLn "" }
     | Assign                  {  % putStrLn "" }
     | Selector                {  % putStrLn "" }
     | Iterator                {  % putStrLn "" }
     | Function                { % putStrLn "" }
     | return Exp              { % putStrLn "" }
     | {- empty -}             {  % putStrLn "" }

Block : dream Ins wake         {  % putStrLn ""  }

Algebraic : data type dream Sums wake  { putStrLn "Alg"  }

Sums : Sums Sum                {  % putStrLn "" }
     | Sum                     { % putStrLn ""  }

Sum : type '(' Prods ')' ';'   {  % putStrLn "" }

Declaration : Type Ids         { % putStrLn ""  }
            | Type DAssign     { % putStrLn ""  }

DAssign : id ',' DAssign ',' RV { % putStrLn ""  }
        | id '=' RV             { % putStrLn "" }

Prods : Prods ',' Prod         { % putStrLn ""  }
      | Prod                   { % putStrLn ""  }

Prod : Type id                 { % putStrLn ""  }

-- Identificadores
Ids : Ids ',' id               { % putStrLn ""  }
    | id                       { % putStrLn "" }

Id : id                        {  % putStrLn "" }
   | Id '[' Exp ']'            { % putStrLn ""  }
   | Id '.' MCall              { % putStrLn ""  }

MCall : MCall '.' id           { % putStrLn "" }
      | id                     { % putStrLn "" } 

Types : Types ';' Type         { % putStrLn "" }
      | Type                   { % putStrLn "" } 

Type : type                    { % putStrLn "" } 
     | '[' Type ']'            { % putStrLn ""  }
     | '{' Type ':' num '}'    {  % putStrLn "" }
     | '[' Type ':' Type ']'   { % putStrLn ""   }
     | '(' Types ')'           { % putStrLn "" }

Assign : Id ',' Assign ',' RV  { % putStrLn ""  }
       | Id '=' RV             { % putStrLn ""  }

RV : Exp    { % putStrLn "" }
   | Cons   { % putStrLn ""  }

Cons : type '(' ')'      { % putStrLn ""  }
     | type '(' Exps ')' { % putStrLn ""  }

-- Expresiones
Exp : Exp '+' Exp              { % putStrLn ""  }
    | Exp '-' Exp              { % putStrLn ""  } 
    | Exp '*' Exp              { % putStrLn ""  } 
    | Exp '/' Exp              { % putStrLn "" }
    | Exp '%' Exp              { % putStrLn "" }
    | Exp '**' Exp             { % putStrLn "" }
    | Exp '//' Exp             { % putStrLn "" }
    | Exp '<<' Exp             {  % putStrLn "" }
    | Exp '>>' Exp             { % putStrLn ""  }
    | Exp '|' Exp              { % putStrLn ""  }
    | Exp '&' Exp              { % putStrLn ""  }
    | Exp '^' Exp              {  % putStrLn "" }
    | Exp '||' Exp             { % putStrLn "" }
    | Exp '&&' Exp             { % putStrLn "" }
    | Exp '>' Exp              { % putStrLn ""  }
    | Exp '<' Exp              { % putStrLn ""  }
    | Exp '>=' Exp             { % putStrLn "" }
    | Exp '<=' Exp             { % putStrLn "" }
    | Exp '==' Exp             { % putStrLn "" }
    | Exp '/=' Exp             {  % putStrLn "" }
    | '-' Exp %prec NEG        { % putStrLn "" }
    | '!' Exp                  { % putStrLn "" }
    | '~' Exp                  { % putStrLn "" }
    | Exp '?'                  { % putStrLn "" }
    | '(' Exp ')'              { % putStrLn "" }
    | Id                       { % putStrLn "" }
    | num                      { % putStrLn "" }
    | true                     { % putStrLn "" }
    | false                    { % putStrLn "" }
    | str                      { % putStrLn "" }
    | char                     { % putStrLn "" }
    | List                     { % putStrLn "" }
    | Arr                      { % putStrLn "" }
    | Dict                     { % putStrLn "" }
    | Tup                      { % putStrLn "" }
    | FunCall                  { % putStrLn "" }

Exps : Exps ',' Exp            {  % putStrLn "" }
     | Exp                     {  % putStrLn "" }

List : '[' Exps ']'            {  % putStrLn "" }
     | '[' ']'                 {  % putStrLn "" }

Arr : '{' Exps '}'             { % putStrLn ""  }
    | '{' '}'                  { % putStrLn "" }

Dict : '[' KV ']'             {  % putStrLn "" }

KV : KV ',' Exp ':' Exp        { % putStrLn ""  }
   | Exp ':' Exp               { % putStrLn ""  }

Tup : '(' Exps ',' Exp ')'     { % putStrLn ""  }

-- Funciones
FunCall : id '(' Exps ')'      {  % putStrLn ""  }
        | id '(' ')'           {  % putStrLn ""  }

Function : func '(' Type Ret Exp ')' Block         { % putStrLn ""  }
         | func '(' Type NoRet Exp ')' Block       { % putStrLn ""  }
         | func '(' '->' Type ')' id '(' ')' Block { % putStrLn ""  }
         | func '(' ')' id '(' ')' Block           { % putStrLn ""  }

Ret : ',' Type Ret Exp ','     { % putStrLn ""  }
    | '->' Type ')' id '('     { % putStrLn ""  }

NoRet : '->' ',' Type NoRet Exp ',' { % putStrLn ""  }
      | ')' id '('                  { % putStrLn ""  }

-- Selectores
Selector : If                  { % putStrLn ""  }
         | Case                { % putStrLn ""  }

If : if Exp then In            { % putStrLn "" }
   | if Exp then In else In    {  % putStrLn ""  }

Case : case Exp of Conds       {  % putStrLn ""  }

-- Condiciones
Conds : Conds Cond             {  % putStrLn ""  }
      | {- empty -}            { % putStrLn ""   }

Cond : Exp In                  {  % putStrLn ""  }

-- Iteradores
Iterator : Indet               {% putStrLn ""  }
         | Det                 {  % putStrLn ""  }

Indet : while Exp In           { % putStrLn ""   }

Det : for Type Id from Exp to Exp                     {  % putStrLn ""  }
    | for Type Id from Exp to Exp if Exp In           {  % putStrLn ""  }
    | for Type Id from Exp to Exp with Exp if Exp In  {  % putStrLn ""  }
    | for Type Id from Exp to Exp with Exp In         {  % putStrLn ""  }

{
parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError _ = error $ "ERROR"
}
