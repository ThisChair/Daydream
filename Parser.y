{
module Parser where
import Lexer
}

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
S : Imports Ins                { putStrLn "Inicio" }

-- Importaciones
Imports : Imports import Type  { }
        | {- empty -}          { }

-- Instrucciones
Ins : Ins In                   {  }
    | {- empty -}              {  }

In : Body ';'                  {  }
   | Block                     {  }
   | Algebraic                 {  }

Body : Declaration             {  }
     | Assign                  {  }
     | Selector                {  }
     | Iterator                {  }
     | Function                { }
     | return Exp              { }
     | {- empty -}             {  }

Block : dream Ins wake         {   }

Algebraic : data type dream Sums wake  {  }

Sums : Sums Sum                {  }
     | Sum                     {  }

Sum : type '(' Prods ')' ';'   {  }

Declaration : Type Ids         {  }
            | Type DAssign     {  }

DAssign : id ',' DAssign ',' RV {  }
        | id '=' RV             { }

Prods : Prods ',' Prod         {  }
      | Prod                   {  }

Prod : Type id                 {  }

-- Identificadores
Ids : Ids ',' id               {  }
    | id                       { }

Id : id                        {  }
   | Id '[' Exp ']'            {  }
   | Id '.' MCall              {  }

MCall : MCall '.' id           { }
      | id                     { } 

Types : Types ';' Type         { }
      | Type                   { } 

Type : type                    { } 
     | '[' Type ']'            { }
     | '{' Type ':' num '}'    {  }
     | '[' Type ':' Type ']'   {   }
     | '(' Types ')'           { }

Assign : Id ',' Assign ',' RV  {  }
       | Id '=' RV             {  }

RV : Exp    { }
   | Cons   {  }

Cons : type '(' ')'      {  }
     | type '(' Exps ')' {  }

-- Expresiones
Exp : Exp '+' Exp              {  }
    | Exp '-' Exp              {  } 
    | Exp '*' Exp              {  } 
    | Exp '/' Exp              { }
    | Exp '%' Exp              { }
    | Exp '**' Exp             { }
    | Exp '//' Exp             { }
    | Exp '<<' Exp             {  }
    | Exp '>>' Exp             {  }
    | Exp '|' Exp              {  }
    | Exp '&' Exp              {  }
    | Exp '^' Exp              {  }
    | Exp '||' Exp             { }
    | Exp '&&' Exp             { }
    | Exp '>' Exp              {  }
    | Exp '<' Exp              {  }
    | Exp '>=' Exp             { }
    | Exp '<=' Exp             { }
    | Exp '==' Exp             { }
    | Exp '/=' Exp             {  }
    | '-' Exp %prec NEG        { }
    | '!' Exp                  { }
    | '~' Exp                  { }
    | Exp '?'                  { }
    | '(' Exp ')'              { }
    | Id                       { }
    | num                      { }
    | true                     { }
    | false                    { }
    | str                      { }
    | char                     { }
    | List                     { }
    | Arr                      { }
    | Dict                     { }
    | Tup                      { }
    | FunCall                  { }

Exps : Exps ',' Exp            {  }
     | Exp                     {  }

List : '[' Exps ']'            {  }
     | '[' ']'                 {  }

Arr : '{' Exps '}'             {  }
    | '{' '}'                  { }

Dict : '[' KV ']'             {  }

KV : KV ',' Exp ':' Exp        {  }
   | Exp ':' Exp               {  }

Tup : '(' Exps ',' Exp ')'     {  }

-- Funciones
FunCall : id '(' Exps ')'      {   }
        | id '(' ')'           {   }

Function : func '(' Type Ret Exp ')' Block         {  }
         | func '(' Type NoRet Exp ')' Block       {  }
         | func '(' '->' Type ')' id '(' ')' Block {  }
         | func '(' ')' id '(' ')' Block           {  }

Ret : ',' Type Ret Exp ','     {  }
    | '->' Type ')' id '('     {  }

NoRet : '->' ',' Type NoRet Exp ',' {  }
      | ')' id '('                  {  }

-- Selectores
Selector : If                  {  }
         | Case                {  }

If : if Exp then In            { }
   | if Exp then In else In    {   }

Case : case Exp of Conds       {   }

-- Condiciones
Conds : Conds Cond             {   }
      | {- empty -}            {   }

Cond : Exp In                  {   }

-- Iteradores
Iterator : Indet               { }
         | Det                 {   }

Indet : while Exp In           {   }

Det : for Type Id from Exp to Exp                     {   }
    | for Type Id from Exp to Exp if Exp In           {   }
    | for Type Id from Exp to Exp with Exp if Exp In  {   }
    | for Type Id from Exp to Exp with Exp In         {   }

{
parseError :: [Token] -> a
parseError _ = error $ "ERROR"
}
