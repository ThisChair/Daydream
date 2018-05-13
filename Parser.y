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
    '%'        { TPerfect _ }
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
S : Imports Ins                { Init $1 $2 }

-- Importaciones
Imports : Imports import Type  { Import $3 : $1 }
        | {- empty -}          { [] }

-- Instrucciones
Ins : Ins In                   { $2 : $1 }
    | {- empty -}              { [] }

In : Body ';'                  { $1 }
   | Block                     { $1 }
   | Algebraic                 { TypeDec $1 }

Body : Declaration             { $1 }
     | Assign                  { $1 }
     | Selector                { $1 }
     | Iterator                { $1 }
     | Function                { $1 }
     | return Exp              { IReturn $2 }
     | {- empty -}             { IEmpty  }

Block : dream Ins wake         { Block $2  }

Algebraic : data type dream Sums wake  { DataType $2 $4 }

Sums : Sums Sum                { $2 : $1 }
     | Sum                     { [$1] }

Sum : type '(' Prods ')' ';'   { Constructor $1 $3 }

Declaration : Type Ids         { Dec $1 $2 }
            | Type DAssign     { DecA $1 $2 }

DAssign : id ',' DAssign ',' RV { ( \(l,r) -> ($1:l,r++[$5]) ) $3  }
        | id '=' RV             { ([$1],[$3]) }

Prods : Prods ',' Prod         { $2 : $1 }
      | Prod                   { [$1] }

Prod : Type id                 { Member $1 $2 }

-- Identificadores
Ids : Ids ',' id               { [$2] : $1 }
    | id                       { [$1] }

Id : id                        { Variable $1  }
   | Id '[' Exp ']'            { Index $1  }
   | Id '.' MCall              { MemberCall $1 $3 }

MCall : MCall '.' id           { $3 : $1 }
      | id                     { [$1] }

Types : Types ';' Type         { $3 : $1  }
      | Type                   { [$1] }

Type : type                    { Name $1 }
     | '[' Type ']'            { List $2  }
     | '{' Type ':' num '}'    { Array $2 $4 }
     | '[' Type ':' Type ']'   { Dict ($2,$4)  }
     | '(' Types ')'           { Tuple $2 }

Assign : Id ',' Assign ',' RV  { ( \(l,r) -> ($1:l,r++[$5]) ) $3 }
       | Id '=' RV             { ([$1],[$3]) }

RV : Exp    { ValueExp $1 }
   | Cons   { ValueCons $1 }

Cons : type '(' ')'      { CCall $1 [] }
     | type '(' Exps ')' { CCall $1 $3 }

-- Expresiones
Exp : Exp '+' Exp              { ESum (SumOp $1 $3) }
    | Exp '-' Exp              { EDif (Dif $1 $3) }
    | Exp '*' Exp              { EMul (Mul $1 $3) }
    | Exp '/' Exp              { EDiv (Div $1 $3) }
    | Exp '%' Exp              { EMod (Mod $1 $3) }
    | Exp '**' Exp             { EPot (Pot $1 $3) }
    | Exp '//' Exp             { EDivE (DivE $1 $3) }
    | Exp '<<' Exp             { ELShift (LShift $1 $3) }
    | Exp '>>' Exp             { ERShift (RShift $1 $3) }
    | Exp '|' Exp              { EBitOr (BitOr $1 $3) }
    | Exp '&' Exp              { EBitAnd (BitAnd $1 $3) }
    | Exp '^' Exp              { EBitXor (BitXor $1 $3) }
    | Exp '||' Exp             { EOr (Or $1 $3) }
    | Exp '&&' Exp             { EAnd (And $1 $3) }
    | Exp '>' Exp              { EGreat (Great $1 $3) }
    | Exp '<' Exp              { ELess (ELess $1 $3) }
    | Exp '>=' Exp             { EGEq (GEq $1 $3) }
    | Exp '<=' Exp             { ELEq (LEq $1 $3) }
    | Exp '==' Exp             { EEq (Eq $1 $3) }
    | Exp '/=' Exp             { ENotEq (NotEq $1 $3) }
    | '-' Exp %prec NEG        { ENeg $2 }
    | '!' Exp                  { ENot $2 }
    | '~' Exp                  { EBitNot $2 }
    | Exp '?'                  { ERef $1 }
    | '(' Exp ')'              { $2 }
    | Id                       { EIdentifier $1 }
    | num                      { EToken $1 }
    | true                     { EToken $1 }
    | false                    { EToken $1 }
    | str                      { EToken $1 }
    | char                     { EToken $1 }
    | List                     { $1 }
    | Arr                      { $1 }
    | Dict                     { $1 }
    | Tup                      { $1 }
    | FunCall                  { EFCall $1 }

Exps : Exps ',' Exp            { $3 : $1 }
     | Exp                     { [$1] }

List : '[' Exps ']'            { EList $2 }
     | '[' ']'                 { EList [] }

Arr : '{' Exps '}'             { EArr $2 }
    | '{' '}'                  { EArr [] }

Dict : '[' KV ']'             { EDict $2 }

KV : KV ',' Exp ':' Exp        { ($3,$5) : $1 }
   | Exp ':' Exp               { [($1,$3)] }

Tup : '(' Exps ',' Exp ')'     { ETup ($3 : $1) }

-- Funciones
FunCall : id '(' Exps ')'      { FCall $1 $2  }
        | id '(' ')'           { FCall $1 []  }

Function : func '(' Type Ret Exp ')' Block         { }
         | func '(' Type NoRet Exp ')' Block       { }
         | func '(' '->' Type ')' id '(' ')' Block { }
         | func '(' ')' id '(' ')' Block           { }

Ret : ',' Type Ret Exp ','     {  }
    | '->' Type ')' id '('     {  }

NoRet : '->' ',' Type NoRet Exp ',' {  }
      | ')' id '('                  {  }

-- Selectores
Selector : If                  { $1 }
         | Case                {  }

If : if Exp then In            { IfThen $2 $4 }
   | if Exp then In else In    { IfElse $2 $4 $6  }

Case : case Exp of Conds       {  }

-- Condiciones
Conds : Conds Cond             {  }
      | {- empty -}            {  }

Cond : Exp In                  {  }

-- Iteradores
Iterator : Indet               {  }
         | Det                 {  }

Indet : while Cond             {  }

Det : for Type Id from Exp to Exp                     {  }
    | for Type Id from Exp to Exp if Exp In           {  }
    | for Type Id from Exp to Exp with Exp if Exp In  {  }
    | for Type Id from Exp to Exp with Exp In         {  }

{
parseError :: [Token] -> a
parseError _ = error $ "ERROR"
}
