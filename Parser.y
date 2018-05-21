{
module Parser where
import Lexer
import Control.Monad.Trans.Except
import SyntaxTree
import SymTable
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
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
S : Mod Imports Body            { Init $1 $2 $3 }

Mod : module type              { Module $2 }
    | {- empty -}              { Main }

-- Importaciones
Imports : Imports import type  { (Import $3) : $1 }
        | {- empty -}          {   [] }

-- Instrucciones
Body : Body In                  { $2 : $1 }
     | Body Algebraic            { $1 }
     | Body Declaration          { $1 }
     | Body Function             { $2 : $1 }
     | {- empty -}               { [] }

In : SingleI ';'               {   $1 }
   | Selector                  {   $1 }
   | Iterator                  {   $1 }
   | Block                     { Block $1 }

SingleI : IDeclaration            { Assign $1 }
        | Assign                  { Assign $1 }
        | return Exp              { Ret $2 }
        | Print                   { $1 }
        | PrintLn                 { $1 }
        | continue                { Continue }
        | break                   { Break }

Print : print '(' Exp ')'      { Print $3}

PrintLn : printLn '(' Exp ')'  { PrintLn $3 }

Read : read '(' ')'            { Read }

-- Bloques
Block : dream Body wake         { $2 }

Algebraic : data type dream Sums wake  { % lift $ pushS (StackEntry 1 False "")  }

Sums : Sums Sum                {     }
     | Sum                     {     }

Sum : type '(' Prods ')' ';'   {     }
    | type '(' ')' ';'         {     }

Declaration : Type Ids ';'     {     }

IDeclaration : Type DAssign { $2 }


DAssign : id ',' DAssign ',' RV {   (( \(l,r) -> ((Variable $1):l,r++[$5]) ) $3)  }
        | id '=' RV             {   ([Variable $1],[$3]) }

Prods : Prods ',' Prod         {   }
      | Prod                   {   }

Prod : Type id                 {  }

-- Identificadores
Ids : id ',' Ids               { $1 : $3 }
    | id                       { [$1] }

Id : id                        { Variable $1  }
   | Id '[' Exp ']'            { Index $1 $3 }
   | Id '.' MCall              { MemberCall $1 $3 }

MCall : MCall '.' id           {   ($3 : $1) }
      | id                     {   [$1] }

Types : Types ',' Type         {   ($3 : $1) }
      | Type                   {   [$1] }

Type : type                    {   (Name $1) }
     | '[' Type ']'            {   (List $2) }
     | '{' Type ':' num '}'    {   (Array $2 $4) }
     | '[' Type ':' Type ']'   {   (Dict ($2,$4)) }
     | '(' Types ')'           {   (Tuple $2) }

Assign : Id ',' Assign ',' RV  {   (( \(l,r) -> ($1:l,r++[$5]) ) $3) }
       | Id '=' RV             {   ([$1],[$3]) }

RV : Exp    {   (ValueExp $1) }
   | Cons   {   (ValueCons $1) }

Cons : type '(' ')'      {   (CCall $1 []) }
     | type '(' Exps ')' {   (CCall $1 $3) }

-- Expresiones
Exp : Exp '+' Exp              {   (ESum (SumOp $1 $3)) }
    | Exp '-' Exp              {   (EDif (Dif $1 $3)) }
    | Exp '*' Exp              {   (EMul (Mul $1 $3)) }
    | Exp '/' Exp              {   (EDiv (Div $1 $3)) }
    | Exp '%' Exp              {   (EMod (Mod $1 $3)) }
    | Exp '**' Exp             {   (EPot (Pot $1 $3)) }
    | Exp '//' Exp             {   (EDivE (DivE $1 $3)) }
    | Exp '<<' Exp             {   (ELShift (LShift $1 $3)) }
    | Exp '>>' Exp             {   (ERShift (RShift $1 $3)) }
    | Exp '|' Exp              {   (EBitOr (BitOr $1 $3)) }
    | Exp '^' Exp              {   (EBitXor (BitXor $1 $3)) }
    | Exp '&' Exp              {   (EBitAnd (BitAnd $1 $3)) }
    | Exp '||' Exp             {   (EOr (Or $1 $3)) }
    | Exp '&&' Exp             {   (EAnd (And $1 $3)) }
    | Exp '>' Exp              {   (EGreat (Great $1 $3)) }
    | Exp '<' Exp              {   (ELess (Less $1 $3))}
    | Exp '>=' Exp             {   (EGEq (GEq $1 $3)) }
    | Exp '<=' Exp             {   (ELEq (LEq $1 $3)) }
    | Exp '==' Exp             {   (EEqual (Equal $1 $3)) }
    | Exp '/=' Exp             {   (ENEq (NEq $1 $3)) }
    | '-' Exp %prec NEG        {   (ENeg $2) }
    | '!' Exp                  {   (ENot $2) }
    | '~' Exp                  {   (EBitNot $2) }
    | '(' Exp ')'              {   $2 }
    | Id                       {   EIdent $1 }
    | num                      {   (EToken $1) }
    | true                     {   (EToken $1) }
    | false                    {   (EToken $1) }
    | str                      {   (EToken $1) }
    | char                     {   (EToken $1) }
    | List                     {   $1 }
    | Arr                      {   $1 }
    | Dict                     {   $1 }
    | Tup                      {   $1 }
    | FunCall                  {   EFCall $1}
    | Read                     {   $1 }
    | Id '?'                   { ERef $1 }

Exps : Exps ',' Exp            {   ($3 : $1) }
     | Exp                     {   [$1] }

List : '[' Exps ']'            {   (EList $2) }
     | '[' ']'                 {   (EList []) }

Arr : '{' Exps '}'             {   (EArr $2) }
    | '{' '}'                  {   (EArr []) }

Dict : '[' KV ']'              {   (EDict $2) }

KV : KV ',' Exp ':' Exp        {   (($3,$5) : $1)}
   | Exp ':' Exp               {   [($1,$3)] }

Tup : '(' Exp ',' Exps ')'     {   (ETup ($2 : $4)) }

-- Funciones
FunCall : id '(' Exps ')'      {    (FCall $1 $3)  }
        | id '(' ')'           {    (FCall $1 []) }

Function : func '(' Type Ret Exp ')' Block         {   Function }
         | func '(' Type NoRet Exp ')' Block       {   Function }
         | func '(' '->' Type ')' id '(' ')' Block {   Function }
         | func '(' ')' id '(' ')' Block           {   Function }

Ret : ',' Type Ret Exp ','     {    }
    | '->' Type ')' id '('     {    }

NoRet : '->' ',' Type NoRet Exp ',' {    }
      | ')' id '('                  {    }

-- Selectores
Selector : If                  { $1 }
         | Case                { Det  }

If : if Exp then In            {   (IfThen $2 $4) }
   | if Exp then In else In    {   (IfElse $2 $4 $6) }

Case : case Exp of Conds ';'   {    }

-- Condiciones
Conds : Conds Cond             {    }
      | {- empty -}            {    }

Cond : Exp In                  {     }

-- Iteradores
Iterator : Indet               { $1 }
         | Det                 { Det }

Indet : while Exp In           { (While $2 $3) }

Det : for Type Id from Exp to Exp                     {      }
    | for Type Id from Exp to Exp if Exp In           {      }
    | for Type Id from Exp to Exp with Exp if Exp In  {      }
    | for Type Id from Exp to Exp with Exp In         {      }
    | for Type Id in Exp if Exp In                    {      }

{
parseError [] = throwE $ "Unexpected ending."
parseError (t:_) = throwE $ "Unexpected token: " ++ show t
}
