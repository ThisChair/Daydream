{
module Parser where
import Lexer
import Control.Monad.Trans.Except
import SyntaxTree
import SymTable
import Control.Monad.Trans.State.Lazy
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
S : Mod Imports Body            { % return $ Init $1 $2 $3 }

Mod : module type              { % return $ Module $2 }
    | {- empty -}              { % return $ Main }

-- Importaciones
Imports : Imports import type  { % return $ (Import $3) : $1 }
        | {- empty -}          { % return $   [] }

-- Instrucciones
Body : Body In                  { % return $ $2 : $1 }
     | Body Algebraic            { % return $1 }
     | Body Declaration          { % return $1 }
     | Body Function             { % return $1 }
     | {- empty -}               { % return [] }

In : SingleI ';'               { % return $   $1 }
   | Selector                  { % return $   $1 }
   | Iterator                  { % return $   $1 }
   | Block                     { % return $ Block $1 }

SingleI : IDeclaration            { % return $ Assign $1 }
        | Assign                  { % return $ Assign $1 }
        | return Exp              { % return $ Ret $2 }
        | Print                   { % return $ $1 }
        | PrintLn                 { % return $ $1 }
        | continue                { % return $ Continue }
        | break                   { % return $ Break }

Print : print '(' Exp ')'      { % return $ Print $3}

PrintLn : printLn '(' Exp ')'  { % return $ PrintLn $3 }

Read : read '(' ')'            { % return $ Read }

-- Bloques
Block : BlockScope dream Body wake         { % lift $ popS >> (return $3) }

BlockScope : {- empty -}                    { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SBlock Nothing []);
                                                          addNumber } }

Algebraic : data type dream Sums wake  {  }

Sums : Sums Sum                { }
     | Sum                     { }

Sum : type '(' Prods ')' ';'   { }
    | type '(' ')' ';'         { }

Declaration : Type Ids ';'     { }

IDeclaration : Type DAssign { % return $ $2 }


DAssign : id ',' DAssign ',' RV { % return $   (( \(l,r) -> ((Variable $1):l,r++[$5]) ) $3)  }
        | id '=' RV             { % return $   ([Variable $1],[$3]) }

Prods : Prods ',' Prod         { }
      | Prod                   { }

Prod : Type id                 { }

-- Identificadores
Ids : id ',' Ids               { % return $ $1 : $3 }
    | id                       { % return $ [$1] }

Id : id                        { % return $ Variable $1  }
   | Id '[' Exp ']'            { % return $ Index $1 $3 }
   | Id '.' MCall              { % return $ MemberCall $1 $3 }

MCall : MCall '.' id           { % return $   ($3 : $1) }
      | id                     { % return $   [$1] }

Types : Types ',' Type         { % return $   ($3 : $1) }
      | Type                   { % return $   [$1] }

Type : type                    { % return $   (Name $1) }
     | '[' Type ']'            { % return $   (List $2) }
     | '{' Type ':' num '}'    { % return $   (Array $2 $4) }
     | '[' Type ':' Type ']'   { % return $   (Dict ($2,$4)) }
     | '(' Types ')'           { % return $   (Tuple $2) }

Assign : Id ',' Assign ',' RV  { % return $   (( \(l,r) -> ($1:l,r++[$5]) ) $3) }
       | Id '=' RV             { % return $   ([$1],[$3]) }

RV : Exp    { % return $   (ValueExp $1) }
   | Cons   { % return $   (ValueCons $1) }

Cons : type '(' ')'      { % return $   (CCall $1 []) }
     | type '(' Exps ')' { % return $   (CCall $1 $3) }

-- Expresiones
Exp : Exp '+' Exp              { % return $   (ESum (SumOp $1 $3)) }
    | Exp '-' Exp              { % return $   (EDif (Dif $1 $3)) }
    | Exp '*' Exp              { % return $   (EMul (Mul $1 $3)) }
    | Exp '/' Exp              { % return $   (EDiv (Div $1 $3)) }
    | Exp '%' Exp              { % return $   (EMod (Mod $1 $3)) }
    | Exp '**' Exp             { % return $   (EPot (Pot $1 $3)) }
    | Exp '//' Exp             { % return $   (EDivE (DivE $1 $3)) }
    | Exp '<<' Exp             { % return $   (ELShift (LShift $1 $3)) }
    | Exp '>>' Exp             { % return $   (ERShift (RShift $1 $3)) }
    | Exp '|' Exp              { % return $   (EBitOr (BitOr $1 $3)) }
    | Exp '^' Exp              { % return $   (EBitXor (BitXor $1 $3)) }
    | Exp '&' Exp              { % return $   (EBitAnd (BitAnd $1 $3)) }
    | Exp '||' Exp             { % return $   (EOr (Or $1 $3)) }
    | Exp '&&' Exp             { % return $   (EAnd (And $1 $3)) }
    | Exp '>' Exp              { % return $   (EGreat (Great $1 $3)) }
    | Exp '<' Exp              { % return $   (ELess (Less $1 $3))}
    | Exp '>=' Exp             { % return $   (EGEq (GEq $1 $3)) }
    | Exp '<=' Exp             { % return $   (ELEq (LEq $1 $3)) }
    | Exp '==' Exp             { % return $   (EEqual (Equal $1 $3)) }
    | Exp '/=' Exp             { % return $   (ENEq (NEq $1 $3)) }
    | '-' Exp %prec NEG        { % return $   (ENeg $2) }
    | '!' Exp                  { % return $   (ENot $2) }
    | '~' Exp                  { % return $   (EBitNot $2) }
    | '(' Exp ')'              { % return $   $2 }
    | Id                       { % return $   EIdent $1 }
    | num                      { % return $   (EToken $1) }
    | true                     { % return $   (EToken $1) }
    | false                    { % return $   (EToken $1) }
    | str                      { % return $   (EToken $1) }
    | char                     { % return $   (EToken $1) }
    | List                     { % return $   $1 }
    | Arr                      { % return $   $1 }
    | Dict                     { % return $   $1 }
    | Tup                      { % return $   $1 }
    | FunCall                  { % return $   EFCall $1}
    | Read                     { % return $   $1 }
    | Id '?'                   { % return $ ERef $1 }

Exps : Exps ',' Exp            { % return $   ($3 : $1) }
     | Exp                     { % return $   [$1] }

List : '[' Exps ']'            { % return $   (EList $2) }
     | '[' ']'                 { % return $   (EList []) }

Arr : '{' Exps '}'             { % return $   (EArr $2) }
    | '{' '}'                  { % return $   (EArr []) }

Dict : '[' KV ']'              { % return $   (EDict $2) }

KV : KV ',' Exp ':' Exp        { % return $   (($3,$5) : $1)}
   | Exp ':' Exp               { % return $   [($1,$3)] }

Tup : '(' Exp ',' Exps ')'     { % return $   (ETup ($2 : $4)) }

-- Funciones
FunCall : id '(' Exps ')'      { % return $    (FCall $1 $3)  }
        | id '(' ')'           { % return $    (FCall $1 []) }

Function : func '(' Type Ret Exp ')' Block         {  }
         | func '(' Type NoRet Exp ')' Block       {  }
         | func '(' '->' Type ')' id '(' ')' Block {  }
         | func '(' ')' id '(' ')' Block           {  }

Ret : ',' Type Ret Exp ','     { }
    | '->' Type ')' id '('     { }

NoRet : '->' ',' Type NoRet Exp ',' { }
      | ')' id '('                  { }

-- Selectores
Selector : If                  { % return $ $1 }
         | Case                { % return $ Det  }

If : if Exp then In            { % return $   (IfThen $2 $4) }
   | if Exp then In else In    { % return $   (IfElse $2 $4 $6) }

Case : case Exp of Conds ';'   { }

-- Condiciones
Conds : Conds Cond             { }
      | {- empty -}            { }

Cond : Exp In                  { }

-- Iteradores
Iterator : Indet               { % return $ $1 }
         | Det                 { % return $ Det }

Indet : while Exp In           { % return $ (While $2 $3) }

Det : ForScope for Type id from Exp to Exp                     { }
    | ForScope for Type id from Exp to Exp if Exp In           { }
    | ForScope for Type id from Exp to Exp with Exp if Exp In  { }
    | ForScope for Type id from Exp to Exp with Exp In         { }
    | ForScope for Type id in Exp if Exp In                    { }

ForScope : {- empty -}                     { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SFor Nothing []);
                                                          addNumber } }

{
parseError [] = throwE $ "Unexpected ending."
parseError (t:_) = throwE $ "Unexpected token: " ++ show t
}
