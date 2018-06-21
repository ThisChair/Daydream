{
module Parser where
import Lexer
import Control.Monad.Trans.Except
import SyntaxTree
import SymTable
import Control.Monad.Trans.State.Strict
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
S : Mod Imports Body            { % return $ Init $1 (reverse $2) (reverse $3)  }

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

In : SingleI ';'               { % return $1 }
   | Selector                  { % return $1 }
   | Iterator                  { % return $1 }
   | Block                     { % return $ Block $1 }

SingleI : IDeclaration            { % return $ Assign $1 }
        | Assign                  { % return $ Assign $1 }
        | return Exp              { % return $ Ret $2 }
        | Print                   { % return $ $1 }
        | PrintLn                 { % return $ $1 }
        | continue                { % return $ Continue }
        | break                   { % return $ Break }

Print : print '(' Exp ')'      { % return $ Print $3 }

PrintLn : printLn '(' Exp ')'  { % return $ PrintLn $3 }

Read : read '(' ')'            { % return $ Read }

-- Bloques
Block : BlockScope dream Body wake         { % lift $ popS >> (return $3) }

BlockScope : {- empty -}                    { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SBlock Nothing []);
                                                          addNumber } }

Algebraic : AlgScope data type dream Sums wake  { % pervasiveCheck (tokenVal $3) >> 
                                                              (lift $ do{i <- getActualScope;
                                                              popS;
                                                              j <- getActualScope;
                                                              insertSymS (tokenVal $3) (SymScope j (TypeType,0) [("",i)] (tokenPos $3)); 
                                                             })
                                                }

AlgScope : {- empty -}         { % lift $ do {i <- getScopeNumber; 
                                              pushS (StackEntry i SType Nothing []);
                                              addNumber } }

Sums : Sums Sum                { }
     | Sum                     { }

Sum : ConsScope type '(' Prods ')' ';'   { % lift $do{i <- getActualScope; popS;
                                                      j <- getActualScope;
                                                      insertSymS (tokenVal $2) (SymScope j (TypeType,0) [("",i)] (tokenPos $3)); 
                                                     } 
                                         }
    | ConsScope type '(' ')' ';'         { % lift  popS }

ConsScope : {- empty -}      { % lift $ do {i <- getScopeNumber; 
                                            pushS (StackEntry i SCons Nothing []);
                                            addNumber } }

Declaration : Type Ids ';'     { % do { (mapM_ pervasiveCheck (map tokenVal $2));
                                        i <- lift getActualScope;
                                        t <- getType $1;
                                        lift $ mapM_ (\x -> insertSymS (tokenVal x) (SymScope i t [] (tokenPos x))) (reverse $2); 
                                      } 
                               }

IDeclaration : Type DAssign { % do { mapM_ pervasiveCheck (map idString (fst $2));
                                     t <- getType $1;
                                     lift $ mapM_ (\(Variable (x,y,z)) -> insertSymS x (SymScope y t [] z)) (fst $2);
                                     return (fst $2, reverse (snd $2)) } }


DAssign : id ',' DAssign ',' RV { % do{ i <- lift getActualScope;
                                        return (( \(l,r) -> ((Variable (tokenVal $1,i,tokenPos $1)):l,$5:r) ) $3)
                                      } 
                                }
        | id '=' RV             { % do{ i <- lift getActualScope; return ([Variable (tokenVal $1,i,tokenPos $1)],[$3]) } }

Prods : Prods ',' Prod         { }
      | Prod                   { }

Prod : Type id                 { %  do{i <- lift getActualScope;
                                       t <- getType $1;
                                       lift $ insertSymS (tokenVal $2) (SymScope i t [] (tokenPos $2)); } }

-- Identificadores
Ids : id ',' Ids               { % return $ $1 : $3 }
    | id                       { % return $ [$1] }

Id : id                        { % do{ s <- searchTable (tokenVal $1);
                                       i <- lift getActualScope;
                                       return $ (Variable s); }  }
   | Id '[' Exp ']'            { % return $ Index $1 $3 }
   | Id '.' MCall              { % return $ MemberCall $1 $3 }

MCall : MCall '.' id           { % return ($3 : $1) }
      | id                     { % return [$1] }

Types : Types ',' Type         { % return ($3 : $1) }
      | Type                   { % return [$1] }

Type : type                    { % return (Name (tokenVal $1)) }
     | '[' Type ']'            { % return (List $2) }
     | '{' Type ':' num '}'    { % return (Array $2 $4) }
     | '[' Type ':' Type ']'   { % return (Dict ($2,$4)) }
     | '(' Types ')'           { % return (Tuple $ reverse $2) }

Assign : Id ',' Assign ',' RV  { % return (( \(l,r) -> ($1:l,$5:r) ) $3) }
       | Id '=' RV             { % return ([$1],[$3]) }

RV : Exp    { % return $   (ValueExp $1) }
   | Cons   { % return $   (ValueCons $1) }

Cons : type '(' ')'      { % return $   (CCall $1 []) }
     | type '(' Exps ')' { % return $   (CCall $1 (reverse $3)) }

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

List : '[' Exps ']'            { % return $   (EList $ reverse $2) }
     | '[' ']'                 { % return $   (EList []) }

Arr : '{' Exps '}'             { % return $   (EArr $ reverse $2) }
    | '{' '}'                  { % return $   (EArr []) }

Dict : '[' KV ']'              { % return $   (EDict $ reverse $2) }

KV : KV ',' Exp ':' Exp        { % return $   (($3,$5) : $1)}
   | Exp ':' Exp               { % return $   [($1,$3)] }

Tup : '(' Exp ',' Exps ')'     { % return $   (ETup $ reverse ($2 : $4)) }

-- Funciones
FunCall : id '(' Exps ')'      { % return $    (FCall $1 $3)  }
        | id '(' ')'           { % return $    (FCall $1 []) }

Function : FuncScope func '('ParRet ')' Block { % lift $ do { popS;
                                                              i <- getActualScope; 
                                                              (\((_,_),(_,n)) -> 
                                                                  insertSymS (tokenVal n) (SymScope i (TypeFunc [] [],0) [] (tokenPos n))) $4; 
                                                            }
                                              }
         | FuncScope func '(' ParNoRet ')' Block {  % lift $ do { popS;
                                                                i <- getActualScope; 
                                                                (\((_,_),(_,n)) -> 
                                                                    insertSymS (tokenVal n) (SymScope i (TypeFunc [] [],0) [] (tokenPos n))) $4; 
                                                                } 
                                                 }
         | FuncScope func '(' '->' Types ')' id '(' ')' Block {  }
         | FuncScope func '(' ')' id '(' ')' Block           {  }

FuncScope : {- empty -}                    { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SFunc Nothing []);
                                                          addNumber } }

ParRet : Type Ret id          { % do { i <- lift getActualScope;
                                       l <- return ((\((x,y),(z,n)) -> (($1 : x,$3 : y ),(z,n))) $2); 
                                       t <- mapM getType (fst $ fst l);
                                       mapM_ pervasiveCheck (map tokenVal (snd $ fst l));
                                       mapM_ searchTable (map typeString (fst $ snd l));
                                       lift $ mapM_ (\(x,y) -> 
                                           insertSymS (tokenVal y) (SymScope i x [] (tokenPos y))) (zip t (snd $ fst l));
                                       return l; } }

ParNoRet : Type NoRet id    { % do { i <- lift getActualScope;
                                     l <- return ((\((x,y),(_,n)) -> 
                                         (($1 : x,$3 : y ),([],n))) $2); 
                                     t <- mapM getType (fst $ fst l);
                                     mapM_ pervasiveCheck (map tokenVal (snd $ fst l));
                                     lift $ mapM_ (\(x,y) -> 
                                         insertSymS (tokenVal y) (SymScope i x [] (tokenPos y))) (zip t (snd $ fst l));
                                     return l; } }

Ret : ',' Type Ret id ','     { % return $ (\((x,y),(z,n)) -> (($2 : x,$4 : y ),(z,n))) $3  }
    | '->' Types ')' id '('     { % return (([],[]),($2,$4)) }

NoRet : ',' Type NoRet id ',' { % return $ (\((x,y),(_,n)) -> (($2 : x,$4 : y),([],n))) $3 }
      | ')' id '('                  { % return (([],[]),([],$2)) }

-- Selectores
Selector : If                  { % return $ $1 }
         | Case                { % return $ Block [] }

If : if Exp then In            { % return $   (IfThen $2 $4) }
   | if Exp then In else In    { % return $   (IfElse $2 $4 $6) }

Case : case Exp of Conds ';'   { }

-- Condiciones
Conds : Conds Cond             { }
      | {- empty -}            { }

Cond : Exp In                  { }

-- Iteradores
Iterator : Indet               { % return $ $1 }
         | Det                 { % return $ Det $1 }

Indet : while Exp In           { % return $ (While $2 $3) }

Det : ForScope for ForDec from Exp to Exp In                  { % lift $ popS >> (return $ FromTo $5 $7 $8)}
    | ForScope for ForDec from Exp to Exp if Exp In           { % lift $ popS >> (return $ FromToIf $5 $7 $9 $10)}
    | ForScope for ForDec from Exp to Exp with Exp if Exp In  { % lift $ popS >> (return $ FromToWithIf $5 $7 $9 $11 $12)}
    | ForScope for ForDec from Exp to Exp with Exp In         { % lift $ popS >> (return $ FromToWith $5 $7 $9 $10)}
    | ForScope for ForDec in Exp if Exp In                    { % lift $ popS >> (return $ InIf $5 $7 $8)}

ForDec : Type id                         {% do {i <- lift getActualScope;
                                                t <- getType $1;
                                                lift $ insertSymS (tokenVal $2) (SymScope i t [] (tokenPos $2)); } }

ForScope : {- empty -}                     { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SFor Nothing []);
                                                          addNumber } }

{
parseError [] = throwE $ "Unexpected ending."
parseError (t:_) = throwE $ "Unexpected token: " ++ show t
}
