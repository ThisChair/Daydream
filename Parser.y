{
module Parser where
import Lexer
import Control.Monad.Trans.Except
import SyntaxTree
import SymTable
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad(zipWithM_,zipWithM)
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
    '|:'       { TOpenT _ }
    ':|'       { TCloseT _ }
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
S : Mod Imports Body           { % return $ Init TypeError $1 (reverse $2) (reverse $3) }

Mod : module type              { % return $ Module TypeError $2 }
    | {- empty -}              { % return $ Main TypeError }

-- Importaciones
Imports : Imports import type  { % return $ (Import TypeError $3) : $1 }
        | {- empty -}          { % return $ [] }

-- Instrucciones
Body : Body In                 { % return $ $2 : $1 }
     | Body Algebraic          { % return $1 }
     | Body Declaration        { % return $1 }
     | Body Function           { % return $1 }
     | {- empty -}             { % return [] }

In : SingleI ';'               { % return $1 }
   | Selector                  { % return $1 }
   | Iterator                  { % return $1 }
   | Block                     { % return $ Block TypeError $1 }

SingleI : IDeclaration         { % return $ Assign (assignType $1) $1 }
        | Assign               { % checkAssign $1 >>= (\t -> return $ Assign t $1) }
        | return Exp           { % checkInFun (tokenPos $1) >> (return $ Ret TypeVoid $2) }
        | Print                { % return $ $1 }
        | PrintLn              { % return $ $1 }
        | continue             { % return $ Continue TypeVoid }
        | break                { % return $ Break TypeVoid }
        | FunCall              { % checkIFunCall $1 >>= (\x -> return $ IFCall x $1) }

Print : print '(' Exp ')'      { % return $ Print TypeError $3 }

PrintLn : printLn '(' Exp ')'  { % return $ PrintLn TypeError $3 }

Read : read '(' ')'            { % return $ Read TypeError }

-- Bloques
Block : BlockScope dream Body wake         { % lift $ popS >> (return $3) }

BlockScope : {- empty -}                    { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SBlock Nothing []);
                                                          addNumber } }

Algebraic : AlgScope data type '|:' Sums ':|'  { % do  {
                                                            pervasiveCheck (tokenVal $3) (tokenPos $3);
                                                            i <- lift getActualScope;
                                                            lift popS;
                                                            j <- lift getActualScope;
                                                            redeclaredCheck (tokenVal $3) j (tokenPos $3);
                                                            lift $ insertSymS (tokenVal $3) (SymScope j (TypeType,0) [] (tokenPos $3)); 
                                                        }
                                                }
AlgScope : {- empty -}         { % lift $ do {i <- getScopeNumber; 
                                              pushS (StackEntry i SType Nothing []);
                                              addNumber } }

Sums : Sums Sum                { }
     | Sum                     { }

Sum : ConsScope type '(' Prods ')' ';'   { % do{
                                                    pervasiveCheck (tokenVal $2) (tokenPos $2);
                                                    i <- lift getActualScope;
                                                    lift popS;
                                                    j <- lift getActualScope;
                                                    redeclaredCheck (tokenVal $2) j (tokenPos $2);
                                                    lift $ insertSymS (tokenVal $2) (SymScope j (TypeType,0) [] (tokenPos $3)); 
                                               } 
                                         }
    | ConsScope type '(' ')' ';'         { % lift  popS }

ConsScope : {- empty -}        { % lift $ do {i <- getScopeNumber; 
                                            pushS (StackEntry i SCons Nothing []);
                                            addNumber } }

Declaration : Type Ids ';'     { % do { (zipWithM_ pervasiveCheck (map tokenVal $2) (map tokenPos $2));
                                        i <- lift getActualScope;
                                        zipWith3M_ redeclaredCheck (map tokenVal $2) (repeat i) (map tokenPos $2);
                                        t <- getType $1;
                                        lift $ mapM_ (\x -> insertSymS (tokenVal x) (SymScope i t [] (tokenPos x))) (reverse $2); 
                                      } 
                               }

IDeclaration : Type DAssign { % do { zipWithM_ pervasiveCheck (map idString (fst $2)) (map idPos (fst $2));
                                     t <- getType $1;
                                     ts <- mapM (\e -> checkDAssign (fst t) e (idPos $ head (fst $2))) (snd $2);
                                     mapM_ (\(Variable _ (x,y,z)) ->
                                                redeclaredCheck x y z >>
                                                (lift $ insertSymS x (SymScope y t [] z))) (fst $2);
                                     vs <- zipWithM (\(Variable _ v) tc -> return $ Variable tc v) (fst $2) ts;
                                     return (vs, reverse (snd $2));
                                   }
                            }


DAssign : id ',' DAssign ',' RV { % do{ i <- lift getActualScope;
                                        return (( \(l,r) -> ((Variable TypeError (tokenVal $1,i,tokenPos $1)):l,$5:r) ) $3)
                                      } 
                                }
        | id '=' RV             { % do{ i <- lift getActualScope; return ([Variable TypeError (tokenVal $1,i,tokenPos $1)],[$3]) } }

Prods : Prods ',' Prod         { }
      | Prod                   { }

Prod : Type id                 { %  do{
                                        pervasiveCheck (tokenVal $2) (tokenPos $2);                                        
                                        i <- lift getActualScope;
                                        redeclaredCheck (tokenVal $2) i (tokenPos $2);
                                        t <- getType $1;
                                        lift $ insertSymS (tokenVal $2) (SymScope i t [] (tokenPos $2));
                                       }
                               }

-- Identificadores
Ids : id ',' Ids               { % return $ $1 : $3 }
    | id                       { % return $ [$1] }

Id : id                        { % do{ 
                                        (s,t) <- searchTable' (tokenVal $1);
                                        i <- lift getActualScope;
                                        return $ (Variable t s); 
                                     }
                               }
   | Id '[' Exp ']'            { % checkIndex (idString $1) $3 (idPos $1) >>=(\t -> return $ Index t $1 $3) }
   | Id '.' MCall              { % return $ MemberCall TypeError $1 $3 }

MCall : MCall '.' id           { % return ($3 : $1) }
      | id                     { % return [$1] }

Types : Types ',' Type         { % return ($3 : $1) }
      | Type                   { % return [$1] }

Type : type                    { % return (Name TypeType (tokenVal $1)) }
     | '[' Type ']'            { % return (List TypeType $2) }
     | '{' Type ':' num '}'    { % return (Array TypeType $2 $4) }
     | '[' Type ':' Type ']'   { % return (Dict TypeType ($2,$4)) }
     | '(' Types ')'           { % return (Tuple TypeType $ reverse $2) }
     | ':' Type '>'            { % return (Pointer TypeType $2) }

Assign : Id ',' Assign ',' RV  { % return (( \(l,r) -> ($1:l,$5:r) ) $3) }
       | Id '=' RV             { % return ([$1],[$3]) }

RV : Exp    { % return $   (ValueExp $1) }
   | Cons   { % return $   (ValueCons $1) }

Cons : type '(' ')'      { % return $   (CCall $1 []) }
     | type '(' Exps ')' { % return $   (CCall $1 (reverse $3)) }

-- Expresiones
Exp : Exp '+' Exp              { % checkNumBin $1 $3 (tokenPos $2) >>= (\t -> return $ ESum t $1 $3) }
    | Exp '-' Exp              { % checkNumBin $1 $3 (tokenPos $2) >>= (\t -> return $ EDif t $1 $3) }
    | Exp '*' Exp              { % checkNumBin $1 $3 (tokenPos $2) >>= (\t -> return $ EMul t $1 $3) }
    | Exp '/' Exp              { % checkNumBin $1 $3 (tokenPos $2) >>= (\t -> return $ EDiv t $1 $3) }
    | Exp '%' Exp              { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ EMod t $1 $3) }
    | Exp '**' Exp             { % checkNumBin $1 $3 (tokenPos $2) >>= (\t -> return $ EPot t $1 $3) }
    | Exp '//' Exp             { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ EDivE t $1 $3) }
    | Exp '<<' Exp             { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ ELShift t $1 $3) }
    | Exp '>>' Exp             { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ ERShift t $1 $3) }
    | Exp '|' Exp              { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ EBitOr t $1 $3) }
    | Exp '^' Exp              { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ EBitXor t $1 $3) }
    | Exp '&' Exp              { % checkIntBin $1 $3 (tokenPos $2) >>= (\t -> return $ EBitAnd t $1 $3) }
    | Exp '||' Exp             { % checkBoolBin $1 $3 (tokenPos $2) >>= (\t -> return $ EOr t $1 $3) }
    | Exp '&&' Exp             { % checkBoolBin $1 $3 (tokenPos $2) >>= (\t -> return $ EAnd t $1 $3) }
    | Exp '>' Exp              { % checkNumComp $1 $3 (tokenPos $2) >>= (\t -> return $ EGreat t $1 $3) }
    | Exp '<' Exp              { % checkNumComp $1 $3 (tokenPos $2) >>= (\t -> return $ ELess t $1 $3) }
    | Exp '>=' Exp             { % checkNumComp $1 $3 (tokenPos $2) >>= (\t -> return $ EGEq t $1 $3) }
    | Exp '<=' Exp             { % checkNumComp $1 $3 (tokenPos $2) >>= (\t -> return $ ELEq t $1 $3) }
    | Exp '==' Exp             { % checkComp $1 $3 (tokenPos $2) >>= (\t -> return $ EEqual t $1 $3) }
    | Exp '/=' Exp             { % checkComp $1 $3 (tokenPos $2) >>= (\t -> return $ ENEq t $1 $3) }
    | '-' Exp %prec NEG        { % checkNumUn $2 (tokenPos $1) >>= (\t -> return $ ENeg t $2) }
    | '!' Exp                  { % checkBoolUn $2 (tokenPos $1) >>= (\t -> return $ ENot t $2) }
    | '~' Exp                  { % checkNumUn $2 (tokenPos $1) >>= (\t -> return $ EBitNot t $2) }
    | '(' Exp ')'              { % return $2 }
    | Id                       { % return (EIdent (returnType $1) $1) }
    | num                      { % return (EToken (getNumType $ tokenVal $1) $1) }
    | true                     { % return (EToken TypeBool $1) }
    | false                    { % return (EToken TypeBool $1) }
    | str                      { % return (EToken TypeString $1) }
    | char                     { % return (EToken TypeChar $1) }
    | List                     { % return $1 }
    | Arr                      { % return $1 }
    | Dict                     { % return $1 }
    | Tup                      { % return $1 }
    | FunCall                  { % return (EFCall (returnType $1) $1) }
    | Read                     { % return $1 }
    | Id '?'                   { % return (ERef TypeError $1) }

Exps : Exps ',' Exp            { % return $   ($3 : $1) }
     | Exp                     { % return $   [$1] }

List : '[' Exps ']'            { % checkListType $2 (tokenPos $1) >>=(\t -> return $ EList t (reverse $2)) }
     | '[' ']'                 { % return $   (EList TypeError []) }

Arr : '{' Exps '}'             { % checkArrType $2 (tokenPos $1) >>= (\t -> return $ EArr t (reverse $2)) }
    | '{' '}'                  { % return $   (EArr TypeError []) }

Dict : '[' KV ']'              { % checkDictType $2 (tokenPos $1) >>= (\t -> return $ EDict t (reverse $2)) }

KV : KV ',' Exp ':' Exp        { % return $   (($3,$5) : $1)}
   | Exp ':' Exp               { % return $   [($1,$3)] }

Tup : '(' Exp ',' Exps ')'     { % checkTupType ($2:(reverse $4)) (tokenPos $1) >>= (\t -> return $ ETup t ($2 : (reverse $4))) }

-- Funciones
FunCall : id '(' Exps ')'      { % checkFunCall (tokenVal $1) $3 (tokenPos $1) >>= (\x -> return $ FCall x $1 $3) }
        | id '(' ')'           { % checkFunCall (tokenVal $1) [] (tokenPos $1) >>= (\x -> return $ FCall x $1 []) }

Function : FuncScope func '('ParRet ')' Block { % do { lift popS;
                                                       i <- lift getActualScope; 
                                                       (\((_,_),(_,n)) -> 
                                                           insertIns (tokenVal n) i $6) $4;
                                                       t <- mapM getType (fst $ snd $4); 
                                                       (checkRetT (snd $ snd $4) $6 (map fst t)); 
                                                     }
                                              }
         | FuncScope func '(' ParNoRet ')' Block { % do { lift popS;
                                                       i <- lift getActualScope; 
                                                       (\((_,_),(_,n)) -> 
                                                           insertIns (tokenVal n) i $6) $4; 

                                                     }
                                              }
         | FuncScope func '(' '->' AddFuncRet '(' ')' Block { % do { lift popS;
                                                                  i <- lift getActualScope; 
                                                                  insertIns (tokenVal $5) i $8;
                                                                }
                                                         }
         | FuncScope func '(' ')' AddFunc '(' ')' Block  { % do { lift popS;
                                                                  i <- lift getActualScope; 
                                                                  insertIns (tokenVal $5) i $8;
                                                                }
                                                         }

AddFuncRet : Types ')' id { % do { i <- lift getActualScope;
                                   t <- mapM getType (reverse $1);
                                   lift $ insertSymS (tokenVal $3) (SymScope i (TypeFunc [] (map fst t),0) [] (tokenPos $3));
                                   return $3;
                         } }

AddFunc : id   { % do { i <- lift getActualScope;
                        lift $ insertSymS (tokenVal $1) (SymScope i (TypeFunc [] [],0) [] (tokenPos $1));
                        return $1;
                         } }



FuncScope : {- empty -}                    { % lift $ do {i <- getScopeNumber; 
                                                          pushS (StackEntry i SFunc Nothing []);
                                                          addNumber } }

ParRet : Type Ret id          { % do { i <- lift getActualScope;
                                       l <- return ((\((x,y),(z,n)) -> (($1 : x,reverse ($3 : y) ),(z,n))) $2); 
                                       t <- mapM getType (fst $ fst l);
                                       zipWithM_ pervasiveCheck (map tokenVal (snd $ fst l)) (map tokenPos (snd $ fst l));
                                       zipWith3M_ redeclaredCheck (map tokenVal (snd $ fst l)) (repeat i) (map tokenPos (snd $ fst l));
                                       mapM_ searchTable (map typeString (fst $ snd l));
                                       lift $ zipWithM_ (\x y -> 
                                           insertSymS (tokenVal y) (SymScope i x [] (tokenPos y))) t (snd $ fst l);
                                       funi <- lift $ peekScope 1;
                                       outypes <- mapM getType (fst $ snd l);
                                       lift $ (\((_,_),(_,tid)) -> 
                                         insertSymS (tokenVal tid) (SymScope funi (TypeFunc (map fst t) (map fst outypes),0) [] (tokenPos tid))) l;
                                       return l; } }

ParNoRet : Type NoRet id    { % do { i <- lift getActualScope;
                                     l <- return ((\((x,y),(_,n)) -> (($1 : x,reverse($3 : y) ),([],n))) $2); 
                                     t <- mapM getType (fst $ fst l);
                                     zipWithM_ pervasiveCheck (map tokenVal (snd $ fst l)) (map tokenPos (snd $ fst l));
                                     zipWith3M_ redeclaredCheck (map tokenVal (snd $ fst l)) (repeat i) (map tokenPos (snd $ fst l));
                                     lift $ zipWithM_ (\x y -> 
                                         insertSymS (tokenVal y) (SymScope i x [] (tokenPos y))) t (snd $ fst l);
                                     funi <- lift $ peekScope 1;
                                     lift $ (\((_,_),(_,tid)) -> 
                                       insertSymS (tokenVal tid) (SymScope funi (TypeFunc (map fst t) [],0) [] (tokenPos tid))) l;
                                     return l; } }

Ret : ',' Type Ret id ','     { % return $ (\((x,y),(z,n)) -> (($2 : x,$4 : y ),(z,n))) $3  }
    | '->' Types ')' id '('   { % return (([],[]),(reverse $2,$4)) }

NoRet : ',' Type NoRet id ',' { % return $ (\((x,y),(_,n)) -> (($2 : x,$4 : y),([],n))) $3 }
      | ')' id '('            { % return (([],[]),([],$2)) }

-- Selectores
Selector : If                  { % return $ $1 }
         | Case                { % return $ Block TypeError [] }

If : if Exp then In            { % checkIf $2 $4 (tokenPos $1) >>= (\t -> return $ IfThen t $2 $4) }
   | if Exp then In else In    { % checkIfE $2 $4 $6 (tokenPos $1) >>= (\t -> return $ IfElse t $2 $4 $6) }

Case : case Exp of Conds ';'   { }

-- Condiciones
Conds : Conds Cond             { }
      | {- empty -}            { }

Cond : Exp In                  { }

-- Iteradores
Iterator : Indet               { % return $ $1 }
         | Det                 { % return $ Det TypeError $1 }

Indet : while Exp In           { % return $ (While TypeError $2 $3) }

Det : ForScope for ForDec from Exp to Exp In                  { % lift $ popS >> (return $ FromTo TypeError $5 $7 $8)}
    | ForScope for ForDec from Exp to Exp if Exp In           { % lift $ popS >> (return $ FromToIf TypeError $5 $7 $9 $10)}
    | ForScope for ForDec from Exp to Exp with Exp if Exp In  { % lift $ popS >> (return $ FromToWithIf TypeError $5 $7 $9 $11 $12)}
    | ForScope for ForDec from Exp to Exp with Exp In         { % lift $ popS >> (return $ FromToWith TypeError $5 $7 $9 $10)}
    | ForScope for ForDec in Exp if Exp In                    { % lift $ popS >> (return $ InIf TypeError $5 $7 $8)}

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
