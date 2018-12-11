{-|
Module : Grammar
Authors : Carlos Infante
          Daniel Varela

Functions for processing each grammar rule. 
t1...tn are equivalent to $1...$n
-}
module Grammar where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Class(lift)
import Control.Monad(zipWithM_,zipWithM)

import Lexer
import SyntaxTree
import SymTable
import Utils

---------------------------
-- Misc --
---------------------------

-- | S -> Mod, Imports, Body
start t1 t2 t3 = return $ Init TypeVoid t1 (reverse t2) (reverse t3)

-- | Imports -> Imports import type
imports t1 t3 = return $ Import TypeVoid t3 : t1

---------------------------
-- Instructions --
---------------------------

-- | In -> Block
iblock t1 = return $ Block TypeVoid t1

-- | SingleI -> IDeclaration
idec t1 = return $ Assign (assignType t1) t1

-- | SingleI -> Assign
asgn t1 = checkAssign t1 >>= (\t -> return $ Assign t t1)

-- | SingleI -> return Exp
ireturn t1 t2 = checkInFun (tokenPos t1) >> return (Ret TypeVoid t2)

-- | SingleI -> FunCall
ifuncall t1 = checkIFunCall t1 >>= (\x -> return $ IFCall x t1)

-- | Block -> BlockScope dream Body wake 
exitbs t3 = lift $ popS >> return t3

-- | BlockScope -> Lambda
enterbs :: ParseMonad ()
enterbs = lift $ do 
    i <- getScopeNumber
    pushS (StackEntry i SBlock Nothing [])
    addNumber

---------------------------
-- ADT --
---------------------------

-- | Algebraic -> AlgScope data type '|:' Sums ':|'
outalg t3 = do
    pervasiveCheck (tokenVal t3) (tokenPos t3)
    i <- lift getActualScope
    lift popS
    j <- lift getActualScope
    redeclaredCheck (tokenVal t3) j (tokenPos t3)
    lift $ insertSymS (tokenVal t3) (SymScope j (TypeType,0) [] (tokenPos t3))

-- | AlgScope -> Lambda
enteralg :: ParseMonad()
enteralg = lift $ do 
    i <- getScopeNumber 
    pushS (StackEntry i SType Nothing [])
    addNumber

-- | Sum -> ConsScope type '(' Prods ')' ';'
insum t2 t3 = do
    pervasiveCheck (tokenVal t2) (tokenPos t2)
    i <- lift getActualScope
    lift popS
    j <- lift getActualScope
    redeclaredCheck (tokenVal t2) j (tokenPos t2)
    lift $ insertSymS (tokenVal t2) (SymScope j (TypeType,0) [] (tokenPos t3))

-- | ConsScope -> Lambda
entercons :: ParseMonad()
entercons = lift $ do 
    i <- getScopeNumber; 
    pushS (StackEntry i SCons Nothing []);
    addNumber

-- | Prod -> Type id
inprods t1 t2 = do
    pervasiveCheck (tokenVal t2) (tokenPos t2)                                        
    i <- lift getActualScope
    redeclaredCheck (tokenVal t2) i (tokenPos t2)
    t <- getType t1
    lift $ insertSymS (tokenVal t2) (SymScope i t [] (tokenPos t2))

---------------------------
-- Declarations
---------------------------

-- | Declaration -> Type Ids ';'
sdecl t1 t2 = do 
    zipWithM_ pervasiveCheck (map tokenVal t2) (map tokenPos t2)
    i <- lift getActualScope
    zipWith3M_ redeclaredCheck (map tokenVal t2) (repeat i) (map tokenPos t2)
    t <- getType t1
    lift $ mapM_ (\x -> insertSymS (tokenVal x) (SymScope i t [] (tokenPos x))) (reverse t2)

-- | IDeclaration -> Type DAssign
idecl t1 t2 = do 
    zipWithM_ pervasiveCheck (map idString (fst t2)) (map idPos (fst t2))
    t <- getType t1
    ts <- mapM (\e -> checkDAssign (fst t) e (idPos $ head (fst t2))) (snd t2)
    mapM_ (\(Variable _ (x,y,z)) ->
        redeclaredCheck x y z >>
           lift (insertSymS x (SymScope y t [] z))) (fst t2)
    vs <- zipWithM (\(Variable _ v) tc -> return $ Variable tc v) (fst t2) ts
    return (vs, reverse (snd t2))

-- | DAssign -> id ',' DAssign ',' RV
dass t1 t3 t5 = do
    i <- lift getActualScope
    return (( \(l,r) -> (Variable TypeVoid (tokenVal t1,i,tokenPos t1):l,t5:r) ) t3)

-- | DAssign -> id '=' RV 
dass' t1 t3 = do
    i <- lift getActualScope
    return ([Variable TypeVoid (tokenVal t1,i,tokenPos t1)],[t3])

---------------------------
-- Identifiers
---------------------------

-- | Id -> id
idname t1 = do
    (s,t) <- searchTable' (tokenVal t1)
    i <- lift getActualScope
    return (Variable t s)

-- | Id -> Id '[' Exp ']' 
idind t1 t3 = do
    t <- checkIndex (idString t1) t3 (idPos t1)
    return $ Index t t1 t3

---------------------------
-- Selectors
---------------------------

-- | If -> if Exp then In 
ifthenr t1 t2 t4 = do
    t <- checkIf t2 t4 (tokenPos t1)
    return $ IfThen t t2 t4

-- | If -> if Exp then In else In
ifthener t1 t2 t4 t6 = do
    t <- checkIfE t2 t4 t6 (tokenPos t1)
    return $ IfElse t t2 t4 t6

---------------------------
-- Iterators
---------------------------

-- | Indet -> while Exp In 
whiler t1 t2 t3 = do
    t <- checkWhile t2 t3 (tokenPos t1)
    return $ While t t2 t3

-- | Det -> ForScope for ForDec from Exp to Exp In
forfromto t2 ((t,_),v) t5 t7 t8 = do
    t_low <- checkNumUn t5 (tokenPos t2)
    t_high <- checkNumUn t7 (tokenPos t2)
    let low_err = t_low == TypeError
        high_err = t_high == TypeError
    if low_err || high_err then 
        return (Block TypeError [])
    else do
        i <- lift getActualScope
        let var = Variable t (tokenVal v,i,tokenPos v)
            evar = EIdent t var
            ass = Assign TypeVoid ([var],[ValueExp t5])
            ex = ELess TypeBool evar t7
            one = EToken TypeInt (TNum noPos "1")
            s = ESum t evar one
            act = Assign TypeVoid ([var],[ValueExp s])
            bl = Block TypeVoid [t8,act]
            w = While TypeVoid ex bl
        lift popS
        return (Block TypeVoid [ass,w])

-- | Det -> ForScope for ForDec from Exp to Exp if Exp In
forfromtoif t2 ((t,_),v) t5 t7 t8 t9 = do
    t_low <- checkNumUn t5 (tokenPos t2)
    t_high <- checkNumUn t7 (tokenPos t2)
    t_cond <- checkBoolUn t8 (tokenPos t2)
    let low_err = t_low == TypeError
        high_err = t_high == TypeError
        cond_err = t_cond == TypeError
    if low_err || high_err || cond_err then
        return (Block TypeError [])
    else do
        i <- lift getActualScope
        let var = Variable t (tokenVal v,i,tokenPos v)
            evar = EIdent t var
            ass = Assign TypeVoid ([var],[ValueExp t5])
            ex = ELess TypeBool evar t7
            one = EToken TypeInt (TNum noPos "1")
            s = ESum t evar one
            act = Assign TypeVoid ([var],[ValueExp s])
            bl = Block TypeVoid [t9,act]
            w = While TypeVoid ex bl
            fb = Block TypeVoid [ass,w]
        lift popS
        return (IfThen TypeVoid t8 fb)