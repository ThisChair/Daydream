{-|
Module : SyntaxTree
Authors : Carlos Infante
          Daniel Varela

Functions and definitions relevant to the construction and manipulation
of the abstract syntax tree.
-}
module SyntaxTree where

import Data.List (nub)

import Lexer
import Utils

class AST a where
    -- | Returns the type of the node.
    returnType :: a -> Type

-- Abstract syntax tree nodes definition.

data Init
    -- | Root of the tree.
    = Init Type Module [Import] [Instruction]
    deriving (Show)

data Module
    -- | Module name definition.
    = Module Type Token 
    -- | If no module name present, then it is Main.
    | Main Type
    deriving (Show)

data Import
    -- | All imports.
    = Import Type Token
    deriving (Show)

data Instruction
    -- | Instruction blocks.
    = Block Type [Instruction]
    -- | Assigns.
    | Assign Type ([Identifier],[RightValue])
    -- | If-Then instruction.
    | IfThen Type Exp Instruction
    -- | If-Then-Else instruction.
    | IfElse Type Exp Instruction Instruction
    -- | Undefined iterator While instruction.
    | While Type Exp Instruction
    -- | Determined iterator For instructions.
    | Det Type For
    -- | Return instruction.
    | Ret Type Exp
    -- | Continue instruction.
    | Continue Type
    -- | Break instruction.
    | Break Type
    -- | Print instruction.
    | Print Type Exp
    -- | PrintLn instruction.
    | PrintLn Type Exp
    -- | Function call instruction.
    | IFCall Type FCall
    deriving (Show,Eq)

data For
    -- | For-From-To instruction.
    = FromTo Type Exp Exp Instruction
    -- | For-From-To-If instruction.
    | FromToIf Type Exp Exp Exp Instruction
    -- | For-From-To-With-If instruction.
    | FromToWithIf Type Exp Exp Exp Exp Instruction
    -- | For-From-To-With instruction.
    | FromToWith Type Exp Exp Exp Instruction
    -- | For-In-If instruction.
    | InIf Type Exp Exp Instruction
    deriving (Show,Eq)

data Member
    -- | Member of a structured type.
    = Member TypeName Token
    deriving (Show,Eq)

data Constructor    
    -- | Type constructor.
    = Constructor Token [Member]
    deriving (Show,Eq)

data RightValue
    -- | Expression as a right value.
    = ValueExp Exp
    -- | Type constructor call as a right value.
    | ValueCons CCall
    deriving (Show,Eq)

data CCall
    -- | Type constructor call.
    = CCall Token [Exp]
    deriving (Show,Eq)

data DataType
    -- | Structured data types.
    = DataType Token [Constructor]
    deriving (Show,Eq)

data TypeName
    -- | Basic or structured type.
    = Name Type String
    -- | Array with its type and size.
    | Array Type TypeName Token
    -- | List with its type.
    | List Type TypeName
    -- | Tuple with its types.
    | Tuple Type [TypeName]
    -- | Dictionary with its first and second type.
    | Dict Type (TypeName,TypeName)
    -- | Pointer with its type.
    | Pointer Type TypeName
    deriving (Show,Eq)

-- | Type name as a string for symtable.
typeString :: TypeName -> String
typeString (Name _ s) = s
typeString (List _ _) = "_list"
typeString (Array _ _ _) = "_array"
typeString (Tuple _ _) = "_tuple"
typeString (Dict _ _) = "_dict"
typeString (Pointer _ _) = "_pointer"

data Identifier
    -- | Variable name, scope and position.
    = Variable Type (String,Integer,AlexPosn)
    -- | Index operation over an identifier.
    | Index Type Identifier Exp
    -- | Member call over an identifier.
    | MemberCall Type Identifier [Token]
    deriving (Show,Eq)

-- | Returns the name of an identifier.
idString :: Identifier -> String
idString (Variable _ (s,_,_)) = s
idString (Index _ id _) = idString id
idString (MemberCall _ id _) = idString id

-- | Returns the position of an identifier.
idPos :: Identifier -> AlexPosn
idPos (Variable _ (_,_,p)) = p
idPos (Index _ id _) = idPos id
idPos (MemberCall _ id _) = idPos id

data Exp
    -- | Sum between two expressions.
    = ESum Type Exp Exp
    -- | Difference between two expressions.
    | EDif Type Exp Exp
    -- | Multiplication between two expressions.
    | EMul Type Exp Exp
    -- | Division between two expressions.
    | EDiv Type Exp Exp
    -- | Module between two expressions.
    | EMod Type Exp Exp
    -- | Power between two expressions.
    | EPot Type Exp Exp
    -- | Integer division between two expressions.
    | EDivE Type Exp Exp
    -- | Left shift between two expressions.
    | ELShift Type Exp Exp
    -- | Right shift between two expressions.
    | ERShift Type Exp Exp
    -- | Bitwise or between two expressions.
    | EBitOr  Type Exp Exp
    -- | Bitwise and between two expressions.
    | EBitAnd Type Exp Exp
    -- | Bitwise exclusive or between two expressions.
    | EBitXor Type Exp Exp
    -- | Logic or between two expressions.
    | EOr Type Exp Exp
    -- | Logic and between two expressions.
    | EAnd Type Exp Exp
    -- | Greater or equal than between two expressions.
    | EGEq Type Exp Exp
    -- | Greater than between two expressions.
    | EGreat Type Exp Exp
    -- | Less or equal than between two expressions.
    | ELEq Type Exp Exp
    -- | Less than between two expressions.
    | ELess Type Exp Exp
    -- | Not equal than between two expressions.
    | ENEq    Type Exp Exp
    -- | Equal than between two expressions.
    | EEqual  Type Exp Exp
    -- | Negative of an expression.
    | ENeg Type Exp
    -- | Logic complement of an expression.
    | ENot Type Exp
    -- | Bitwise complement of an expression.
    | EBitNot Type Exp
    -- | Function call as an expression.
    | EFCall Type FCall
    -- | Token as an expression.
    | EToken Type Token
    -- | List expression.
    | EList Type [Exp]
    -- | Array expression.
    | EArr Type [Exp]
    -- | Dictionary expression.
    | EDict Type [(Exp,Exp)]
    -- | Tuple expression.
    | ETup Type [Exp]
    -- | Identifier as an expression.
    | EIdent Type Identifier
    -- | Reference as an expression.
    | ERef Type Identifier
    -- | Read expression.
    | Read Type
    deriving (Show,Eq)

data FCall
    -- | Function call.
    = FCall Type Token [Exp]
    deriving (Show,Eq)

-- Types definition.

data Type = TypeInt                |
            TypeFloat              |
            TypeBool               |
            TypeChar               |
            TypeString             |
            TypeVoid               |
            TypeError              |
            TypeType               |
            TypeArray Type String  |
            TypeList Type          |
            TypeDict Type Type     |
            TypeTuple [Type]       |
            TypeFunc [Type] [Type] |
            TypePointer Type       |
            TypeData String
            deriving (Eq)

instance Show Type where
    show TypeInt = "Int"
    show TypeFloat = "Float"
    show TypeBool = "Bool"
    show TypeChar = "Char"
    show TypeString = "String"
    show TypeVoid = "Void"
    show TypeError = "Type Error"
    show TypeType = "Type"
    show (TypeArray t n) = "Array of " ++ (show t) ++ " of size " ++ n
    show (TypeList t) = "List of " ++ (show t)
    show (TypeDict k v) = "Dictionary of pairs (" ++ (show k) ++ ", " ++ (show v) ++ ")"
    show (TypeTuple t) = "Tuple of: " ++ (show t)
    show (TypeFunc a r) = "Function from " ++ (show a) ++ " to " ++ (show r)
    show (TypePointer t) = "Pointer of " ++ (show t)
    show (TypeData s) = s

-- Funciones para retorno de tipos -- 

-- Init 
instance AST Init where
    returnType (Init t _ _ _) = t

-- Module
instance AST Module where
    returnType (Module t _) = t 
    returnType (Main t) = t

-- Import
instance AST Import where
    returnType (Import t _) = t

-- Instruction
instance AST Instruction where
    returnType (Block t _) = t
    returnType (Assign t _) = t
    returnType (IfThen t _ _) = t
    returnType (IfElse t _ _ _) = t 
    returnType (While t _ _) = t 
    returnType (Det t _) = t 
    returnType (Ret t _) = t 
    returnType (Continue t) = t 
    returnType (Break t) = t 
    returnType (Print t _) = t 
    returnType (PrintLn t _) = t

-- For
instance AST For where
    returnType (FromTo t _ _ _) = t
    returnType (FromToIf t _ _ _ _) = t
    returnType (FromToWithIf t _ _ _ _ _) = t
    returnType (FromToWith t _ _ _ _) = t
    returnType (InIf t _ _ _) = t

-- TypeName
instance AST TypeName where
    returnType (Name t _) = t
    returnType (Array t _ _) = t
    returnType (List t _) = t
    returnType (Tuple t  _) = t
    returnType (Dict t _) = t
    returnType (Pointer t _) = t

-- Identifier
instance AST Identifier where
    returnType (Variable t _) = t
    returnType (Index t _ _) = t
    returnType (MemberCall t _ _) = t

-- Exp
instance AST Exp where
    returnType (ESum t _ _ ) = t
    returnType (EDif t _ _ ) = t
    returnType (EMul t _ _ ) = t
    returnType (EDiv t _ _ ) = t
    returnType (EMod t _ _ ) = t
    returnType (EPot t _ _ ) = t
    returnType (EDivE t _ _ ) = t
    returnType (ELShift t _ _ ) = t
    returnType (ERShift t _ _ ) = t
    returnType (EBitOr t _ _ ) = t
    returnType (EBitAnd t _ _ ) = t
    returnType (EBitXor t _ _ ) = t
    returnType (EOr t _ _ ) = t
    returnType (EAnd t _ _ ) = t
    returnType (EGEq t _ _ ) = t
    returnType (EGreat t _ _ ) = t
    returnType (ELEq t _ _ ) = t
    returnType (ELess t _ _ ) = t
    returnType (ENEq t _ _ ) = t
    returnType (EEqual t _ _ ) = t
    returnType (ENeg t _ ) = t
    returnType (ENot t _ ) = t
    returnType (EBitNot t _ ) = t 
    returnType (EFCall t _ ) = t
    returnType (EToken t _ ) = t
    returnType (EList t _ ) = t
    returnType (EArr t _ ) = t
    returnType (EDict t _ ) = t
    returnType (ETup t _ ) = t
    returnType (EIdent t _ ) = t
    returnType (Read t ) = t
    returnType (ERef t _ ) = t

-- FCall
instance AST FCall where
    returnType (FCall t _ _) = t

-- | Checks that the type in a list of expressions is unique.
expsType :: [Exp] -> Type
expsType es = case (nub $ map returnType es) of
    t:[] -> t
    _ -> TypeError