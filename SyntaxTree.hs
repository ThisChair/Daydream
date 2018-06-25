module SyntaxTree where
import Lexer

class AST a where
    returnType :: a -> Type

data Init = Init Type Module [Import] [Instruction] deriving (Show)

data Module = Module Type Token | Main Type deriving (Show)

data Import = Import Type Token deriving (Show)

data Instruction = 
    Block Type [Instruction]                |
    Assign Type ([Identifier],[RightValue]) |
    IfThen Type Exp Instruction             |
    IfElse Type Exp Instruction Instruction |
    While Type Exp Instruction              |
    Det Type For                            |
    Ret Type Exp                            |
    Continue Type                           |
    Break Type                              |
    Print Type Exp                          |
    PrintLn Type Exp
    deriving (Show)

data For = 
    FromTo       Type Exp Exp Instruction         |
    FromToIf     Type Exp Exp Exp Instruction     |
    FromToWithIf Type Exp Exp Exp Exp Instruction |
    FromToWith   Type Exp Exp Exp Instruction     |
    InIf         Type Exp Exp Instruction
    deriving (Show)

data Member = Member TypeName Token deriving (Show)

data Constructor = Constructor Token [Member] deriving (Show)

data RightValue =
    ValueExp Exp   |
    ValueCons CCall
    deriving (Show)

data CCall = CCall Token [Exp] deriving (Show)

data DataType = DataType Token [Constructor] deriving (Show)

typeString :: TypeName -> String
typeString (Name _ s) = s
typeString (List _ _) = "_list"
typeString (Array _ _ _) = "_array"
typeString (Tuple _ _) = "_tuple"
typeString (Dict _ _) = "_dict"

data TypeName = 
    Name Type String              |
    Array Type TypeName Token     |
    List Type TypeName            |
    Tuple Type [TypeName]         |
    Dict Type (TypeName,TypeName) 
    deriving (Show)

idString :: Identifier -> String
idString (Variable _ (s,_,_)) = s
idString _ = error $ "No variable"

idPos :: Identifier -> AlexPosn
idPos (Variable _ (_,_,p)) = p
idPos _ = error $ "No variable"

data Identifier = 
    Variable Type (String,Integer,AlexPosn) |
    Index Type Identifier Exp               |
    MemberCall Type Identifier [Token]
    deriving (Show) 

data Exp = 
    ESum    Type Exp Exp     |
    EDif    Type Exp Exp     |
    EMul    Type Exp Exp     |
    EDiv    Type Exp Exp     |
    EMod    Type Exp Exp     |
    EPot    Type Exp Exp     |
    EDivE   Type Exp Exp     |
    ELShift Type Exp Exp     |
    ERShift Type Exp Exp     |
    EBitOr  Type Exp Exp     |
    EBitAnd Type Exp Exp     |
    EBitXor Type Exp Exp     |
    EOr     Type Exp Exp     |
    EAnd    Type Exp Exp     |
    EGEq    Type Exp Exp     |
    EGreat  Type Exp Exp     |
    ELEq    Type Exp Exp     |
    ELess   Type Exp Exp     |
    ENEq    Type Exp Exp     |
    EEqual  Type Exp Exp     |
    ENeg    Type Exp         |
    ENot    Type Exp         |
    EBitNot Type Exp         |
    EFCall  Type FCall       |
    EToken  Type Token       |
    EList   Type [Exp]       |
    EArr    Type [Exp]       |
    EDict   Type [(Exp,Exp)] |
    ETup    Type [Exp]       |
    EIdent  Type Identifier  |
    Read    Type             |
    ERef    Type Identifier
    deriving (Show)

data FCall = FCall Type Token [Exp] deriving (Show)

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
            deriving (Show,Eq)

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
