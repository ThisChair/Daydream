module SyntaxTree where
import Lexer

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