module SyntaxTree where
import Lexer

data Init = Init [Import] [Instruction] deriving (Show)

data Import = Import Token deriving (Show)

data Instruction = 
    Block [Instruction]                           |
    TypeDec DataType                              |
    Assign ([Identifier],[RightValue])            |
    IfThen Expression [Instruction]               |
    IfElse Expression [Instruction] [Instruction] |
    While Expression [Instruction]
    deriving (Show)

data Member = Member TypeName Token deriving (Show)

data Constructor = Constructor Token [Member] deriving (Show)

data RightValue =
    ValueExp Expression   |
    ValueCons CCall
    deriving (Show)

data CCall = CCall Token [Expression]

data DataType = DataType Token [Constructor] deriving (Show)


data TypeName = 
    Name Token               |
    Array TypeName Token     |
    List TypeName            |
    Tuple [TypeName]         |
    Dict (TypeName,TypeName) 
    deriving (Show)

data Declare =
    Dec [TypeName] [Token]                |
    DecA [TypeName] ([Token],[RightValue])
    deriving (Show)

data Identifier = 
    Variable Token               |
    Index Identifier Token       |
    MemberCall Identifier [Token]
    deriving (Show) 

data Expression = 
    ESum    SumOp     |
    EDif    Dif       |
    EMul    Mul       |
    EDiv    Div       |
    EMod    Mod       |
    EPot    Pot       |
    EDivE   DivE      |
    ELShift ELShift   |
    ERShift ERShift   |
    EBitOr  BitOr     |
    EBitAnd EBitAnd   |
    EBitXor EBitXor   |
    EOr     Or        |
    EAnd    And       |
    EGeq    Geq       |
    EGreat  Great     |
    ELeq    Leq       |
    ELess   Less      |
    ENeq    Neq       |
    EEqual  Equal     |
    ENeg    Expression |
    ENot    Expression |
    EBitNot Expression |
    EFCall  FCall     |
    EToken  Token     |
    EList   [Expression] |
    EArr    [Expression] |
    EDict   [(Expression):(Expression)] |
    ETup    [Expression] |
    EIdent  Identifier
    deriving (Show)

data FCall = FCall Token [Expression] deriving (Show)
