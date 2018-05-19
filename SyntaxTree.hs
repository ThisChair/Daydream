module SyntaxTree where
import Lexer
import Control.Monad.Trans.Except

type ParseMonad = ExceptT String IO

data Init = Init [Import] [Instruction] deriving (Show)

data Import = Import Token deriving (Show)

data Instruction = 
    Block [Instruction]                           |
    TypeDec DataType                              |
    Assign ([Identifier],[RightValue])            |
    IfThen Exp [Instruction]               |
    IfElse Exp [Instruction] [Instruction] |
    While Exp [Instruction]                |
    Det |
    NonExists
    deriving (Show)


data Member = Member TypeName Token deriving (Show)

data Constructor = Constructor Token [Member] deriving (Show)

data RightValue =
    ValueExp Exp   |
    ValueCons CCall
    deriving (Show)

data CCall = CCall Token [Exp] deriving (Show)

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

data Exp = 
    ESum    SumOp     |
    EDif    Dif       |
    EMul    Mul       |
    EDiv    Div       |
    EMod    Mod       |
    EPot    Pot       |
    EDivE   DivE      |
    ELShift LShift   |
    ERShift RShift   |
    EBitOr  BitOr     |
    EBitAnd BitAnd   |
    EBitXor BitXor   |
    EOr     Or        |
    EAnd    And       |
    EGeq    Geq       |
    EGreat  Great     |
    ELeq    Leq       |
    ELess   Less      |
    ENeq    Neq       |
    EEqual  Equal     |
    ENeg    Exp |
    ENot    Exp |
    EBitNot Exp |
    EFCall  FCall     |
    EToken  Token     |
    EList   [Exp] |
    EArr    [Exp] |
    EDict   [(Exp,Exp)] |
    ETup    [Exp] |
    EIdent  Identifier
    deriving (Show)

data SumOp    = SumOp   Exp Exp   deriving (Show)
data Dif    = Dif   Exp Exp   deriving (Show)
data Mul    = Mul   Exp Exp   deriving (Show)
data Div    = Div   Exp Exp   deriving (Show)
data Mod    = Mod   Exp Exp   deriving (Show)
data DivE   = DivE  Exp Exp   deriving (Show)
data Pot    = Pot   Exp Exp   deriving (Show)
data LShift = LShift Exp Exp deriving (Show)
data RShift = RShift Exp Exp deriving (Show)
data BitOr = BitOr Exp Exp deriving (Show)
data BitAnd = BitAnd Exp Exp deriving (Show)
data BitXor = BitXor Exp Exp deriving (Show)
data Or     = Or    Exp Exp   deriving (Show)
data And    = And   Exp Exp   deriving (Show)
data Geq    = Geq   Exp Exp   deriving (Show)
data Great     = Great    Exp Exp   deriving (Show)
data Leq    = Leq   Exp Exp   deriving (Show)
data Less   = Less  Exp Exp   deriving (Show)
data Neq    = Neq   Exp Exp   deriving (Show)
data Equal = Equal Exp Exp deriving (Show)

data FCall = FCall Token [Exp] deriving (Show)
