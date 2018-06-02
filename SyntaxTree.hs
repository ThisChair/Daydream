module SyntaxTree where
import Lexer

data Init = Init Module [Import] [Instruction] deriving (Show)

data Module = Module Token | Main deriving (Show)

data Import = Import Token deriving (Show)

data Instruction = 
    Block [Instruction]                    |
    Assign ([Identifier],[RightValue])     |
    IfThen Exp Instruction                 |
    IfElse Exp Instruction Instruction     |
    While Exp Instruction                  |
    Det For                                |
    Ret Exp                                |
    Continue                               |
    Break                                  |
    Print Exp                              |
    PrintLn Exp
    deriving (Show)

data For = 
    FromTo       Exp Exp Instruction         |
    FromToIf     Exp Exp Exp Instruction     |
    FromToWithIf Exp Exp Exp Exp Instruction |
    FromToWith   Exp Exp Exp Instruction     |
    InIf         Exp Exp Instruction
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
typeString (Name  s) = s
typeString (List  _) = "_list"
typeString (Array _ _) = "_array"
typeString (Tuple _) = "_tuple"
typeString (Dict  _) = "_dict"

data TypeName = 
    Name String              |
    Array TypeName Token     |
    List TypeName            |
    Tuple [TypeName]         |
    Dict (TypeName,TypeName) 
    deriving (Show)

idString :: Identifier -> String
idString (Variable (s,_)) = s
_ = error $ "No variable"

data Identifier = 
    Variable (String,Integer)    |
    Index Identifier Exp         |
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
    EGEq    GEq       |
    EGreat  Great     |
    ELEq    LEq       |
    ELess   Less      |
    ENEq    NEq       |
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
    EIdent  Identifier |
    Read |
    ERef Identifier
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
data GEq    = GEq   Exp Exp   deriving (Show)
data Great     = Great    Exp Exp   deriving (Show)
data LEq    = LEq   Exp Exp   deriving (Show)
data Less   = Less  Exp Exp   deriving (Show)
data NEq    = NEq   Exp Exp   deriving (Show)
data Equal = Equal Exp Exp deriving (Show)

data FCall = FCall Token [Exp] deriving (Show)
