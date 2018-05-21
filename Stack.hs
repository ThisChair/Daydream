module Stack where

type Stack = [StackEntry]

data StackEntry = StackEntry { scope :: Integer
                             , closed :: Bool
                             , other :: String } deriving (Show, Eq)

stack = []
s0 = StackEntry 0 False "nothing"
s1 = StackEntry 1 False "nothing"
s2 = StackEntry 2 False "nothing"
s3 = StackEntry 3 False "nothing"

pop :: Stack -> Maybe Stack
pop [] = Nothing
pop (x:xs) = Just xs

push :: StackEntry -> Stack -> Stack
push x xs = x:xs  