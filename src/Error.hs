module Error where

data Error 
    = EmptyPop 
    | InvalidBinOpTypes String
    | InvalidUnOpTypes String
    | EmptyStack

    | IdNotFound String

    | InvalidLValue String
    | InvalidRValue String 
    | EmptyState

    | LabelNotFound String

    | InvalidTopForJump Bool String

    | InvalidInput String

    deriving Eq

instance Show Error where
    show EmptyPop = "Tried to pop from empty stack."
    show (InvalidBinOpTypes op) = "Invalid types for " ++ op ++ " operation at the top of the stack."
    show (InvalidUnOpTypes op) = "Invalid type for " ++ op ++ " operation at the top of the stack."
    show EmptyStack = "Performed an operation over an empty stack."
    show (IdNotFound idS) = "Couldn't find Id " ++ idS ++ "."
    show (InvalidLValue val) = val ++ " is not a valid Lvalue."
    show (InvalidRValue val) = val ++ " is not a valid Rvalue"
    show EmptyState          = "Tried to operate over an empty stack."
    show (LabelNotFound idS) = "Couldn't find Label " ++ idS ++ "."
    show (InvalidTopForJump good el) = "Expected " ++ show good ++ " but found " ++ el
    show (InvalidInput inp) = inp ++ " is not a known word in the language"