module Error where

data Error 
    = EmptyPop 
    | InsufficientArguments String Int Int
    | InvalidBinOpTypes String
    | InvalidUnOpTypes String

    | IdNotFound String

    | InvalidLValue String
    | InvalidRValue String 
    | IncompleteState

    | LabelNotFound String

    | InvalidTopForJump Bool String

    | InvalidStackValue String
    | InvalidInput String

instance Show Error where
    show EmptyPop                     = "Tried to pop from empty stack."
    show (InsufficientArguments op n m) = op ++ " requires " ++ show n ++ " arguments, but " ++ show m
                                            ++ " were given" 
    show (InvalidBinOpTypes op)       = "Invalid types for " ++ op ++ " operation."
    show (InvalidUnOpTypes op)        = "Invalid type for " ++ op ++ " operation."
    show (IdNotFound idS)             = "Couldn't find Id " ++ idS ++ "."
    show (InvalidLValue val)          = val ++ " is not a valid Lvalue."
    show (InvalidRValue val)          = val ++ " is not a valid Rvalue"
    show IncompleteState              = "Tried to assign with fewer than 2 elements available."
    show (LabelNotFound idS)          = "Couldn't find Label " ++ idS ++ "."
    show (InvalidTopForJump good el)  = "Expected " ++ show good ++ " but found " ++ el
    show (InvalidStackValue val)      = val ++ " is not true, false or a number."
    show (InvalidInput inp)           = inp ++ " is not a known word in the language"