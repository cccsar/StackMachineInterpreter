module Error where

data Error 
    = EmptyPop 
    | InvalidOpTypes 
    | EmptyStack

    | IdNotFound String

    | UnassignableElement
    | EmptyState

    | LabelNotFound String

    | InvalidTopForJump 

    | InvalidInput String
    deriving Eq