module Error where

data Error 
    = InvalidType 
    | NotEnoughElements
    | UnknownIdentifier String
    | NonAssignedIdentifier

    deriving Eq