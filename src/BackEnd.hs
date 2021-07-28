module BackEnd where

import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.List(foldl', intercalate)
import System.Exit (exitSuccess)

import Constants ( exitMessage, idRequest )
import Error
    ( Error(InvalidInput, InvalidTopForJump, LabelNotFound,
            IncompleteState, InvalidLValue, InvalidRValue, IdNotFound,
            InvalidUnOpTypes, InsufficientArguments, InvalidBinOpTypes,
            EmptyPop) )
import Utility ( request, isNum, isBool, toHaskellBool, warning)



import qualified Data.Map as M

type Id      = String
type Label   = String

data Instruction 
    = Push StackContent
    | Pop
    | Add 
    | Sub 
    | Mul 
    | Div 
    | And 
    | Or
    | Lt 
    | Le 
    | Gt 
    | Ge 
    | Eq
    | Neq
    | Uminus
    | Not
    | Rvalue Id
    | Lvalue Id
    | Assign 

    | Goto Label
    | GoTrue Label
    | GoFalse Label 

    | Read String 
    | Print String

    | Exit
    deriving Show

data StackContent 
    = Int Int 
    | Bool Bool 
    | Id Id

instance Show StackContent where
    show (Int e)  = show e
    show (Bool e) = show e
    show (Id e)   = show e

type Environment = M.Map Id  StackContent
type Stack = [StackContent]

data StackMachine = StackMachine {
    state       :: Stack,
    ledge       :: [Instruction],
    environment :: Environment,
    labels      :: M.Map Label Int
}

instance Show StackMachine where
    show stackM = "*State: " ++ stateShow (state stackM)
                  ++ "\n*Bindings: " ++ environmentShow (M.toList $ environment stackM)
        where
        stateShow [] = "empty stack"
        stateShow xs = unwords . map show $ xs

        environmentShow [] = "empty environment"
        environmentShow xs = intercalate "," . map (\(a,b) -> ' ' : a ++ " : " ++ show b) $ xs

{- API for State Stack -}

stackOperations = [add, sub, mul, divS, andS, orS, lt, gt, ge, eq, neq, uminus, notS]

pop, add, sub, mul, divS, andS, orS, lt, le, gt, ge, eq, neq, uminus, notS :: Stack -> IO Stack


pop []     = reportError EmptyPop >> return []
pop (_:xs) = return xs

add (Int a : Int b : xs) = return $ Int (a + b) : xs
add a@(_ : _ : xs )      = reportError (InvalidBinOpTypes "ADD") >> return a
add xs                   = reportError (InsufficientArguments "ADD" 2 $ length xs) >> return xs

sub (Int b : Int a : xs) = return $ Int (a - b) : xs
sub a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "SUB") >> return xs
sub xs                   = reportError (InsufficientArguments "SUB" 2 $ length xs) >> return xs

mul (Int a : Int b : xs) = return $ Int (a * b) : xs
mul a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "MUL") >> return a
mul xs                   = reportError (InsufficientArguments "MUL" 2 $ length xs) >> return xs

divS (Int b : Int a : xs) = return $ Int (a `div` b) : xs
divS a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "DIV") >> return a
divS xs                   = reportError (InsufficientArguments "DIV" 2 $ length xs) >> return xs

andS (Bool a : Bool b : xs) = return $ Bool (a && b) : xs
andS a@(_ : _ : xs)         = reportError (InvalidBinOpTypes "AND") >> return a
andS xs                     = reportError (InsufficientArguments "AND" 2 $ length xs) >> return xs 

orS (Bool a : Bool b : xs) = return $ Bool (a || b) : xs
orS a@(_ : _ : xs)         = reportError (InvalidBinOpTypes "OR") >> return a
orS xs                     = reportError (InsufficientArguments "OR" 2 $ length xs) >> return xs 

lt (Int b : Int a : xs) = return $ Bool (a < b) : xs
lt a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "LT") >> return a
lt xs                   = reportError (InsufficientArguments "LT" 2 $ length xs) >> return xs 

le (Int b : Int a : xs) = return $ Bool (a <= b) : xs
le a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "LE") >> return a
le xs                   = reportError (InsufficientArguments "LE" 2 $ length xs) >> return xs 

gt (Int b : Int a : xs) = return $ Bool (a > b) : xs
gt a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "GT") >> return a
gt xs                   = reportError (InsufficientArguments "GT" 2 $ length xs) >> return xs 

ge (Int b : Int a : xs) = return $ Bool (a >= b) : xs
ge a@(_ : _ : xs)       = reportError (InvalidBinOpTypes "GE") >> return a
ge xs                   = reportError (InsufficientArguments "GE" 2 $ length xs) >> return xs 

eq (Int a : Int b : xs)   = return $ Bool (a==b) : xs
eq (Bool a : Bool b : xs) = return $ Bool (a==b) : xs
eq a@(_ : _ : xs)         = reportError (InvalidBinOpTypes "EQ") >> return a 
eq xs                     = reportError (InsufficientArguments "EQ" 2 $ length xs) >> return xs  

neq (Int a : Int b : xs)   = return $ Bool (a/=b) : xs
neq (Bool a : Bool b : xs) = return $ Bool (a/=b) : xs
neq a@(_ : _ : xs)         = reportError (InvalidBinOpTypes "NEQ") >> return a
neq xs                     = reportError (InsufficientArguments "NEQ" 2 $ length xs) >> return xs  

uminus (Int a : xs) = return $ Int (-a) : xs
uminus []           = reportError (InsufficientArguments "UMINUS" 1 0) >> return []
uminus xs           = reportError (InvalidUnOpTypes "UMINUS") >> return xs

notS (Bool a : xs) = return $ Bool (not a) : xs
notS []            = reportError (InsufficientArguments "NOT" 1 0) >> return [] 
notS xs            = reportError (InvalidUnOpTypes "NOT") >> return xs

{- Stack Machine Specific's -}

pushSM :: StackMachine -> StackContent -> StackMachine
pushSM stackM el = stackM { state = el : state stackM }

popSM :: StackMachine -> StackMachine
popSM stackM = stackM { state = tail $ state stackM}

insertInstruction :: StackMachine -> Instruction -> StackMachine
insertInstruction stackM instruction = stackM { ledge = instruction : ledge stackM }

insertLabel :: StackMachine -> Label -> Int -> StackMachine
insertLabel stackM label ind = stackM { labels = M.insert label ind (labels stackM) } 

insertId :: StackMachine -> Id -> StackContent -> StackMachine
insertId stackM idS val = stackM { environment = M.insert idS val (environment stackM) }

performOperation :: StackMachine -> (Stack -> IO Stack) -> IO StackMachine
performOperation stackM op = do
    newState <- op $ state stackM
    return $ stackM { state = newState }

performInstruction :: StackMachine -> Instruction -> IO StackMachine
performInstruction stackM instruction = do 
    displayCurrentState instruction stackM
    case instruction of
     Push val        -> return $ pushSM stackM val
     Pop             -> performOperation stackM pop
     Add             -> performOperation stackM add
     Sub             -> performOperation stackM sub
     Mul             -> performOperation stackM mul
     Div             -> performOperation stackM divS
     And             -> performOperation stackM andS
     Or              -> performOperation stackM orS
     Lt              -> performOperation stackM lt
     Le              -> performOperation stackM le
     Gt              -> performOperation stackM gt
     Ge              -> performOperation stackM ge
     Eq              -> performOperation stackM eq
     Neq             -> performOperation stackM neq
     Uminus          -> performOperation stackM uminus
     Not             -> performOperation stackM notS
     (Rvalue sId)    -> case M.lookup sId (environment stackM) of
                         Just val     -> return $ pushSM stackM val
                         Nothing      -> reportError (IdNotFound sId) >> return stackM
     (Lvalue sId)    -> return $ pushSM stackM (Id sId)
     Assign          -> case state stackM of
                         (cont : Id name : xs) 
                             | not $ isLvalue cont -> let newEnv = M.insert name cont (environment stackM) 
                                                      in return $ stackM { environment = newEnv, state = xs } 
                             | otherwise           -> reportError (InvalidRValue (show cont)) >> return stackM
                         ( a : _ : _ )         -> reportError (InvalidLValue (show a)) >> return stackM
                         _                     -> reportError IncompleteState >> return stackM
     (Goto label)    -> case M.lookup label (labels stackM) of
                         Just startPoint -> do
                                 let instrSize  = length (ledge stackM) - startPoint + 1
                                     instrBlock = reverse $ take instrSize (ledge stackM)
                                 -- foldWithInstructions stackM instrBlock
                                 foldM performInstruction stackM instrBlock
                         Nothing        -> reportError (LabelNotFound label) >> return stackM
     (GoTrue label)  -> case state stackM of
                         (Bool b : _) -> let newStackM = popSM stackM 
                                         in if b 
                                             then performInstruction newStackM (Goto label)
                                             else return $ newStackM
                         (a : _ )     -> reportError(InvalidTopForJump True (show a)) >> return stackM
                         []           -> reportError IncompleteState >> return stackM
     (GoFalse label) ->  case state stackM of
                         (Bool b: _) -> let newStackM = popSM stackM 
                                        in if not b
                                             then performInstruction newStackM (Goto label)
                                             else return newStackM
                         (a : _ )    -> reportError (InvalidTopForJump True (show a)) >> return stackM
                         []          -> reportError IncompleteState >> return stackM
     (Read idS)      -> do
         input <- request idRequest
         if isBool input 
             then let arg = read (toHaskellBool input) :: Bool 
                  in return $ insertId stackM idS (Bool arg) 
             else if isNum input 
                 then let arg = read input :: Int
                      in return $ insertId stackM idS (Int arg) 
                 else reportError (InvalidInput idS) >> return stackM
     (Print idS)     -> do
         case M.lookup idS (environment stackM) of
             Just val -> print val >> return stackM
             Nothing  -> reportError (IdNotFound idS) >> return stackM
     Exit            -> do
                         putStrLn exitMessage
                         putStrLn (show stackM)
                         exitSuccess


displayCurrentState :: Instruction -> StackMachine -> IO ()
displayCurrentState instruction stackM = putStrLn goodDisplay
    where 
        goodDisplay = arrow ++ show instruction ++ alignColumn ++ "stack: "++ stackState
        arrow = "--> " 
        alignColumn = "\ESC[25G" 
        stackState = show $ state stackM

reportError :: Error -> IO ()
reportError err = warning (show err)

{- Helpers -}

isLvalue :: StackContent -> Bool 
isLvalue (Id _) = True 
isLvalue _      = False

{- Constants -}

initialSM :: StackMachine 
initialSM = StackMachine { 
    state       = [], 
    ledge       = [],
    environment = M.empty,
    labels      = M.empty
}