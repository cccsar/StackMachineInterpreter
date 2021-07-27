module BackEnd where

import Constants
import Error
import Utility

import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.List(foldl', intersperse)

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
    show stackM = "*State: " ++ stateShow
                  ++ "\n*Exisiting bindings " ++ environmentShow  
        where
        stateShow = unwords . map show $ state stackM 
        environmentShow = concatMap (\(a,b) -> a ++ " : " ++ show b) . M.toList $ environment stackM

{- API for State Stack -}

stackOperations = [add, sub, mul, divS, andS, orS, lt, gt, ge, eq, neq, uminus, notS]

pop, add, sub, mul, divS, andS, orS, lt, le, gt, ge, eq, neq, uminus, notS :: Stack -> Either Error Stack


pop []     = Left EmptyPop
pop (_:xs) = Right xs

add (Int a : Int b : xs) = Right $ Int (a + b) : xs
add (_ : _ : xs )        = Left $ InvalidBinOpTypes "ADD"
add _                    = Left EmptyStack

sub (Int a : Int b : xs) = Right $ Int (a - b) : xs
sub (_ : _ : xs)         = Left $ InvalidBinOpTypes "SUB"
sub _                    = Left EmptyStack

mul (Int a : Int b : xs) = Right $ Int (a * b) : xs
mul (_ : _ : xs)         = Left $ InvalidBinOpTypes "MUL"
mul _                    = Left EmptyStack

divS (Int a : Int b : xs) = Right $ Int (a `div` b) : xs
divS (_ : _ : xs)         = Left $ InvalidBinOpTypes "DIV"
divS _                    = Left EmptyStack

andS (Bool a : Bool b : xs) = Right $ Bool (a && b) : xs
andS (_ : _ : xs)           = Left $ InvalidBinOpTypes "AND"
andS _                      = Left EmptyStack

orS (Bool a : Bool b : xs) = Right $ Bool (a || b) : xs
orS (_ : _ : xs)           = Left $ InvalidBinOpTypes "OR"
orS _                      = Left EmptyStack

lt (Int a : Int b : xs) = Right $ Bool (a < b) : xs
lt (_ : _ : xs)         = Left $ InvalidBinOpTypes "LT"
lt _                    = Left EmptyStack

le (Int a : Int b : xs) = Right $ Bool (a <= b) : xs
le (_ : _ : xs)         = Left $ InvalidBinOpTypes "LE"
le _                    = Left EmptyStack

gt (Int a : Int b : xs) = Right $ Bool (a > b) : xs
gt (_ : _ : xs)         = Left $ InvalidBinOpTypes "GT"
gt _                    = Left EmptyStack

ge (Int a : Int b : xs) = Right $ Bool (a >= b) : xs
ge (_ : _ : xs)         = Left $ InvalidBinOpTypes "GE"
ge _                    = Left EmptyStack

eq (Int a : Int b : xs)   = Right $ Bool (a==b) : xs
eq (Bool a : Bool b : xs) = Right $ Bool (a==b) : xs
eq (_ : _ : xs)           = Left $ InvalidBinOpTypes "EQ"
eq _                      = Left EmptyStack

neq (Int a : Int b : xs)   = Right $ Bool (a/=b) : xs
neq (Bool a : Bool b : xs) = Right $ Bool (a/=b) : xs
neq (_ : _ : xs)           = Left $ InvalidBinOpTypes "NEQ"
neq _                      = Left EmptyStack

uminus (Int a : xs) = Right $ Int (-a) : xs
uminus []           = Left EmptyStack 
uminus _            = Left $ InvalidUnOpTypes "UMINUS"

notS (Bool a : xs) = Right $ Bool (not a) : xs
notS []            = Left EmptyStack 
notS _             = Left $ InvalidUnOpTypes "NOT"

{- Stack Machine Specific's -}

pushSM :: StackMachine -> StackContent -> StackMachine
pushSM stackM el = stackM { state = el : state stackM }

insertInstruction :: StackMachine -> Instruction -> StackMachine
insertInstruction stackM instruction = stackM { ledge = instruction : ledge stackM }

insertLabel :: StackMachine -> Label -> Int -> StackMachine
insertLabel stackM label ind = stackM { labels = M.insert label ind (labels stackM) } 

insertId :: StackMachine -> Id -> StackContent -> StackMachine
insertId stackM idS val = stackM { environment = M.insert idS val (environment stackM ) }

performOperation :: StackMachine -> (Stack -> Either Error Stack) -> Either Error StackMachine
performOperation stackM op = do
    newState <- op $ state stackM
    return $ stackM { state = newState }

performInstruction :: StackMachine -> Instruction -> IO (Either Error StackMachine)
performInstruction stackM instruction = case instruction of
    Push el         -> return $ Right $ pushSM stackM el
    Pop             -> return $ performOperation stackM pop
    Add             -> return $ performOperation stackM add
    Sub             -> return $ performOperation stackM sub
    Mul             -> return $ performOperation stackM mul
    Div             -> return $ performOperation stackM divS
    And             -> return $ performOperation stackM andS
    Or              -> return $ performOperation stackM orS
    Lt              -> return $ performOperation stackM lt
    Le              -> return $ performOperation stackM le
    Gt              -> return $ performOperation stackM gt
    Ge              -> return $ performOperation stackM ge
    Eq              -> return $ performOperation stackM eq
    Neq             -> return $ performOperation stackM neq
    Uminus          -> return $ performOperation stackM uminus
    Not             -> return $ performOperation stackM notS
    (Rvalue sId)    -> case M.lookup sId (environment stackM) of
                        Just val     -> return $ Right $ pushSM stackM val
                        Nothing      -> return $ Left $ IdNotFound sId
    (Lvalue sId)    -> return $ Right $ pushSM stackM (Id sId)
    Assign          -> case state stackM of
                        (Id name : cont : xs) 
                            | not $ isLvalue cont -> let newEnv = M.insert name cont (environment stackM) 
                                                     in return $ Right $ stackM { environment = newEnv, state = xs } 
                            | otherwise           -> return $ Left $ InvalidRValue (show cont)
                        ( a : _ : _ )         -> return $ Left $ InvalidLValue (show a)
                        _                     -> return $ Left EmptyState
    (Goto label)    -> case M.lookup label (labels stackM) of
                        Just startPoint -> do
                                let instrSize  = length (ledge stackM) - startPoint + 1
                                    instrBlock = reverse $ take startPoint (ledge stackM)
                                foldWithInstructions stackM instrBlock
                        Nothing      -> return $ Left $ LabelNotFound label
    (GoTrue label)  -> case state stackM of
                        (Bool True : _) -> performInstruction stackM (Goto label)
                        (a : _ )        -> return $ Left $ InvalidTopForJump True (show a)
                        []              -> return $ Left EmptyState
    (GoFalse label) ->  case state stackM of
                        (Bool False : _) -> performInstruction stackM (Goto label)
                        (a : _ )         -> return $ Left $ InvalidTopForJump True (show a)
                        []               -> return $ Left EmptyState
    (Read idS)      -> do
        input <- request idRequest
        if isBool input 
            then return $ Right $ insertId stackM idS (Bool (read input :: Bool)) 
            else if isNum input 
                then return $ Right $ insertId stackM idS (Int (read input :: Int)) 
                else return $ Left $ InvalidInput idS
    (Print idS)     -> do
        case M.lookup idS (environment stackM) of
            Just val -> print val >> return (Right stackM)
            Nothing  -> return $ Left $ IdNotFound idS

foldWithInstructions :: StackMachine -> [Instruction] -> IO (Either Error StackMachine)
foldWithInstructions stackM = foldl' foo (return $ Right stackM)
    where
        foo acc b = do
            extracted <- acc 
            case extracted of 
                Left err        -> return $ Left err 
                Right newStackM -> performInstruction newStackM b

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