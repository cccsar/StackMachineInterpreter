module BackEnd where

import Error

import Control.Monad (foldM)
import Data.Maybe (fromJust)

import qualified Data.Map as M

type Id      = String
type Label   = String
type Address = Int

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
    deriving Show


data StackContent 
    = Int Int 
    | Bool Bool 
    | Id Id
    deriving Show

type Environment = M.Map Id  StackContent
type Stack = [StackContent]

data StackMachine = StackMachine {
    state       :: Stack,
    ledge       :: [Instruction],
    environment :: Environment,
    labels      :: M.Map Label Int
}

-- API for stack
stackOperations = [add, sub, mul, divS, andS, orS, lt, gt, ge, eq, neq, uminus, notS]

pop, add, sub, mul, divS, andS, orS, lt, le, gt, ge, eq, neq, uminus, notS :: Stack -> Either Error Stack


pop []     = Left EmptyPop
pop (_:xs) = Right xs

add (Int a : Int b : xs) = Right $ Int (a + b) : xs
add (_ : _ : xs )        = Left InvalidOpTypes
add _                    = Left EmptyStack

sub (Int a : Int b : xs) = Right $ Int (a - b) : xs
sub (_ : _ : xs)         = Left InvalidOpTypes
sub _                    = Left EmptyStack

mul (Int a : Int b : xs) = Right $ Int (a * b) : xs
mul (_ : _ : xs)         = Left InvalidOpTypes
mul _                    = Left EmptyStack

divS (Int a : Int b : xs) = Right $ Int (a `div` b) : xs
divS (_ : _ : xs)         = Left InvalidOpTypes
divS _                    = Left EmptyStack

andS (Bool a : Bool b : xs) = Right $ Bool (a && b) : xs
andS (_ : _ : xs)           = Left InvalidOpTypes
andS _                      = Left EmptyStack

orS (Bool a : Bool b : xs) = Right $ Bool (a || b) : xs
orS (_ : _ : xs)           = Left InvalidOpTypes
orS _                      = Left EmptyStack

lt (Int a : Int b : xs) = Right $ Bool (a < b) : xs
lt (_ : _ : xs)         = Left InvalidOpTypes
lt _                    = Left EmptyStack

le (Int a : Int b : xs) = Right $ Bool (a <= b) : xs
le (_ : _ : xs)         = Left InvalidOpTypes
le _                    = Left EmptyStack

gt (Int a : Int b : xs) = Right $ Bool (a > b) : xs
gt (_ : _ : xs)         = Left InvalidOpTypes
gt _                    = Left EmptyStack

ge (Int a : Int b : xs) = Right $ Bool (a >= b) : xs
ge (_ : _ : xs)         = Left InvalidOpTypes
ge _                    = Left EmptyStack

eq (Int a : Int b : xs)   = Right $ Bool (a==b) : xs
eq (Bool a : Bool b : xs) = Right $ Bool (a==b) : xs
eq (_ : _ : xs)           = Left InvalidOpTypes 
eq _                      = Left EmptyStack

neq (Int a : Int b : xs)   = Right $ Bool (a/=b) : xs
neq (Bool a : Bool b : xs) = Right $ Bool (a/=b) : xs
neq (_ : _ : xs)           = Left InvalidOpTypes 
neq _                      = Left EmptyStack

uminus (Int a : xs) = Right $ Int (-a) : xs
uminus []           = Left EmptyStack 
uminus _            = Left InvalidOpTypes

notS (Bool a : xs) = Right $ Bool (not a) : xs
notS []            = Left EmptyStack 
notS _             = Left InvalidOpTypes 


-- Stack Machine Specific's -- 

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
                            | otherwise           -> return $ Left UnassignableElement
                        ( a : _ : _ )         -> return $ Left UnassignableElement
                        _                     -> return $ Left EmptyState
    (Goto label)    -> case M.lookup label (labels stackM) of
                        Just startPoint -> do
                                let instrSize  = length (ledge stackM) - startPoint + 1
                                    instrBlock = reverse $ take startPoint (ledge stackM)
                                a <- foldM performInstruction stackM instrBlock
                        Nothing      -> return $ Left $ LabelNotFound label
    (GoTrue label)  -> case state stackM of
                        (Bool True : _) -> performInstruction stackM (Goto label)
                        (a : _ )        -> return $ Left InvalidTopForJump 
                        []              -> return $ Left EmptyState
    (GoFalse label) ->  case state stackM of
                        (Bool False : _) -> performInstruction stackM (Goto label)
                        (a : _ )         -> return $ Left InvalidTopForJump 
                        []               -> return $ Left EmptyState

isLvalue :: StackContent -> Bool 
isLvalue (Id _) = True 
isLvalue _      = False

-- Constants

initialSM :: StackMachine 
initialSM = StackMachine { 
    state       = [], 
    ledge       = [],
    environment = M.empty,
    labels      = M.empty
}