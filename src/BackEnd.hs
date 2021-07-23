module BackEnd where

import Error

import Data.Maybe (fromJust)
import qualified Data.Map as M

type Id = String
type Instruction = String
type Address = Int

data StackContent 
    = Int Int 
    | Bool Bool 
    | Default 
    deriving Eq

type Environment = M.Map Id (Address, StackContent)
type Stack = [StackContent]


data StackMachine = StackMachine {
    stack :: Stack,
    environment :: Environment,
    tags :: M.Map String Instruction,
    nextAddress :: Int
}

-- checkers

isBool :: StackContent -> Bool 
isBool (Bool _) = True 
isBool _        = False

-- API for stack

pop :: Stack -> Stack
pop []     = [] 
pop (_:xs) = xs

-- Stack Machine Stack's specific functions 

add, sub, mul, divS, and, or, lt, le, gt, ge, eq, neq, uminus, notS :: Stack -> Either Error Stack

add (Int a : Int b : xs) = Right $ Int (a + b) : xs
add (_ : _ : xs )        = Left InvalidType
add _                    = Left NotEnoughElements

sub (Int a : Int b : xs) = Right $ Int (a - b) : xs
sub (_ : _ : xs)         = Left InvalidType
sub _                    = Left NotEnoughElements

mul (Int a : Int b : xs) = Right $ Int (a * b) : xs
mul (_ : _ : xs)         = Left InvalidType
mul _                    = Left NotEnoughElements

divS (Int a : Int b : xs) = Right $ Int (a `div` b) : xs
divS (_ : _ : xs)         = Left InvalidType
divS _                    = Left NotEnoughElements

and (Bool a : Bool b : xs) = Right $ Bool (a && b) : xs
and (_ : _ : xs)           = Left InvalidType
and _                      = Left NotEnoughElements

or (Bool a : Bool b : xs) = Right $ Bool (a || b) : xs
or (_ : _ : xs)           = Left InvalidType
or _                      = Left NotEnoughElements

lt (Int a : Int b : xs) = Right $ Bool (a < b) : xs
lt (_ : _ : xs)         = Left InvalidType
lt _                    = Left NotEnoughElements

le (Int a : Int b : xs) = Right $ Bool (a <= b) : xs
le (_ : _ : xs)         = Left InvalidType
le _                    = Left NotEnoughElements

gt (Int a : Int b : xs) = Right $ Bool (a > b) : xs
gt (_ : _ : xs)         = Left InvalidType
gt _                    = Left NotEnoughElements

ge (Int a : Int b : xs) = Right $ Bool (a >= b) : xs
ge (_ : _ : xs)         = Left InvalidType
ge _                    = Left NotEnoughElements

eq (Int a : Int b : xs)   = Right $ Bool (a==b) : xs
eq (Bool a : Bool b : xs) = Right $ Bool (a==b) : xs
eq (_ : _ : xs)           = Left InvalidType 
eq _                      = Left NotEnoughElements

neq (Int a : Int b : xs)   = Right $ Bool (a/=b) : xs
neq (Bool a : Bool b : xs) = Right $ Bool (a/=b) : xs
neq (_ : _ : xs)           = Left InvalidType 
neq _                      = Left NotEnoughElements

uminus (Int a : xs) = Right $ Int (-a) : xs
uminus []           = Left NotEnoughElements 
uminus _            = Left InvalidType

notS (Bool a : xs) = Right $ Bool (not a) : xs
notS []            = Left NotEnoughElements 
notS _             = Left InvalidType 

-- Stack Machine Specific functions

pushSM :: StackMachine -> StackContent -> StackMachine
pushSM stackM el = stackM { stack = el : stack stackM }

popSM :: StackMachine -> StackMachine
popSM stackM = stackM { stack = pop $ stack stackM}

increaseNextAddr :: StackMachine -> StackMachine 
increaseNextAddr stackM = stackM { nextAddress = succ $ nextAddress stackM}

insertId :: StackMachine -> Id -> StackContent -> StackMachine
insertId stackM id val = (increaseNextAddr stackM) { environment = newEnv }
    where newEnv = M.insert id (nextAddress stackM, val) $ environment stackM

someStackOp :: StackMachine -> (Stack -> Either Error Stack) -> Either Error StackMachine
someStackOp stackM op = do
    newStack <- op (stack stackM)
    return (stackM { stack = newStack} )


rvalue:: Id -> StackMachine  -> Either Error StackMachine
rvalue id stackM = case res of
                Nothing     -> Left $ UnknownIdentifier id
                Just (_,val) -> case val of
                            Default -> Left NonAssignedIdentifier 
                            _       ->  Right $ stackM { stack = val : stack stackM }
    where res = M.lookup id (environment stackM)

lvalue :: Id -> StackMachine -> StackMachine
lvalue id stackM = case M.lookup id (environment stackM) of
    Nothing -> let preSM = pushSM stackM (Int $ nextAddress stackM)
                   newSM = insertId preSM id Default 
               in increaseNextAddr newSM 
    Just (addr,_) -> pushSM stackM (Int addr)

-- TODO ver como resolver lo de "Asigna en la direccion representada por el elemento que estaba en el tope"
assign :: StackMachine -> StackMachine 
assign = undefined

goto, gotrue, gofalse :: StackMachine -> String -> Either Error StackMachine
goto = undefined

gotrue = undefined 

gofalse = undefined

-- Constants

initialSM :: StackMachine 
initialSM = StackMachine { 
    stack = [],
    environment = M.empty,
    tags = M.empty,
    nextAddress = 0
}