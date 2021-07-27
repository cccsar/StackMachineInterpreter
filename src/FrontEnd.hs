module FrontEnd where

import Constants
import Error

import BackEnd
import Data.Char (isDigit)
import System.IO

import qualified Data.Map as M

parse :: StackMachine -> Int -> [String] -> IO (Either Error StackMachine)
parse stackM n [] = return $ Right stackM 
parse stackM n xs = case xs of 
    ("Push":val:xs)
        | isBool val -> return $ Right $ insertInstruction stackM (Push $ Bool (read val :: Bool) ) 
        | isNum val  -> return $ Right $ insertInstruction stackM (Push $ Int (read val :: Int))
    ("Pop":xs)            -> parse (insertInstruction stackM Pop) (n+1) xs
    ("Add":xs)            -> parse (insertInstruction stackM Add) (n+1) xs
    ("Sub":xs)            -> parse (insertInstruction stackM Sub) (n+1) xs
    ("Mul":xs)            -> parse (insertInstruction stackM Mul) (n+1) xs
    ("Div":xs)            -> parse (insertInstruction stackM Div) (n+1) xs
    ("And":xs)            -> parse (insertInstruction stackM And) (n+1) xs
    ("Or":xs)             -> parse (insertInstruction stackM Or) (n+1) xs
    ("Lt":xs)             -> parse (insertInstruction stackM Lt) (n+1) xs
    ("Le":xs)             -> parse (insertInstruction stackM Le) (n+1) xs
    ("Gt":xs)             -> parse (insertInstruction stackM Gt) (n+1) xs
    ("Ge":xs)             -> parse (insertInstruction stackM Ge) (n+1) xs
    ("Eq":xs)             -> parse (insertInstruction stackM Eq) (n+1) xs
    ("Neq":xs)            -> parse (insertInstruction stackM Neq) (n+1) xs
    ("Uminus":xs)         -> parse (insertInstruction stackM Uminus) (n+1) xs
    ("Not":xs)            -> parse (insertInstruction stackM Not) (n+1) xs
    ("Rvalue":idS:xs)     -> parse (insertInstruction stackM (Rvalue idS)) (n+1) xs
    ("Lvalue":idS:xs)     -> parse (insertInstruction stackM (Lvalue idS)) (n+1) xs
    ("Assign":xs)         -> parse (insertInstruction stackM Assign) (n+1) xs
    ("Goto":label:xs)     -> parse (insertInstruction stackM (Goto label)) (n+1) xs
    ("GoTrue":label:xs)   -> parse (insertInstruction stackM (GoTrue label)) (n+1) xs
    ("GoFalse":label:xs)  -> parse (insertInstruction stackM (GoFalse label)) (n+1) xs
    ("Read":idS:xs)       -> do
        input <- request idRequest
        if isBool input 
            then let newStackM = insertId stackM idS (Bool (read input :: Bool)) 
                 in parse (insertInstruction newStackM _) (n+1) xs
            else if isNum input 
                then let newStackM = insertId stackM idS (Int (read input :: Int)) 
                     in parse (insertInstruction newStackM _) (n+1) xs
                else return $ Left $ InvalidInput idS
    ("Print":idS:xs)      -> case M.lookup idS (environment stackM) of
            Just val -> print val >> parse (insertInstruction stackM _) (n+1) xs
            Nothing  -> return $ Left $ IdNotFound idS
    (maybeLabel:xs)       
        | isLabel maybeLabel -> parse (insertLabel stackM maybeLabel n) (n+1) xs
        | otherwise          -> return $ Left $ InvalidInput maybeLabel
    _                     -> undefined

{- Display -}

prompt :: IO () 
prompt = putStr promptStr

request :: String -> IO String 
request msg = do 
    prompt 
    putStr msg
    hFlush stdout 
    getLine

warning :: String -> IO () 
warning msg = prompt >> putStrLn msg

{- Helpers -}

isBool :: String -> Bool 
isBool el = el `elem` ["true","false"]

isNum :: String -> Bool
isNum = all isDigit

isLabel :: String -> Bool 
isLabel = (==':') . last