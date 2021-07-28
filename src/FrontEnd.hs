module FrontEnd where

import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import Control.Monad (foldM)

import BackEnd
    ( insertLabel,
      insertInstruction,
      StackContent(Int, Bool),
      Instruction(Print, Read, GoFalse, GoTrue, Goto, Assign, Lvalue,
                  Rvalue, Not, Uminus, Neq, Eq, Ge, Gt, Le, Lt, Or, And, Div, Mul,
                  Sub, Add, Pop, Push, Exit),
      StackMachine(ledge,labels),
      performInstruction,
      reportError,
      initialSM)
import Constants (startMessage, exitMessage, initialize, askForFile, fileDoesNotExists, errorHead)
import Error ( Error(InvalidInput, InvalidStackValue) )
import Utility ( isBool, isNum, isLabel, request, warning, toHaskellBool)

startPoint :: IO () 
startPoint = do 
    putStr initialize
    putStrLn startMessage
    loop

loop :: IO () 
loop = do 
    file  <- request askForFile
    check <- doesFileExist file

    if check 
        then do 
            content        <- readFile file
            initialStackM  <- parse initialSM 1 (words content)
            finalStackM    <- foldM performInstruction initialStackM (reverse $ ledge initialStackM)

            putStrLn exitMessage
            putStrLn (show finalStackM)
            exitSuccess

        else putStrLn (fileDoesNotExists file) >> loop

parse :: StackMachine -> Int -> [String] -> IO StackMachine
parse stackM n [] = return stackM 
parse stackM n xs = case xs of 
    ("PUSH":val:xs)
        | isBool val -> let arg = read (toHaskellBool val) :: Bool 
                        in parse (insertInstruction stackM (Push $ Bool arg )) (n+1) xs
        | isNum val  -> let arg = read val :: Int 
                        in parse (insertInstruction stackM (Push $ Int arg ) ) (n+1) xs
        | otherwise  -> putStrLn "Marico3" >> reportError (InvalidStackValue val) >> parse stackM n (val:xs)
    ("POP":xs)            -> parse (insertInstruction stackM Pop) (n+1) xs
    ("ADD":xs)            -> parse (insertInstruction stackM Add) (n+1) xs
    ("SUB":xs)            -> parse (insertInstruction stackM Sub) (n+1) xs
    ("MUL":xs)            -> parse (insertInstruction stackM Mul) (n+1) xs
    ("DIV":xs)            -> parse (insertInstruction stackM Div) (n+1) xs
    ("AND":xs)            -> parse (insertInstruction stackM And) (n+1) xs
    ("OR":xs)             -> parse (insertInstruction stackM Or) (n+1) xs
    ("LT":xs)             -> parse (insertInstruction stackM Lt) (n+1) xs
    ("LE":xs)             -> parse (insertInstruction stackM Le) (n+1) xs
    ("GT":xs)             -> parse (insertInstruction stackM Gt) (n+1) xs
    ("GE":xs)             -> parse (insertInstruction stackM Ge) (n+1) xs
    ("EQ":xs)             -> parse (insertInstruction stackM Eq) (n+1) xs
    ("NEQ":xs)            -> parse (insertInstruction stackM Neq) (n+1) xs
    ("UMINUS":xs)         -> parse (insertInstruction stackM Uminus) (n+1) xs
    ("NOT":xs)            -> parse (insertInstruction stackM Not) (n+1) xs
    ("RVALUE":idS:xs)     -> parse (insertInstruction stackM (Rvalue idS)) (n+1) xs
    ("LVALUE":idS:xs)     -> parse (insertInstruction stackM (Lvalue idS)) (n+1) xs
    ("ASSIGN":xs)         -> parse (insertInstruction stackM Assign) (n+1) xs
    ("GOTO":label:xs)     -> parse (insertInstruction stackM (Goto label)) (n+1) xs
    ("GOTRUE":label:xs)   -> parse (insertInstruction stackM (GoTrue label)) (n+1) xs
    ("GOFALSE":label:xs)  -> parse (insertInstruction stackM (GoFalse label)) (n+1) xs
    ("READ":idS:xs)       -> parse (insertInstruction stackM (Read idS)) (n+1) xs
    ("PRINT":idS:xs)      -> parse (insertInstruction stackM (Print idS)) (n+1) xs
    ("EXIT":xs)           -> parse (insertInstruction stackM Exit) (n+1) xs
    (maybeLabel:xs)       
        | isLabel maybeLabel -> parse (insertLabel stackM (init maybeLabel) n) n xs
        | otherwise          -> do
            reportError (InvalidInput maybeLabel) 
            parse stackM n xs
    _                     -> error "It shouldn't arrive here"