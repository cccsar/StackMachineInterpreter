module FrontEnd where

import Constants (startMessage, exitMessage, initialize, askForFile)
import Error ( Error(InvalidInput) )
import Utility ( isBool, isNum, isLabel, request, warning)

import BackEnd
    ( insertLabel,
      insertInstruction,
      StackContent(Int, Bool),
      Instruction(Print, Read, GoFalse, GoTrue, Goto, Assign, Lvalue,
                  Rvalue, Not, Uminus, Neq, Eq, Ge, Gt, Le, Lt, Or, And, Div, Mul,
                  Sub, Add, Pop, Push),
      StackMachine,
      initialSM )


startPoint :: IO () 
startPoint = do 
    putStr initialize
    putStrLn startMessage
    file    <- request askForFile
    content <- readFile file

    result <- parse initialSM 0 (words content)

    case result of
        Left err     -> warning (show err)
        Right stackM -> do putStrLn exitMessage
                           print (show stackM)

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
    ("Read":idS:xs)       -> parse (insertInstruction stackM (Read idS)) (n+1) xs
    ("Print":idS:xs)      -> parse (insertInstruction stackM (Print idS)) (n+1) xs
    (maybeLabel:xs)       
        | isLabel maybeLabel -> parse (insertLabel stackM maybeLabel n) (n+1) xs
        | otherwise          -> return $ Left $ InvalidInput maybeLabel
    _                     -> error "It shouldn't arrive here"