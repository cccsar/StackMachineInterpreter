module Utility where

import Constants (promptStr)
import Data.Char (isDigit, toUpper)

import System.IO ( stdout, hFlush ) 

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

toHaskellBool :: String -> String 
toHaskellBool []     = error "This shouldn't happen" 
toHaskellBool (x:xs) = toUpper x : xs 

isBool :: String -> Bool 
isBool el = el `elem` ["true","false"]

isNum :: String -> Bool
isNum = all isDigit

isLabel :: String -> Bool 
isLabel = (==':') . last